%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.

%% Test suite for the MQTT username message interceptor plugin.
-module(mqtt_username_interceptor_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

all() ->
    [{group, tests}].

groups() ->
    [{tests, [shuffle],
      [mqtt_v5_publish_adds_username_annotation,
       mqtt_v311_publish_adds_username_annotation,
       amqp_publish_does_not_add_annotation]}].

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    rabbit_ct_helpers:run_setup_steps(Config).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_group(_Group, Config0) ->
    Config1 = rabbit_ct_helpers:set_config(
                Config0,
                [{rmq_nodes_count, 1},
                 {mqtt_version, v5},
                 {start_rmq_with_plugins_disabled, true}]),
    Config = rabbit_ct_helpers:run_steps(
               Config1,
               rabbit_ct_broker_helpers:setup_steps() ++
               rabbit_ct_client_helpers:setup_steps()),
    ok = rabbit_ct_broker_helpers:enable_plugin(Config, 0, rabbitmq_mqtt),
    ok = rabbit_ct_broker_helpers:enable_plugin(Config, 0, rabbitmq_mqtt_username_interceptor),
    Config.

end_per_group(_Group, Config) ->
    rabbit_ct_helpers:run_steps(
      Config,
      rabbit_ct_client_helpers:teardown_steps() ++
      rabbit_ct_broker_helpers:teardown_steps()).

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    timer:sleep(500),
    rabbit_ct_broker_helpers:rpc(Config, ?MODULE, delete_queues, []),
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% Test that publishing via MQTT 5.0 adds the username annotation.
mqtt_v5_publish_adds_username_annotation(Config) ->
    mqtt_publish_adds_username_annotation(Config, v5).

%% Test that publishing via MQTT 3.1.1 adds the username annotation.
mqtt_v311_publish_adds_username_annotation(Config) ->
    mqtt_publish_adds_username_annotation(Config, v4).

mqtt_publish_adds_username_annotation(Config, ProtoVer) ->
    Username = <<"guest">>,
    Password = <<"guest">>,
    ClientId = atom_to_binary(?FUNCTION_NAME),
    Topic = <<"test/topic">>,
    Payload = <<"test payload">>,
    Q = <<"mqtt-username-test-queue">>,

    Ch = rabbit_ct_client_helpers:open_channel(Config),
    #'queue.declare_ok'{} = amqp_channel:call(
                              Ch,
                              #'queue.declare'{queue = Q, durable = true}),
    #'queue.bind_ok'{} = amqp_channel:call(
                           Ch,
                           #'queue.bind'{queue = Q,
                                         exchange = <<"amq.topic">>,
                                         routing_key = <<"test.topic">>}),

    Port = rabbit_ct_broker_helpers:get_node_config(Config, 0, tcp_port_mqtt),
    {ok, MqttClient} = emqtt:start_link([{host, "localhost"},
                                          {port, Port},
                                          {proto_ver, ProtoVer},
                                          {clientid, ClientId},
                                          {username, Username},
                                          {password, Password}]),
    {ok, _} = emqtt:connect(MqttClient),
    {ok, _} = emqtt:publish(MqttClient, Topic, Payload, [{qos, 1}]),
    ok = emqtt:disconnect(MqttClient),

    {#'basic.get_ok'{}, #amqp_msg{props = Props}} =
        amqp_channel:call(Ch, #'basic.get'{queue = Q, no_ack = true}),

    Headers = Props#'P_basic'.headers,
    ct:pal("Received message headers: ~p", [Headers]),

    ?assertMatch({<<"x-opt-mqtt-username">>, longstr, <<"guest">>},
                 lists:keyfind(<<"x-opt-mqtt-username">>, 1, Headers)),

    ok = rabbit_ct_client_helpers:close_channel(Ch).

%% Test that publishing via AMQP does not add the annotation.
amqp_publish_does_not_add_annotation(Config) ->
    Q = <<"amqp-test-queue">>,
    Payload = <<"test payload">>,

    Ch = rabbit_ct_client_helpers:open_channel(Config),
    #'queue.declare_ok'{} = amqp_channel:call(
                              Ch,
                              #'queue.declare'{queue = Q, durable = true}),

    amqp_channel:call(Ch,
                      #'basic.publish'{routing_key = Q},
                      #amqp_msg{payload = Payload}),

    {#'basic.get_ok'{}, #amqp_msg{props = Props}} =
        amqp_channel:call(Ch, #'basic.get'{queue = Q, no_ack = true}),

    Headers = Props#'P_basic'.headers,
    ct:pal("Received message headers: ~p", [Headers]),

    case Headers of
        undefined ->
            ok;
        _ ->
            ?assertEqual(false, lists:keyfind(<<"x-opt-mqtt-username">>, 1, Headers))
    end,

    ok = rabbit_ct_client_helpers:close_channel(Ch).

delete_queues() ->
    [{ok, _} = rabbit_amqqueue:delete(Q, false, false, <<"ct">>) ||
     Q <- rabbit_amqqueue:list()],
    ok.
