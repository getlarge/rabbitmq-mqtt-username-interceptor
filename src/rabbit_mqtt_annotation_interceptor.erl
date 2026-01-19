%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.

%% @doc Message interceptor that adds MQTT connection metadata as message annotations.
%%
%% This interceptor adds annotations to messages published via MQTT based on
%% configuration. By default it adds:
%% - `x-opt-mqtt-username': the username used to authenticate the MQTT connection
%%
%% Optional annotations (configurable via application env):
%% - `x-opt-mqtt-vhost': the vhost the MQTT connection is connected to
%% - `x-opt-mqtt-connection-name': the connection name
%%
%% Configuration (in rabbitmq.conf):
%% - mqtt_annotation_interceptor.username = true|false (default: true)
%% - mqtt_annotation_interceptor.vhost = true|false (default: false)
%% - mqtt_annotation_interceptor.connection_name = true|false (default: false)
%%
%% Annotations are only added for MQTT protocols (3.1, 3.1.1, 5.0).
%% Messages from other protocols pass through unchanged.
-module(rabbit_mqtt_annotation_interceptor).

-behaviour(rabbit_msg_interceptor).

-export([intercept/4]).

-rabbit_boot_step({?MODULE,
                   [{description, "MQTT connection metadata message interceptor"},
                    {mfa, {rabbit_msg_interceptor, add, [[{?MODULE, #{}}]]}},
                    {cleanup, {rabbit_msg_interceptor, remove, [[{?MODULE, #{}}]]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

-define(KEY_USERNAME, <<"x-opt-mqtt-username">>).
-define(KEY_VHOST, <<"x-opt-mqtt-vhost">>).
-define(KEY_CONN_NAME, <<"x-opt-mqtt-connection-name">>).

-spec intercept(mc:state(), rabbit_msg_interceptor:context(),
                rabbit_msg_interceptor:stage(), rabbit_msg_interceptor:cfg()) ->
    mc:state().
intercept(Msg, Ctx, incoming, _Cfg) ->
    case is_mqtt_protocol(Ctx) of
        true -> add_annotations(Msg, Ctx);
        false -> Msg
    end;
intercept(Msg, _Ctx, _Stage, _Cfg) ->
    Msg.

-spec is_mqtt_protocol(rabbit_msg_interceptor:context()) -> boolean().
is_mqtt_protocol(#{protocol := Proto})
  when Proto =:= mqtt50 orelse
       Proto =:= mqtt311 orelse
       Proto =:= mqtt310 ->
    true;
is_mqtt_protocol(_) ->
    false.

-spec add_annotations(mc:state(), rabbit_msg_interceptor:context()) -> mc:state().
add_annotations(Msg0, #{username := Username,
                        vhost := VHost,
                        connection_name := ConnName}) ->
    Msg1 = maybe_add_annotation(Msg0, ?KEY_USERNAME, Username, username),
    Msg2 = maybe_add_annotation(Msg1, ?KEY_VHOST, VHost, vhost),
    maybe_add_annotation(Msg2, ?KEY_CONN_NAME, ConnName, connection_name).

-spec maybe_add_annotation(mc:state(), binary(), term(), atom()) -> mc:state().
maybe_add_annotation(Msg, Key, Value, ConfigKey) ->
    case is_annotation_enabled(ConfigKey) of
        true -> mc:set_annotation(Key, Value, Msg);
        false -> Msg
    end.

-spec is_annotation_enabled(atom()) -> boolean().
is_annotation_enabled(ConfigKey) ->
    Default = default_enabled(ConfigKey),
    application:get_env(rabbitmq_mqtt_annotation_interceptor, ConfigKey, Default).

-spec default_enabled(atom()) -> boolean().
default_enabled(username) -> true;
default_enabled(vhost) -> false;
default_enabled(connection_name) -> false.
