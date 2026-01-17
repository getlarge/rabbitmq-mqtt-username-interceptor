%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2025 Broadcom. All Rights Reserved. The term "Broadcom"
%% refers to Broadcom Inc. and/or its subsidiaries. All rights reserved.

%% @doc Message interceptor that adds the MQTT username as a message annotation.
%%
%% This interceptor adds an `x-opt-mqtt-username' annotation to messages
%% published via MQTT. The annotation contains the username used to
%% authenticate the MQTT connection.
%%
%% The annotation is only added for MQTT protocols (3.1, 3.1.1, 5.0).
%% Messages from other protocols pass through unchanged.
-module(rabbit_mqtt_username_interceptor).

-behaviour(rabbit_msg_interceptor).

-export([intercept/4]).

-rabbit_boot_step({?MODULE,
                   [{description, "MQTT username message interceptor"},
                    {mfa, {rabbit_msg_interceptor, add, [[{?MODULE, #{}}]]}},
                    {cleanup, {rabbit_msg_interceptor, remove, [[{?MODULE, #{}}]]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

-define(KEY, <<"x-opt-mqtt-username">>).

-spec intercept(mc:state(), rabbit_msg_interceptor:context(),
                rabbit_msg_interceptor:stage(), rabbit_msg_interceptor:cfg()) ->
    mc:state().
intercept(Msg, #{protocol := Proto, username := Username}, incoming, _Cfg)
  when Proto =:= mqtt50 orelse
       Proto =:= mqtt311 orelse
       Proto =:= mqtt310 ->
    mc:set_annotation(?KEY, Username, Msg);
intercept(Msg, _Ctx, _Stage, _Cfg) ->
    Msg.
