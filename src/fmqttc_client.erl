-module(fmqttc_client).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

start_link(Ctx) ->
    gen_server:start_link(?MODULE, [Ctx], []).

init([#{name := Name} = Ctx]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO(#{app => fmqttc, name => Name, status => up}),
    _ = crypto:rand_seed(),
    gen_server:cast(self(), start),
    {ok, Ctx}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(start, #{name := Name} = Ctx) ->
    {ok, ConnPid} = emqtt:start_link([{clientid, Name}]),
    {ok, _Props} = emqtt:connect(ConnPid),
    gen_server:cast(self(), publish),
    {noreply, Ctx#{conn => ConnPid, packet_id => 0}};
handle_cast(publish, Ctx) ->
    #{
        name := Name,
        topic := Topic,
        qos := QoS,
        conn := ConnPid,
        temp := Temp
    } = Ctx,
    Msg = float_to_binary(Temp),
    ?LOG_INFO(#{
        app => fmqttc,
        name => Name,
        op => publish,
        topic => Topic,
        temperature => Temp
    }),
    {ok, PktId} = emqtt:publish(ConnPid, Topic, Msg, QoS),
    {noreply, Ctx#{packet_id := PktId}}.

handle_info(timeout, #{temp := Temp0, trend := T} = Ctx) ->
    {ok, Temp} = T(Temp0),
    gen_server:cast(self(), publish),
    {noreply, Ctx#{temp := Temp}};
handle_info({puback, PubAck}, #{name := Name, interval := Int} = Ctx) ->
    #{packet_id := PktId0} = PubAck,
    #{packet_id := PktId} = Ctx,
    case PktId0 =:= PktId of
        true ->
            {noreply, Ctx, Int};
        false ->
            ?LOG_ERROR(#{
                app => fmqttc,
                name => Name,
                reason => invalid_ack,
                ack => PubAck
            }),
            {stop, invalid_ack, Ctx}
    end.

terminate(Reason, #{name := Name, conn := ConnPid}) ->
    ?LOG_INFO(#{app => fmqttc, name => Name, status => down, reason => Reason}),
    ok = emqtt:disconnect(ConnPid),
    ok.
