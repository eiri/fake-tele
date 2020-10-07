-module(fmqttc_client).

-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).

-export([
    init/1,
    callback_mode/0,
    terminate/3,
    ready/3,
    waiting_ack/3
]).

start_link(Ctx) ->
    gen_statem:start_link(?MODULE, [Ctx], []).

init([#{name := Name} = Ctx]) ->
    process_flag(trap_exit, true),
    logger:set_process_metadata(#{domain => [fmqttc], module => ?MODULE, role => client}),
    ?LOG_INFO(#{name => Name, status => up}),
    _ = crypto:rand_seed(),
    gen_statem:cast(self(), connect),
    {ok, ready, Ctx}.

callback_mode() ->
    state_functions.

ready(cast, connect, #{name := Name} = Ctx) ->
    {ok, ConnPid} = emqtt:start_link([{clientid, Name}]),
    {ok, _Props} = emqtt:connect(ConnPid),
    ?LOG_INFO(#{op => connect, client_pid => ConnPid}),
    gen_statem:cast(self(), publish),
    {next_state, ready, Ctx#{conn => ConnPid, packet_id => 0}};
ready(cast, publish, Ctx) ->
    #{
        topic := Topic,
        qos := QoS,
        conn := ConnPid,
        temp := Temp
    } = Ctx,
    Msg = float_to_binary(Temp),
    ?LOG_INFO(#{op => publish, topic => Topic, msg => Msg}),
    {ok, PktId} = emqtt:publish(ConnPid, Topic, Msg, QoS),
    {next_state, waiting_ack, Ctx#{packet_id := PktId}};
ready(state_timeout, measure, #{temp := Temp0, trend := Trend} = Ctx) ->
    {ok, Temp} = Trend(Temp0),
    ?LOG_INFO(#{op => measure, temperature => Temp}),
    gen_statem:cast(self(), publish),
    {next_state, ready, Ctx#{temp := Temp}}.

waiting_ack(
    info,
    {puback, #{packet_id := PktId}},
    #{packet_id := PktId, interval := Timeout} = Ctx
) ->
    {next_state, ready, Ctx, [{state_timeout, Timeout, measure}]};
waiting_ack(info, {puback, PubAck}, Ctx) ->
    ?LOG_ERROR(#{reason => invalid_ack, ack => PubAck}),
    {stop, invalid_ack, Ctx}.

terminate(Reason, _State, #{name := Name, conn := ConnPid}) ->
    ?LOG_INFO(#{name => Name, status => down, reason => Reason}),
    ok = emqtt:disconnect(ConnPid),
    ok.
