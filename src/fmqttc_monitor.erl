-module(fmqttc_monitor).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).


-define(C, unicode:characters_to_binary("°")).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("~s is up", [?MODULE]),
    Ctx = #{
        name => <<"monitor">>,
        topic => <<"valve/#">>,
        qos => 1
    },
    gen_server:cast(self(), subscribe),
    {ok, Ctx}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(subscribe, Ctx) ->
    #{
        name := Name,
        topic := Topic,
        qos := QoS
    } = Ctx,
    {ok, ConnPid} = emqtt:start_link([{clientid, Name}]),
    {ok, _Props} = emqtt:connect(ConnPid),
    {ok, _Props1, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, QoS}),
    {noreply, Ctx#{conn => ConnPid}}.

handle_info({publish, #{client_pid := Pid} = Msg}, #{conn := Pid} = Ctx) ->
    #{
        topic := Topic,
        payload := Payload
    } = Msg,
    Temp = binary_to_float(Payload),
    error_logger:info_msg("~s temp ~.2f~ts", [Topic, Temp, ?C]),
    {noreply, Ctx}.
