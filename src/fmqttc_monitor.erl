-module(fmqttc_monitor).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(DB, "fake-tele").
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
    %% point gun at influx db
    {ok, GunPid} = gun:open("localhost", 8086),
    {ok, _Protocol} = gun:await_up(GunPid),
    %% re-create db
    drop_db(GunPid, ?DB),
    create_db(GunPid, ?DB),
    %% subscribe to mosquitto
    error_logger:info_msg("~p subscribe to ~s", [?MODULE, Topic]),
    {ok, ConnPid} = emqtt:start_link([{clientid, Name}]),
    {ok, _Props} = emqtt:connect(ConnPid),
    {ok, _Props1, _ReasonCodes} = emqtt:subscribe(ConnPid, {Topic, QoS}),
    {noreply, Ctx#{conn => ConnPid, gun => GunPid}}.

handle_info({publish, #{client_pid := Pid} = Msg}, #{conn := Pid, gun := GunPid} = Ctx) ->
    %% store data in influx db
    #{topic := Topic, temp := Temp} = Point = make_point(Msg),
    error_logger:info_msg("~s temp ~.2f~ts", [Topic, Temp, ?C]),
    store_point(GunPid, ?DB, Point),
    {noreply, Ctx};
handle_info({gun_response, GunPid, _, _, Code, _}, #{gun := GunPid} = Ctx) ->
    case Code > 300 of
        true ->
            error_logger:error_msg(
                "~b ~s",
                [Code, httpd_util:reason_phrase(Code)]
            );
        false ->
            ok
    end,
    {noreply, Ctx};
handle_info({gun_data, GunPid, _, _, Data}, #{gun := GunPid} = Ctx) ->
    {noreply, Ctx};
handle_info({gun_up, GunPid, _}, Ctx) ->
    error_logger:info_msg("~s gun up", [?MODULE]),
    {noreply, Ctx#{gun := GunPid}};
handle_info({gun_down, GunPid, _, _, _, _}, #{gun := GunPid} = Ctx) ->
    error_logger:info_msg("~s gun down", [?MODULE]),
    {noreply, Ctx#{gun := undefined}}.

terminate(Reason, #{topic := Topic, conn := ConnPid, gun := GunPid}) ->
    error_logger:info_msg("~s is down: ~p", [?MODULE, Reason]),
    ok = gun:close(GunPid),
    {ok, _Props, _ReasonCode} = emqtt:unsubscribe(ConnPid, Topic),
    ok = emqtt:disconnect(ConnPid),
    ok.

%% influx API

common_headers() ->
    [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}].

drop_db(Gun, Db) ->
    error_logger:info_msg("~p drop db ~s", [?MODULE, Db]),
    Q = http_uri:encode("DROP DATABASE \"" ++ Db ++ "\""),
    Query = iolist_to_binary(["q=", Q]),
    gun:post(Gun, "/query", common_headers(), Query).

create_db(Gun, Db) ->
    error_logger:info_msg("~p create db ~s", [?MODULE, Db]),
    Q = http_uri:encode("CREATE DATABASE \"" ++ ?DB ++ "\""),
    Query = iolist_to_binary(["q=", Q]),
    gun:post(Gun, "/query", common_headers(), Query).

make_point(#{topic := Topic, payload := Payload}) ->
    [Type, Num0] = binary:split(Topic, <<"/">>),
    Num = binary_to_integer(Num0),
    Temp = binary_to_float(Payload),
    TS = os:system_time(millisecond),
    #{topic => Topic, type => Type, num => Num, temp => Temp, time => TS}.

store_point(Gun, Db, Point) ->
    #{
        topic := Topic,
        type := Type,
        num := Num,
        temp := Temp,
        time := TS
    } = Point,
    P = io_lib:format(
        "device,topic=~s,type=~s,num=~b temperature=~f ~b",
        [Topic, Type, Num, Temp, TS]
    ),
    Query = iolist_to_binary(P),
    error_logger:info_msg("~p store point ~s", [?MODULE, Query]),
    Path = "/write?db=" ++ Db ++ "&precision=ms",
    gun:post(Gun, Path, common_headers(), Query).
