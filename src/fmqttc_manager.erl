-module(fmqttc_manager).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    logger:set_process_metadata(#{domain => [fmqttc], module => ?MODULE, role => manager}),
    ?LOG_INFO(#{status => up}),
    _ = crypto:rand_seed(),
    ClientsNum = 12,
    gen_server:cast(self(), {start, ClientsNum}),
    {ok, dict:new()}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast({start, ClientsNum}, Ctx) ->
    ?LOG_INFO(#{op => start, msg => "starting clients", count => ClientsNum}),
    NewCtx = lists:foldl(
        fun(_, Acc) ->
            Name = name(),
            Topic = <<"valve/", Name/binary, "/temperature">>,
            ClientCtx = #{
                name => Name,
                topic => Topic,
                qos => 1,
                temp => rand:normal(0, 5),
                interval => 800 + rand:uniform(400),
                trend => trend()
            },
            {ok, Pid} = fmqttc:start_client(ClientCtx),
            dict:store(Name, ClientCtx#{pid => Pid}, Acc)
        end,
        Ctx,
        lists:seq(1, ClientsNum)
    ),
    {noreply, NewCtx};
handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.

handle_info(_, Ctx) ->
    {stop, unknown_info, Ctx}.

name() ->
    <<I:64>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("~.16b", [I])).

trend() ->
    trend(random).

trend(random) ->
    fun(T) ->
        {ok, rand:normal(T, 1)}
    end.
