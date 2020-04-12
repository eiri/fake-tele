-module(fmqttc_client).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).


-define(C, unicode:characters_to_binary("°")).


start_link(Ctx) ->
    gen_server:start_link(?MODULE, [Ctx], []).


init([#{name := Name} = Ctx]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("~s ~s is up", [?MODULE, Name]),
    _ = crypto:rand_seed(),
    gen_server:cast(self(), publish),
    {ok, Ctx}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(publish, #{name := Name, temp := Temp} = Ctx) ->
    error_logger:info_msg("~s temp ~.2f~ts", [Name, Temp, ?C]),
    {noreply, Ctx, 1000}.

handle_info(timeout, #{temp := Temp0} = Ctx) ->
    Temp = rand:normal(Temp0, 1),
    gen_server:cast(self(), publish),
    {noreply, Ctx#{temp := Temp}}.

terminate(_Reason, #{name := Name}) ->
    error_logger:info_msg("~s ~s is down", [?MODULE, Name]),
    ok.
