-module(fmqttc_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).


start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).


init([Args]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("~p (~p) child is up", [?MODULE, Args]),
    {ok, #{}}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.

handle_info(_, Ctx) ->
    {stop, unknown_info, Ctx}.

terminate(_Reason, _Ctx) ->
    error_logger:info_msg("~p child is down", [?MODULE]),
    ok.
