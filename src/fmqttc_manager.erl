-module(fmqttc_manager).

-behaviour(gen_server).

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
    error_logger:info_msg("~p is up", [?MODULE]),
    gen_server:cast(self(), start),
    {ok, #{}}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(start, Ctx) ->
    fmqttc_worker_sup:start_child(1),
    {noreply, Ctx};
handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.

handle_info(_, Ctx) ->
    {stop, unknown_info, Ctx}.
