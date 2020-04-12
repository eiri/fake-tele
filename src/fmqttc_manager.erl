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
    _ = crypto:rand_seed(),
    ClientsNum = rand:uniform(12),
    gen_server:cast(self(), {start, ClientsNum}),
    {ok, dict:new()}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast({start, ClientsNum}, Ctx) ->
    error_logger:info_msg("~p starting ~b clients", [?MODULE, ClientsNum]),
    NewCtx = lists:foldl(fun(_N, Acc) ->
        Name = name(),
        ClientCtx = #{
            name => Name,
            temp => rand:normal(0, 5),
            interval => 800 + rand:uniform(400),
            trend => fun(T) -> {ok, rand:normal(T, 1)} end
        },
        {ok, Pid} = fmqttc:start_client(ClientCtx),
        dict:store(Name, ClientCtx#{pid => Pid}, Acc)
    end, Ctx, lists:seq(1, ClientsNum)),
    {noreply, NewCtx};
handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.

handle_info(_, Ctx) ->
    {stop, unknown_info, Ctx}.


name() ->
    <<I:64>> = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("~.16b", [I])).
