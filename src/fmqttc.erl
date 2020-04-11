-module(fmqttc).

-behaviour(application).
-behaviour(supervisor).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, start_child/1, init/1]).


%% application callbacks

start(_Type, _StartArgs) ->
    fmqttc:start_link().

stop(_State) ->
    ok.


%% supervisor callbacks

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(C) ->
    supervisor:start_child(?MODULE, [C]).

init([]) ->
    MaxRestart = 0,
    MaxWait = 1,
    RestartStrategy = {simple_one_for_one, MaxRestart, MaxWait},
    Children = [{
        fmqttc_client,
        {fmqttc_client, start_link, []},
        transient,
        brutal_kill,
        worker,
        [fmqttc_client]
    }],
    {ok, {RestartStrategy, Children}}.
