-module(fmqttc).

-behaviour(application).
-behaviour(supervisor).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, start_child/1, stop_child/1, init/1]).


%% application callbacks

start(_Type, _StartArgs) ->
    fmqttc:start_link().

stop(_State) ->
    ok.


%% supervisor callbacks

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{
            id => fmqttc_worker_sup,
            start => {fmqttc_worker_sup, start_link, []},
            type => supervisor
        },
        #{
            id => fmqttc_manager,
            start => {fmqttc_manager, start_link, []}
        }
    ],
    {ok, {#{}, Children}}.
