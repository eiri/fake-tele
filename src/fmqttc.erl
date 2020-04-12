-module(fmqttc).

-behaviour(application).
-behaviour(supervisor).

%% public API
-export([start_client/1]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, init/1]).


%% Public API

start_client(#{} = Ctx) ->
    fmqttc_client_sup:start_child(Ctx).


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
            id => fmqttc_client_sup,
            start => {fmqttc_client_sup, start_link, []},
            type => supervisor
        },
        #{
            id => fmqttc_manager,
            start => {fmqttc_manager, start_link, []}
        }
    ],
    {ok, {#{}, Children}}.
