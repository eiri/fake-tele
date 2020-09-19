-module(fmqttc_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, stop_child/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

stop_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
    Strategy = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    Child = #{
        id => fmqttc_client,
        start => {fmqttc_client, start_link, []},
        restart => transient,
        shutdown => 1
    },
    {ok, {Strategy, [Child]}}.
