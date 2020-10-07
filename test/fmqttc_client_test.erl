-module(fmqttc_client_test).

-include_lib("eunit/include/eunit.hrl").

callback_mode_test() ->
    ?assertEqual(state_functions, fmqttc_client:callback_mode()).

ready_test_() ->
    {setup,
        fun() ->
            Pid = erlang:list_to_pid("<0.999.0>"),
            meck:new(emqtt),
            meck:expect(emqtt, start_link, 1, {ok, Pid}),
            meck:expect(emqtt, connect, 1, {ok, []}),
            meck:expect(emqtt, publish, 4, {ok, 1}),
            {ok, #{
                name => <<"name">>,
                topic => <<"topic">>,
                qos => 1,
                conn => Pid,
                temp => 1.0,
                packet_id => 0
            }}
        end,
        fun(_) ->
            meck:unload()
        end,
        fun({ok, Ctx}) ->
            [
                ?_assertMatch(
                    {next_state, ready, _},
                    fmqttc_client:ready(cast, connect, Ctx)
                ),
                ?_assertMatch(
                    {next_state, waiting_ack, _},
                    fmqttc_client:ready(cast, publish, Ctx)
                )
            ]
        end}.

waiting_ack_test_() ->
    logger:remove_handler(default),
    Ctx = #{packet_id => 1, interval => 100},
    [
        ?_assertMatch(
            {next_state, ready, _, [{state_timeout, 100, measure}]},
            fmqttc_client:waiting_ack(info, {puback, #{packet_id => 1}}, Ctx)
        ),
        ?_assertMatch(
            {stop, invalid_ack, _},
            fmqttc_client:waiting_ack(info, {puback, #{packet_id => 2}}, Ctx)
        )
    ].
