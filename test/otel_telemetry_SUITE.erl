-module(otel_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/ot_span.hrl").

all() -> [
          successful_span
         ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = application:ensure_all_started(telemetry_registry),
    {ok, _} = application:ensure_all_started(test_app),
    {ok, _} = application:ensure_all_started(opentelemetry_telemetry),
    {ok, _} = application:ensure_all_started(opentelemetry),
    application:set_env(opentelemetry, processors, [{ot_batch_processor, #{scheduled_delay_ms => 1}}]),
    ot_batch_processor:set_exporter(ot_exporter_pid, self()),
    otel_telemetry:init(test_app),
    Config.

end_per_suite(_Config) ->
    application:stop(telemetry),
    application:stop(telemetry_registry),
    application:stop(telemetry_app),
    application:stop(opentelemetry_telemetry),
    application:stop(opentelemetry),
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    ot_batch_processor:set_exporter(ot_exporter_pid, self()),
    Config.

successful_span(_Config) ->
    Result = test_app:handler(ok),
    ct:print("~p", [Result]),
    receive
        {span, #span{name=Name,attributes=Attributes}} ->
            ?assertEqual(<<"test_app_handler_start">>, Name),
            ?assertMatch(#{}, maps:from_list(Attributes));
        Msg -> ct:print(Msg)
        after
            5000 ->
                error(timeout)
        end,
    ok.
