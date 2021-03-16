-module(otel_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

all() -> [
          successful_span,
          exception_span
         ].

init_per_suite(Config) ->
    ok = application:load(opentelemetry_telemetry),
    ok = application:load(opentelemetry),
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    Config.

end_per_suite(_Config) ->
    ok = application:unload(opentelemetry),
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = application:ensure_all_started(telemetry_registry),
    {ok, _} = application:ensure_all_started(test_app),
    {ok, _} = application:ensure_all_started(opentelemetry_telemetry),
    {ok, _} = application:ensure_all_started(opentelemetry),
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    otel_telemetry:trace_application(test_app),
    opentelemetry:register_tracer(test_tracer, "0.1.0"),
    Config.

end_per_testcase(_, Config) ->
    application:stop(telemetry),
    application:stop(telemetry_registry),
    application:stop(test_app),
    application:stop(opentelemetry_telemetry),
    application:stop(opentelemetry),
    Config.

successful_span(_Config) ->
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx1),
    _Result = test_app:handler(ok),
    ?assertMatch(SpanCtx1, ?current_span_ctx),
    ?end_span(),
    receive
        {span, #span{name=Name1,attributes=Attributes}} ->
            ?assertEqual(<<"test_app_handler">>, Name1),
            Attr = maps:from_list(Attributes),
            ?assert(maps:is_key(<<"duration">>, Attr))
        after
            5000 ->
                error(timeout)
        end,

    receive
        {span, #span{name=Name2}} ->
            ?assertEqual(<<"span-1">>, Name2)
    after
        5000 ->
            error(timeout)
    end,
    ok.

exception_span(_Config) ->
    try test_app:handler(raise_exception) of
        _ -> ok
    catch
        error:badarg -> ok
    end,
    receive
        {span, #span{name=Name,events=Events,status=Status}} ->
            ?assertEqual(<<"test_app_handler">>, Name),
            ?assertEqual({status,error,<<"badarg">>}, Status),
            ?assertEqual(1, erlang:length(Events))
    after
        5000 ->
            error(timeout)
    end,
    ok.
