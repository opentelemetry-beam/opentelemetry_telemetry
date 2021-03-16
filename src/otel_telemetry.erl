-module(otel_telemetry).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([init/1, init/2, handle_event/4, trace_application/1, trace_application/2]).

init(Application) ->
    init(Application, []).

init(_Application, _Opts) ->
    ok.

trace_application(Application) ->
    trace_application(Application, []).

trace_application(Application, _Opts) ->
    _ = telemetry_registry:discover_all([Application]),
    AllEvents = telemetry_registry:list_events(),
    SpannableEvents = telemetry_registry:spannable_events(),
    _ = register_tracers(AllEvents),
    _ = register_event_handlers(SpannableEvents, AllEvents),
    ok.

store_ctx(SpanCtx, TracerId, EventMetadata) ->
    case maps:get(telemetry_span_context, EventMetadata, undefined) of
        undefined ->
            push_to_tracer_stack(SpanCtx, TracerId);
        TelemetryCtx ->
            erlang:put({otel_telemetry, TelemetryCtx}, SpanCtx)
    end,
    ok.

pop_ctx(TracerId, EventMetadata) ->
    case maps:get(telemetry_span_context, EventMetadata, undefined) of
        undefined ->
            pop_from_tracer_stack(TracerId);
        TelemetryCtx ->
            erlang:erase({otel_telemetry, TelemetryCtx})
    end.

push_to_tracer_stack(SpanCtx, TracerId) ->
    case erlang:get({otel_telemetry, TracerId}) of
        undefined ->
            erlang:put({otel_telemetry, TracerId}, [SpanCtx]);
        Stack ->
            erlang:put({otel_telemetry, TracerId}, [SpanCtx | Stack])
    end.

pop_from_tracer_stack(TracerId) ->
    case erlang:get({otel_telemetry, TracerId}) of
        undefined ->
            undefined;
        [SpanCtx | Rest] ->
            erlang:put({otel_telemetry, TracerId}, Rest),
            SpanCtx
    end.

register_event_handlers(SpannableEvents, AllEvents) ->
    maps:fold(fun (Prefix, Suffixes, Handlers) ->
                      TracerId = tracer_id_for_events(Prefix, Suffixes, AllEvents),
                      NewHandlers = [attach_handler(Prefix, Suffix, TracerId)
                                     || Suffix <- Suffixes],
                      NewHandlers ++ Handlers
              end,
              [],
              SpannableEvents).

register_tracers(AllEvents) ->
    lists:foldl(fun ({_Event, Module}, RegisteredModules) ->
                        case lists:member(Module, RegisteredModules) of
                          true ->
                              RegisteredModules;
                          false ->
                              _Result = opentelemetry:register_application_tracer(Module),
                              [Module | RegisteredModules]
                        end
                end,
                [],
                AllEvents).

attach_handler(Prefix, Suffix, TracerId) ->
    Event = Prefix ++ [Suffix],
    SpanName = list_to_binary(lists:join("_",
                                         [atom_to_binary(Segment, utf8) || Segment <- Prefix])),
    Config = #{tracer_id => TracerId, type => Suffix, span_name => SpanName},
    Handler = fun ?MODULE:handle_event/4,
    telemetry:attach({?MODULE, Event}, Event, Handler, Config).

tracer_id_for_events(Prefix, [Suffix | _], AllEvents) ->
    Event = Prefix ++ [Suffix],
    {Event, Module} = lists:keyfind(Event, 1, AllEvents),
    Module.

handle_event(_Event,
             #{system_time := StartTime},
             Metadata,
             #{type := start, tracer_id := TracerId, span_name := Name}) ->
    StartOpts = #{start_time => StartTime},
    Tracer = opentelemetry:get_tracer(TracerId),
    Ctx = otel_tracer:start_span(Tracer, Name, StartOpts),
    _ = store_ctx(Ctx, TracerId, Metadata),
    ok;
handle_event(_Event,
             #{duration := Duration},
             Metadata,
             #{type := stop, tracer_id := TracerId}) ->
    Ctx = pop_ctx(TracerId, Metadata),
    otel_span:set_attribute(Ctx, <<"duration">>, Duration),
    _ = otel_span:end_span(Ctx),
    ok;
handle_event(_Event,
             #{duration := Duration},
             #{kind := Kind, reason := Reason, stacktrace := Stacktrace} = Metadata,
             #{type := exception, tracer_id := TracerId}) ->
    Ctx = pop_ctx(TracerId, Metadata),
    Status = opentelemetry:status(?OTEL_STATUS_ERROR, atom_to_binary(Reason, utf8)),
    _ = otel_span:record_exception(Ctx, Kind, Reason, Stacktrace, [{<<"duration">>, Duration}]),
    otel_span:set_status(Ctx, Status),
    _ = otel_span:end_span(Ctx),
    ok;
handle_event(_Event, _Measurements, _Metadata, _Config) ->
    ok.

