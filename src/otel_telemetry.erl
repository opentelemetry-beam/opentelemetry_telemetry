-module(otel_telemetry).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([
         init/1,
         init/2,
         handle_event/4,
         pop_ctx/2,
         store_ctx/3,
         store_current_ctx/2,
         trace_application/1,
         trace_application/2]).

-spec init(atom()) -> ok.
init(Application) ->
    init(Application, []).

-spec init(atom(), []) -> ok.
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

-spec store_current_ctx(atom(), telemetry:event_metadata()) -> ok.
store_current_ctx(TracerId, EventMetadata) ->
    CurrentCtx = otel_tracer:current_span_ctx(),
    store_ctx(CurrentCtx, TracerId, EventMetadata).

-spec store_ctx(opentelemetry:span_ctx(), atom(), telemetry:event_metadata()) -> ok.
store_ctx(SpanCtx, TracerId, EventMetadata) ->
    case maps:get(telemetry_span_context, EventMetadata, undefined) of
        undefined ->
            push_to_tracer_stack(SpanCtx, TracerId);
        TelemetryCtx ->
            erlang:put({otel_telemetry, TelemetryCtx}, SpanCtx)
    end,
    ok.

-spec pop_ctx(atom(), telemetry:event_metadata()) -> opentelemetry:span_ctx().
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
    lists:foldl(fun ({Prefix, Suffixes}, Handlers) ->
                      TracerId = tracer_id_for_events(Prefix, Suffixes, AllEvents),
                      NewHandlers = [attach_handler(Prefix, Suffix, TracerId)
                                     || Suffix <- Suffixes],
                      NewHandlers ++ Handlers
              end,
              [],
              SpannableEvents).

register_tracers(AllEvents) ->
    lists:foldl(fun ({_Event, Module, _Metadata}, RegisteredModules) ->
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
    {Event, Module, _Metadata} = lists:keyfind(Event, 1, AllEvents),
    Module.

handle_event(_Event,
             #{system_time := StartTime},
             Metadata,
             #{type := start, tracer_id := TracerId, span_name := Name}) ->
    Tracer = opentelemetry:get_tracer(TracerId),
    _ = store_current_ctx(TracerId, Metadata),
    StartOpts = #{start_time => StartTime},
    Ctx = otel_tracer:start_span(Tracer, Name, StartOpts),
    otel_tracer:set_current_span(Ctx),
    ok;
handle_event(_Event,
             #{duration := Duration},
             Metadata,
             #{type := stop, tracer_id := TracerId}) ->
    otel_tracer:set_attribute(<<"duration">>, Duration),
    _ = otel_tracer:end_span(),
    Ctx = pop_ctx(TracerId, Metadata),
    otel_tracer:set_current_span(Ctx),
    ok;
handle_event(_Event,
             #{duration := Duration},
             #{kind := Kind, reason := Reason, stacktrace := Stacktrace} = Metadata,
             #{type := exception, tracer_id := TracerId}) ->
    Status = opentelemetry:status(?OTEL_STATUS_ERROR, atom_to_binary(Reason, utf8)),
    _ = otel_span:record_exception(otel_tracer:current_span_ctx(), Kind, Reason, Stacktrace, [{<<"duration">>, Duration}]),
    otel_tracer:set_status(Status),
    _ = otel_tracer:end_span(),
    Ctx = pop_ctx(TracerId, Metadata),
    otel_tracer:set_current_span(Ctx),
    ok;
handle_event(_Event, _Measurements, _Metadata, _Config) ->
    ok.

