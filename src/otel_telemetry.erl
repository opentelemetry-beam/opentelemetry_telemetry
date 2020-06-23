-module(otel_telemetry).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([init/1, init/2, handle_event/4]).

init(Application) ->
    init(Application, []).

init(Application, _Opts) ->
    _ = telemetry_registry:discover_all([Application]),
    AllEvents = telemetry_registry:list_events(),
    SpannableEvents = telemetry_registry:spannable_events(),
    _ = register_tracers(AllEvents),
    _ = register_event_handlers(SpannableEvents, AllEvents),
    ok.

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
    Tracer = opentelemetry:get_tracer(TracerId),
    Config = #{tracer => Tracer, type => Suffix, span_name => SpanName},
    Handler = fun ?MODULE:handle_event/4,
    telemetry:attach({?MODULE, Event}, Event, Handler, Config).

tracer_id_for_events(Prefix, [Suffix | _], AllEvents) ->
    Event = Prefix ++ [Suffix],
    {Event, Module} = lists:keyfind(Event, 1, AllEvents),
    Module.

handle_event(_Event,
             #{system_time := StartTime},
             _Metadata,
             #{type := start, tracer := Tracer, span_name := Name}) ->
    StartOpts = #{start_time => StartTime},
    _ = ot_tracer:start_span(Tracer, Name, StartOpts),
    ok;
handle_event(_Event,
             #{duration := Duration},
             _Metadata,
             #{type := stop, tracer := Tracer}) ->
    Ctx = ot_tracer:current_span_ctx(Tracer),
    _ = ot_span:set_attribute(Tracer, Ctx, <<"duration">>, Duration),
    _ = ot_tracer:end_span(Tracer, Ctx),
    ok;
handle_event(_Event,
             #{duration := Duration},
             #{kind := Kind, reason := Reason, stacktrace := Stacktrace},
             #{type := exception, tracer := Tracer}) ->
    Ctx = ot_tracer:current_span_ctx(Tracer),
    Status = opentelemetry:status(?OTEL_STATUS_INTERNAL, atom_to_binary(Reason, utf8)),
    FormattedReason = format_reason(Reason),
    FormattedStacktrace = lists:flatten(io_lib:format("~p", [Stacktrace])),
    ot_span:set_status(Tracer, Ctx, Status),
    ot_span:set_attributes(Tracer,
                           Ctx,
                           [{<<"stacktrace">>, FormattedStacktrace},
                            {<<"kind">>, atom_to_binary(Kind, utf8)},
                            {<<"reason">>, FormattedReason},
                            {<<"duration">>, Duration}]),
    _ = ot_tracer:end_span(Tracer),
    ok;
handle_event(_Event, _Measurements, _Metadata, _Config) ->
    ok.

format_reason({Reason, _}) ->
    atom_to_binary(Reason, utf8);
format_reason(Reason) ->
    atom_to_binary(Reason, utf8).

