defmodule OpentelemetryTelemetry do
  @moduledoc """
  `OpentelemetryTelemetry` provides conveniences for leveraging `telemetry`
  events for `OpenTelemetry` bridge libraries.

  ## OpenTelemetry Contexts

  `opentelemetry` does not automatically set current span context when ending
  another span. Since `telemetry` events are executed in separate handlers with
  no shared context, correlating individual events requires a mechanism to do so.
  The provided `store_ctx/2`, `store_current_ctx/3`, and `pop_ctx/2` functions
  give bridge library authors a mechanism for getting around this challenge.

  ### Example Telemetry Event Handlers

  ```erlang
  def handle_event(_event,
             %{system_time: start_time},
             metadata,
             %{type: :start, tracer_id: tracer_id, span_name: name}) do
    tracer = :opentelemetry.get_tracer(tracer_id)
    OpentelemetryTelemetry.store_current_ctx(tracer_id, metadata)
    start_opts = %{start_time: start_time}
    ctx = :otel_tracer.start_span(tracer, name, start_opts)
    :otel_tracer.set_current_span(ctx)
    ok
  end

  def handle_event(_event,
              %{duration: duration},
              metadata,
              %{type: :stop, tracer_id: tracer_id}) do
      :otel_tracer.set_attribute(:duration, duration)
      :otel_tracer.end_span()
      ctx = OpentelemetryTelemetry.pop_ctx(tracer_id, metadata)
      :otel_tracer.set_current_span(ctx)
      :ok
  end

  def handle_event(_event,
              %{duration: duration},
              %{kind: kind, reason: reason, stacktrace: stacktrace} = metadata,
              %{type: :exception, tracer_id: tracer_id}) do
      status = :opentelemetry.status(:error, to_string(reason, :utf8))
      :otel_span.record_exception(:otel_tracer.current_span_ctx(), kind, reason, stacktrace, [{:duration, duration}])
      :otel_tracer.set_status(status)
      :otel_tracer.end_span()
      ctx = OpentelemetryTelemetry.pop_ctx(tracer_id, metadata)
      :otel_tracer.set_current_span(ctx)
      :ok
    end
  def handle_event(_event, _measurements, _metadata, _config), do: :ok

  ```

  ### Limitations

  Span contexts are currently stored in the process dictionary, so spans can only
  be correlated within a single process at this time. This covers the primary use
  case where library authors have implemented `telemetry:with_span` or the pattern
  established in said function. Non-library authors should use opentelemetry directly
  wherever possible.

  If the `event_metadata` includes a `telemetry_span_context` (introduced in telemetry
  `v0.4.3`), contexts are correlated by the `telemetry_span_context` id to guarantee
  the correct otel span context. Span events in earlier versions of `telemetry` are stored
  in a stack by `tracer_id` to lessen the likelihood of inadvertently closing the wrong
  span.
  """

  @doc """
  Stores the passed `t:OpenTelemetry.span_ctx/0` for a given `tracer_id`.
  """
  defdelegate store_ctx(span_ctx, tracer_id, event_metadata), to: :otel_telemetry

  @doc """
  Convenience function `store_context/3` to use the current span context.
  """
  defdelegate store_current_ctx(tracer_id, event_metadata), to: :otel_telemetry

  @doc """
  Pops and returns a `t:OpenTelemetry.span_ctx/0` for a given `tracer_id`
  from the span context store.
  """
  defdelegate pop_ctx(tracer_id, event_metadata), to: :otel_telemetry

  @doc false
  defdelegate trace_application(app), to: :otel_telemetry
  defdelegate trace_application(app, opts), to: :otel_telemetry
end
