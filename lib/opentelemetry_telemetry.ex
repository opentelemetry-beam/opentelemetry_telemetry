defmodule OpentelemetryTelemetry do
  @moduledoc """
  Documentation for `OpentelemetryTelemetry`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> OpentelemetryTelemetry.hello()
      :world

  """

  @doc """

  """
  defdelegate store_ctx(span_ctx, tracer_id, event_metadata), to: :otel_telemetry

  @doc """

  """
  defdelegate store_current_ctx(tracer_id, event_metadata), to: :otel_telemetry

  defdelegate pop_ctx(tracer_id, event_metadata), to: :otel_telemetry

  @doc false
  defdelegate trace_application(app), to: :otel_telemetry
  defdelegate trace_application(app, opts), to: :otel_telemetry
end
