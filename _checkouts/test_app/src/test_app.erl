-module(test_app).

-export([handler/1]).

-telemetry_event [test_app, handler, start].
-telemetry_event [test_app, handler, stop].
-telemetry_event [test_app, handler, exception].

-telemetry_event [test_app, nested_span, start].
-telemetry_event [test_app, nested_span, stop].
-telemetry_event [test_app, nested_span, exception].

-telemetry_event [test_app, only, stop].

-telemetry_event [test_app, cache, miss].
-telemetry_event [test_app, cache, hit].

handler(Args) ->
    _ = telemetry:span(
          [test_app, handler],
          #{},
          fun() ->
                  case Args of
                      raise_exception ->
                          binary_to_list("heh, already a list");
                      _ -> {nested_span(), #{}}
                  end
          end).

nested_span() ->
    _ = telemetry:span(
          [test_app, nested_span],
          #{},
          fun() ->
              {ok, #{}}
          end).
