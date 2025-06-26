# Erlang Backend Tasks for TPCH Q1

The Erlang backend can emit simple functions but dataset grouping is not yet implemented.

- Represent each row as a map and build groups using list folds or `maps:update_with`.
- Implement aggregation helpers `sum/1`, `avg/1` and `count/1` for grouped lists.
- Use a JSON library such as `jiffy` to print the final result in tests.
- Add a golden test for `q1.mochi` under `tests/compiler/erlang`.
