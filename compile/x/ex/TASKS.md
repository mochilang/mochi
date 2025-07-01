# Elixir Backend Tasks for TPCH Q1

The Elixir backend is still experimental and TPCH queries compile but do not
execute correctly. The generated `_query` helper emits `if` expressions as
arguments without parentheses which causes syntax errors. Fixing the helper to
wrap these `if` expressions is required before `q1` will run successfully.

Remaining tasks:

- Wrap `if` expressions in `_query` helper with parentheses.
- Update golden files once the runtime issues are resolved.
