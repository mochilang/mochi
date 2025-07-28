# Elixir AST Conversion

This directory provides a small converter that turns a subset of Elixir source
code into Mochi AST form. The implementation mirrors the Python and TypeScript
converters and is mostly regex based.

Completed programs: 0/104

Supported features include basic `def` functions, simple control flow and
`IO.puts` calls. More advanced Elixir constructs such as macros or pattern
matching are not handled.
