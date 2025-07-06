Package ex implements helpers for translating small Elixir programs into Mochi.
The conversion relies on the Elixir language server to obtain symbol
information and falls back to light‑weight parsing when necessary.

The main entry points are `FromLSP`, which converts source code by querying the
language server, and `Convert` which uses the bundled parser. Both functions
return Mochi source.
