# mochi-runtime

Runtime support for the Mochi to Ruby transpiler (MEP-56).

Programs emitted by `transpiler3/ruby` `require "mochi/runtime"` and use the
helpers under `Mochi::Runtime::*` for IO, numeric coercion, list / map / set
adapters, query, and channel agents. Each phase of MEP-56 adds the helper
needed for that phase's surface area.

## Layout

```
lib/
  mochi/
    runtime.rb          # umbrella require
    runtime/
      version.rb        # Mochi::Runtime::VERSION
      io.rb             # print / formatted output helpers
```

## Compatibility

- CRuby 3.4.8, 4.0.5 (canonical targets).
- JRuby 10.0.3 (Phase 14+).
- TruffleRuby 33.0.0 (Phase 17).
- mruby 4.0 (Phase 18; degraded surface).

## Versioning

The gem version tracks the MEP-56 implementation series; minor bumps land
with each phase that ships a new helper.
