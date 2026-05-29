---
title: "03. Prior art"
sidebar_position: 4
sidebar_label: "03. Prior art"
description: "Ruby C extension API, FFI gem (libffi), Fiddle (stdlib), Rice (C++ bridge), Rutie and Magnus (Rust-Ruby bridges), PyO3 parallel, RubyInline, SciRuby/PyCall comparisons."
---

# 03. Prior art

This note surveys the landscape of Ruby interoperability mechanisms that existed before MEP-76. The goal is to position MEP-76 in the design space: what each prior approach gets right, what it requires the user to write, and what MEP-76 borrows or diverges from.

## Ruby C extension API

The classic way to write gems with native code. A C extension uses `extconf.rb` (a Ruby script that runs `mkmf` to generate a platform-specific `Makefile`) plus a C source file that calls the CRuby embedding API:

```c
#include "ruby.h"

static VALUE my_add(VALUE self, VALUE a, VALUE b) {
    return INT2NUM(NUM2INT(a) + NUM2INT(b));
}

void Init_my_gem(void) {
    VALUE mod = rb_define_module("MyGem");
    rb_define_module_function(mod, "add", my_add, 2);
}
```

Every major data-processing or systems gem uses this path: nokogiri (libxml2 + libxslt), pg (libpq), sqlite3 (libsqlite3), mysql2, redis-client (hiredis), json (yajl-derived), bcrypt, msgpack, grpc, google-protobuf.

**What it gets right.** Performance. Direct access to underlying C libraries. Gems built this way are the workhorses of the Ruby ecosystem. The mkmf + extconf toolchain is mature, portable, and well-understood.

**What it requires.** C source, platform-specific compile, `ruby/ruby.h` internals. Publishing a C extension gem requires pre-building binary gems per platform (e.g., via rake-compiler-dock).

**MEP-76's relationship.** C extension gems are treated as opaque. The bridge cannot extract RBS from C-defined methods by reading source (the Ruby method table is populated at runtime by `rb_define_method` calls). The bridge relies on pre-shipped `.rbs` files or `gem_rbs_collection` entries for these gems. For gems where neither is available, YARD doc comments (often present even for C extension methods) are the last resort. See [[04-rbs-yard-ingest]] for the fallback chain.

The `native_ext = true` capability flag in `[ruby.capabilities]` is triggered by any gem that ships an `extconf.rb` or a pre-built `.so` / `.bundle`.

## ffi gem (github.com/ffi/ffi)

The `ffi` gem wraps libffi, letting Ruby code call into arbitrary C libraries at runtime without writing a C extension:

```ruby
require 'ffi'

module MyLib
  extend FFI::Library
  ffi_lib 'c'
  attach_function :strlen, [:string], :size_t
end

puts MyLib.strlen("hello")  # => 5
```

The `ffi` gem is widely used for binding C system libraries (libsodium, libmagic, OpenSSL) when a full C extension would be overkill.

**What it gets right.** Pure Ruby call site. No C compile. Portable across CRuby, JRuby, and TruffleRuby (all implement the ffi gem API).

**What it requires.** Hand-written `attach_function` declarations per C symbol. Type mapping is the user's responsibility.

**MEP-76's relationship.** MEP-76 does not use the ffi gem in shims. The shim is a pure Ruby `require` + method dispatch; no C library call is needed. However, gems that internally use the ffi gem (e.g., libsodium wrappers like `rbnacl`) appear in the fixture corpus. The MEP-76 bridge treats them like any other gem: it uses their published RBS signatures, not their ffi internals.

## Fiddle (Ruby stdlib)

Fiddle is Ruby's stdlib counterpart to the ffi gem: it also wraps libffi but is shipped with CRuby rather than as a separate gem.

```ruby
require 'fiddle'
require 'fiddle/import'

module CLib
  extend Fiddle::Importer
  dlload 'libc.so.6'
  extern 'int strlen(char*)'
end
```

**What it gets right.** Zero extra dependencies (stdlib). Portable across platforms where libffi is available.

**What it requires.** Same hand-written `extern` declarations as the ffi gem.

**MEP-76's relationship.** MEP-76 shims do not use Fiddle. The shim is a pure-Ruby `require` + dispatch wrapper. Fiddle is relevant only as background for why MEP-76 does not need a C call layer at all: the target is already Ruby, so there is no C ABI to cross.

## Rice (github.com/jasonroelofs/rice)

Rice is a C++ framework for writing Ruby extensions with a cleaner API than raw CRuby C:

```cpp
#include <rice/rice.hpp>

int add(int a, int b) { return a + b; }

extern "C" void Init_my_gem() {
    Rice::Module rb_mMyGem = Rice::define_module("MyGem");
    rb_mMyGem.define_module_function("add", &add);
}
```

Rice uses C++ templates and RAII to reduce boilerplate and eliminate manual type marshalling. Rice 4.x (released 2022) added a companion `rice-rbs-gen` tool that can generate `.rbs` signatures from Rice-annotated C++ source by inspecting the template instantiations.

**What it gets right.** Dramatically less boilerplate than raw C. With `rice-rbs-gen`, Rice-authored gems produce high-quality `.rbs` files automatically.

**MEP-76's relationship.** MEP-76 benefits indirectly: gems authored with Rice + `rice-rbs-gen` are more likely to ship bundled `.rbs` files, which puts them in the best-coverage tier. The bridge does not special-case Rice gems; it simply finds and parses their `.rbs` files like any other.

## Rutie (github.com/danielpclark/rutie)

Rutie is a Rust-Ruby bridge that goes in the opposite direction from MEP-76: it allows Rust code to call Ruby, or Ruby code to call Rust, by linking Rust code against `libruby`.

```rust
rutie::class!(RubyClass);

rutie::methods!(
    RubyClass,
    _rtself,
    fn pub_greeting() -> rutie::RString {
        rutie::RString::new_utf8("Hello from Rust!")
    }
);

rutie::class!(RubyClass).define(|klass| {
    klass.def("greeting", pub_greeting);
});
```

Rutie uses Ruby's `VALUE` type system directly, requiring `unsafe` Rust at most call sites. The Rutie maintainer has flagged the project as in maintenance mode as of 2024; Magnus (see below) is the more active successor.

**Comparison with MEP-76.** Rutie bridges Rust to Ruby by linking against `libruby`. MEP-76 bridges Mochi to Ruby by running inside CRuby (via MEP-56). The directions differ, but both sit in "pure Ruby" land: neither MEP-76 nor Rutie's consume direction uses a C FFI seam to reach Ruby objects. The key lesson from Rutie: using the VALUE type system directly in an unsafe language causes maintenance burden. MEP-76 avoids this entirely by staying in the Ruby method-dispatch layer.

## Magnus (github.com/matsadler/magnus)

Magnus is the modern successor to Rutie: a safe Rust-Ruby bridge with an ergonomic API that avoids the unsafe Rust in Rutie.

```rust
use magnus::{function, prelude::*, Error, Ruby};

fn greeting(_ruby: &Ruby) -> Result<String, Error> {
    Ok("Hello from Rust!".to_string())
}

#[magnus::init]
fn init(ruby: &Ruby) -> Result<(), Error> {
    let module = ruby.define_module("MyGem")?;
    module.define_module_function("greeting", function!(greeting, 1))?;
    Ok(())
}
```

Magnus uses a lifetime-parameterised API to track Ruby GC roots safely in Rust. It is the recommended path for new Rust-Ruby gems as of 2025.

**Comparison with MEP-76.** A combined MEP-73 + MEP-76 implementation could in theory bridge Mochi to Rust to Ruby (Mochi calls a Rust function via MEP-73, which calls a Ruby gem via Magnus). This is not MEP-76's scope. MEP-76 takes the direct path: Mochi to Ruby via the MEP-56 Ruby runtime. The Magnus comparison is useful because it shows that even the Rust-Ruby bridge space has moved to safe, high-level APIs, confirming the direction of the field away from raw C VALUE manipulation.

## PyCall (github.com/mrkn/pycall.rb)

PyCall is a Ruby gem that lets Ruby code call Python. The direction is Ruby-calls-Python, parallel to MEP-76's direction of Mochi-calls-Ruby.

```ruby
require 'pycall/import'
include PyCall::Import

pyimport :numpy, as: :np
pyfrom :sklearn.linear_model, import: :LinearRegression

model = LinearRegression.new
```

PyCall works by embedding libpython inside the CRuby process (via dlopen), then using Python's C API to call Python objects. Runtime introspection drives the binding: PyCall walks `dir(obj)` and `inspect.signature(fn)` to discover methods; there are no static types.

**Comparison with MEP-76.** PyCall and MEP-76 are parallel in direction (host language calls into guest language). The key difference is the type strategy: PyCall uses runtime introspection with no static type information (everything is `Object` at the Ruby side); MEP-76 uses RBS static types first, with runtime fallback only via YARD heuristics. The bridge generates a statically-typed shim file before any runtime execution occurs.

The lesson MEP-76 takes from PyCall: the "require + direct call" pattern works well for embedding a second language runtime, but runtime introspection alone is insufficient for a type-safe bridge. Static type signatures (RBS for Ruby, `.pyi` stubs for Python) are the right source of truth.

## RubyPython (historical)

RubyPython (2008, now unmaintained) was an earlier Ruby-calls-Python bridge, conceptually similar to PyCall but predating `cffi` and `pycall`'s libpython approach. It used a subprocess model for early versions and a libpython embed for later versions.

**MEP-76's relationship.** Mentioned for completeness. RubyPython's maintenance abandonment illustrates the general risk of cross-language bridges: they require ongoing maintenance as both languages evolve. MEP-76 mitigates this by building on the official Ruby 3.0+ RBS standard (versioned grammar) rather than on heuristic introspection.

## RubyInline (github.com/seattlerb/ruby_inline)

RubyInline (2001, by Ryan Davis) lets Ruby code embed C or C++ code inline in the source file, compiling it at runtime on first use:

```ruby
require 'inline'

class MyClass
  inline do |builder|
    builder.c "int add(int a, int b) { return a + b; }"
  end
end
```

**Comparison with MEP-76.** RubyInline is in the same conceptual family as `eval`-at-runtime approaches. MEP-76 is compile-time-resolved and static-type-checked; the shim is generated before the program runs. The contrast is instructive: MEP-76 sits at the opposite end of the dynamism spectrum from RubyInline.

## Summary: what MEP-76 takes from each

| Prior art | Lesson taken |
|-----------|-------------|
| Ruby C extension API | C extension gems are opaque; rely on pre-shipped RBS or YARD. |
| ffi gem / Fiddle | No C call layer needed; the shim is pure Ruby `require` + dispatch. |
| Rice + rice-rbs-gen | Gems with bundled `.rbs` land in the best-coverage tier automatically. |
| Rutie / Magnus | Stay in the Ruby method-dispatch layer; avoid VALUE type system in unsafe code. |
| PyCall | "require + direct call" works; static type sigs (RBS) must be the primary source, not runtime introspection. |
| PyO3 (via MEP-73 prior art) | The type-sigs-first principle: generate a statically-typed shim before any runtime execution. |
| napi-rs (via MEP-73 prior art) | Auto-generate the wrapper; do not require the gem author to annotate items. |

The MEP-76 niche: like PyCall in direction (host calls into guest), like MEP-73 in architecture (static type extraction + shim synthesis before runtime), unlike all of the above in that the host language (Mochi) and the guest language (Ruby) share the same process via the MEP-56 Ruby emit target, removing the need for any C-level language embedding.

## Cross-references

- [[02-design-philosophy]] §2 for the direct-require vs C FFI decision this note informs.
- [[04-rbs-yard-ingest]] for how the bridge handles C extension gems lacking RBS.
- [MEP-73 §3](/docs/mep/mep-0073) (prior-art-bridges) for the Rust-side prior art (PyO3, neon, napi-rs, uniffi, cxx).
- [MEP-76](/docs/mep/mep-0076) for the normative spec.
- [MEP-56](/docs/mep/mep-0056) for the Ruby emit target that enables the direct-call model.
