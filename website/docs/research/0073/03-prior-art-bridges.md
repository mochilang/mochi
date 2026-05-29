---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art"
description: "PyO3, neon, napi-rs, uniffi, diplomat, swift-bridge, cxx, autocxx, wit-bindgen, JNI / JNR, rustler, mlua. What each gets right, what each requires the user to write, what each rejects, and what MEP-73 borrows or diverges from."
---

# 03. Prior-art bridges

This note surveys the prior-art landscape of Rust-to-other-language bridges. The goal is to position MEP-73 in the design space: which problems each prior bridge solves, which it leaves unsolved, and which lessons MEP-73 takes forward.

## PyO3 (Rust ↔ Python)

PyO3 (`pyo3` crate, GA 2017, dominant Python-Rust bridge by 2020) lets a Rust library expose itself to Python via attribute macros:

```rust
use pyo3::prelude::*;

#[pyfunction]
fn add(a: i64, b: i64) -> PyResult<i64> {
    Ok(a + b)
}

#[pymodule]
fn my_module(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(add, m)?)?;
    Ok(())
}
```

The user annotates each Rust function with `#[pyfunction]`, builds with maturin, ships a wheel. Python imports as if the module were pure Python.

**What it gets right.** Mature, fast, well-documented. The attribute-macro model means the bridge spec is local to each item.

**What it requires.** Every exposed item needs an annotation. There is no auto-generation; the user writes the bridge.

**MEP-73 divergence.** MEP-73 reverses the direction: instead of the Rust author annotating items for consumption, the Mochi importer auto-generates the wrapper from rustdoc JSON. The PyO3 user writes Rust glue; the MEP-73 user writes nothing.

## neon (Rust ↔ Node.js)

neon (GA 2015, the original Node-Rust bridge) is similar to PyO3 in shape: the Rust author annotates items, the bridge generates the N-API surface, the Node user imports as if pure JavaScript.

```rust
fn add(mut cx: FunctionContext) -> JsResult<JsNumber> {
    let a = cx.argument::<JsNumber>(0)?.value(&mut cx);
    let b = cx.argument::<JsNumber>(1)?.value(&mut cx);
    Ok(cx.number(a + b))
}
```

**What it gets right.** First-mover advantage in the Node ecosystem. Clean N-API integration.

**What it requires.** Hand-written argument extraction (`cx.argument::<JsNumber>(0)`), explicit type marshalling. More verbose than PyO3 at the per-function level.

**MEP-73 divergence.** Same as PyO3: MEP-73 doesn't require Rust-side bridge code.

## napi-rs (Rust ↔ Node.js, second generation)

napi-rs (GA 2019, by Xinran Xu) replaces neon's verbose argument extraction with attribute macros similar to PyO3:

```rust
#[napi]
fn add(a: i64, b: i64) -> i64 {
    a + b
}
```

By 2024 napi-rs has overtaken neon in download share. The proc-macro-driven model became the new dominant strategy.

**What it gets right.** Idiomatic Rust. The macros generate everything; the user writes a normal-looking Rust function.

**What it requires.** The `#[napi]` annotation on every exported item.

**MEP-73 divergence.** The proc-macro path is closed to MEP-73: requiring the Rust author to add macros violates "no boilerplate." MEP-73 reads rustdoc JSON of unmodified Rust source instead.

## uniffi-rs (Rust ↔ Swift, Kotlin, Python, Ruby, Go)

uniffi (Mozilla, GA 2021, used in production by Firefox and the Tor Browser) takes a different approach: the user authors an interface description in a `.udl` (UniFFI Description Language) file, uniffi generates the Rust-side `extern "C"` glue and the host-side bindings.

```
// example.udl
namespace example {
    string greet(string name);
};
```

```rust
fn greet(name: String) -> String {
    format!("Hello, {}!", name)
}

uniffi::include_scaffolding!("example");
```

**What it gets right.** One interface description, many host languages. Strong typing across the FFI boundary. Mature multi-language story.

**What it requires.** The `.udl` file plus the `include_scaffolding!` call plus the host-side `uniffi_bindgen` invocation. Three layers of glue.

**MEP-73 divergence.** uniffi puts the interface description on the Rust side. MEP-73 puts it on the Mochi side (the user's `mochi.toml` plus the synthesised shim file). The bridge is automatic where uniffi is declarative.

## diplomat (Rust ↔ multiple)

diplomat (ICU4X project, GA 2022) annotates Rust functions with `#[diplomat::bridge]` and generates bindings for C, C++, JavaScript, Dart, Kotlin, and others. Similar to uniffi but with a Rust-side annotation model rather than a separate `.udl`.

**What it gets right.** Multi-language fan-out from a single Rust source. Used in production by the ICU4X i18n library.

**What it requires.** The `#[diplomat::bridge]` annotation on every exposed item; a separate post-processing step (`diplomat-tool`) per host language.

**MEP-73 divergence.** Same boilerplate violation. MEP-73 reads rustdoc JSON of unmodified Rust source.

## swift-bridge (Rust ↔ Swift)

swift-bridge (GA 2022, by Chinedu Francis Nwafili) connects Rust and Swift via a `#[swift_bridge::bridge]` block:

```rust
#[swift_bridge::bridge]
mod ffi {
    extern "Rust" {
        type Counter;
        #[swift_bridge(init)]
        fn new() -> Counter;
        fn count(self: &Counter) -> u32;
        fn increment(self: &mut Counter);
    }
}
```

**What it gets right.** Native-feeling Swift on the consumer side, idiomatic Rust on the producer side. Strong typing.

**What it requires.** The `#[swift_bridge::bridge]` block per Rust crate.

**MEP-73 divergence.** Same boilerplate. MEP-73 reads rustdoc JSON.

## cxx (Rust ↔ C++)

cxx (GA 2020, by David Tolnay) is the most rigorous Rust-C++ bridge. The user declares a `#[cxx::bridge]` block listing the items and types exposed in each direction:

```rust
#[cxx::bridge]
mod ffi {
    extern "Rust" {
        fn add(a: i32, b: i32) -> i32;
    }
    unsafe extern "C++" {
        include!("my_header.h");
        fn cpp_func(s: &CxxString) -> UniquePtr<CxxString>;
    }
}
```

**What it gets right.** Bidirectional. Lifetimes correctly propagated across the FFI boundary. Strong static checks.

**What it requires.** The hand-written bridge block. Boilerplate per crate.

**MEP-73 divergence.** Same. MEP-73 generates the bridge block equivalent from rustdoc JSON.

## autocxx (Rust ↔ C++, automated)

autocxx (Google, GA 2021) extends cxx with automatic binding generation from C++ headers. The user provides a list of types and functions, autocxx walks the headers via libclang and generates the cxx bridge block.

```rust
autocxx::include_cpp! {
    #include "my_header.h"
    safety!(unsafe_ffi)
    generate!("cpp_func")
}
```

**What it gets right.** Auto-discovery via libclang. Closer to MEP-73's "no boilerplate" promise.

**What it requires.** The `include_cpp!` block with the list of items to generate.

**MEP-73 divergence.** Even closer in spirit. MEP-73 takes the same approach but for Rust source (via rustdoc JSON) rather than C++ headers (via libclang).

## wit-bindgen (Rust ↔ Wasm Component Model)

wit-bindgen (Bytecode Alliance, GA 2022, evolving rapidly through 2024-2026) generates bindings between Rust and the Wasm Component Model interface types described in `.wit` (WebAssembly Interface Types) files.

```wit
interface example {
    greet: func(name: string) -> string
}
```

```rust
wit_bindgen::generate!({
    inline: r#"
        package example:default
        world example {
            export example
        }
    "#
});

struct Component;
impl Guest for Component { fn greet(name: String) -> String { format!("Hello, {}!", name) } }
```

**What it gets right.** Forward-compatible with the broader Component Model. Strong typing across heterogeneous runtime languages. The future of Wasm interop.

**What it requires.** The `.wit` file plus the `wit_bindgen::generate!` block.

**MEP-73 divergence.** wit-bindgen and MEP-73 are complementary. v1 of MEP-73 does not consume `.wit`. A v2 mode (`--mode=wit`) could consume `.wit` when the crate ships one; this is deferred to [[12-risks-and-alternatives]] §A6.

## JNI / JNR (Rust ↔ Java)

JNI (Java Native Interface, GA 1997, the canonical Java-C bridge) and JNR (a Java reflection-based wrapper) let Java call into C / Rust libraries. Rust crates target JNI via the `jni` crate.

```rust
#[no_mangle]
pub extern "system" fn Java_com_example_Foo_add(env: JNIEnv, _class: JClass, a: jint, b: jint) -> jint {
    a + b
}
```

**What it gets right.** Production-grade Java integration story.

**What it requires.** Every function hand-written with the `Java_<package>_<class>_<method>` naming convention. JNI is verbose by design.

**MEP-73 divergence.** Same. MEP-73 generates the equivalent for Mochi (the wrapper crate) from rustdoc JSON.

## rustler (Rust ↔ Erlang / Elixir)

rustler (GA 2015, by hansihe) lets Elixir NIF modules be written in Rust. The user annotates each function with `#[rustler::nif]`.

```rust
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 { a + b }
```

**What it gets right.** The cleanest Rust-NIF integration.

**What it requires.** The `#[rustler::nif]` annotation.

**MEP-73 divergence.** Same. MEP-73 doesn't need Rust-side annotation.

## mlua (Rust ↔ Lua)

mlua exposes Rust functions to Lua via runtime registration:

```rust
let lua = Lua::new();
lua.globals().set("add", lua.create_function(|_, (a, b): (i64, i64)| Ok(a + b))?)?;
```

**What it gets right.** Dynamic, no compile-time interface description.

**What it requires.** Hand-written registration calls per function.

**MEP-73 divergence.** MEP-73 is compile-time-resolved; the synthesised wrapper crate makes everything statically callable.

## The MEP-73 niche

Reading the landscape, every prior bridge requires the Rust crate author to either:

1. Annotate items with proc-macro attributes (PyO3, neon, napi-rs, swift-bridge, rustler, diplomat).
2. Author a separate interface description (uniffi `.udl`, cxx `#[cxx::bridge]`, wit-bindgen `.wit`).
3. Register items at runtime (mlua).

In all three categories the Rust author must do work specifically to support the bridge. The Rust author chooses which items to expose and how.

MEP-73 inverts: the bridge consumes any unmodified Rust crate published to crates.io. The Mochi user (the consumer) drives the discovery via rustdoc JSON; the Rust author has no per-bridge obligation. autocxx is the closest prior-art in spirit: it also auto-discovers the surface from machine-readable description (libclang for C++). MEP-73 is "autocxx for Rust crates, via rustdoc JSON, with a Go-side synthesiser, with capability and lockfile integration."

The trade-off MEP-73 accepts that the prior bridges do not: the bridge can refuse items the closed table cannot translate (lifetimes, generics outside `[rust.monomorphise]`, raw pointers, `impl Trait`). The prior bridges can express anything the bridge author writes by hand; MEP-73 can only express what the closed table covers. The escape hatch is the `extern fn ... custom` override, which lets the user take FFI responsibility item-by-item.

## Lessons taken forward

- **From PyO3 + napi-rs**: the wrapper-crate model with `extern "C"` symbols is the right ABI shape. MEP-73 generates the wrapper that PyO3 / napi-rs require the user to write.
- **From uniffi**: a strict separation between "interface description" and "binding generation" is the right architectural shape. In MEP-73 the interface description is rustdoc JSON, the binding generation is the wrapper synthesiser.
- **From cxx**: bidirectional bridges work; lifetimes can be encoded across the FFI boundary; opaque handles are the right strategy for non-repr(C) types.
- **From autocxx**: machine-readable source description (libclang for C++, rustdoc JSON for Rust) eliminates the boilerplate-per-crate cost.
- **From wit-bindgen**: WIT is the long-term destination. v1 does not consume it; v2 should.
- **From the broader supply-chain story**: Sigstore-keyless OIDC is the only acceptable publish path in 2026.

## Cross-references

- [[02-design-philosophy]] for the rationale of the wrapper-crate model.
- [[04-rustdoc-json-ingest]] for the schema-side detail of the autocxx-equivalent ingest.
- [[09-abi-stability]] for the extern "C" ABI choice the wrappers ride on.
- [MEP-73 §Alternatives](/docs/mep/mep-0073#alternatives-considered) for the normative rejection list.
