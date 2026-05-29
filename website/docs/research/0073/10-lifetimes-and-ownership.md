---
title: "10. Lifetimes and ownership"
sidebar_position: 11
sidebar_label: "10. Lifetimes"
description: "The borrow-to-clone strategy at the wrapper layer, the move-to-handle strategy for non-Clone types, how &'a T / &'a mut T / Box<T> / Rc<T> / Arc<T> translate (or refuse), and the lifetime-erasure trade-off that gives the v1 surface."
---

# 10. Lifetimes and ownership

This note documents the strategy by which the wrapper layer reconciles Rust's borrow-checked lifetime model with Mochi's GC ownership model. The strategy is "borrow-to-clone at the boundary, opaque handles for non-Clone types".

## The two ownership models

Rust expresses ownership through three constructs:

- **Owned value (`T`)**: the function takes the value, has unique access, and drops it at the end of the call (or transfers ownership onward).
- **Shared borrow (`&T`)**: the function has read-only access for the lifetime `'a`. The caller retains ownership.
- **Mutable borrow (`&mut T`)**: the function has exclusive read-write access for the lifetime `'a`. The caller retains ownership.

Mochi has only one ownership model: shared, GC-managed. Every value is reachable from the runtime root; values stay alive as long as a reachability chain exists; values become collectable when no chain exists.

The mismatch: Rust borrows have a stack-scoped lifetime; Mochi values have a GC-scoped lifetime. The wrapper layer must convert between these models without violating either.

## The borrow-to-clone strategy

For parameter types `&T` and `&'static T` where T is Clone, the wrapper takes ownership by cloning at the boundary:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_takes_str(name: MochiString) -> MochiString {
    let name = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(name.ptr as *const u8, name.len))
    };
    // call the actual Rust function with a borrowed &str
    let result = upstream_crate::takes_str(name);
    encode_result_as_mochi_string(result)
}
```

The wrapper materialises a `&str` view over the MochiString's bytes for the duration of the call, then encodes the result back. The view does not outlive the wrapper function: the Mochi runtime retains the MochiString through the call, so the underlying bytes are stable.

The strategy works because:

- The caller (Mochi) retains the MochiString through the call (the Rust extern fn takes the struct by value but does not own the heap allocation).
- The wrapper's `&str` view lives only inside the function body, which the Rust borrow checker validates.
- The original Rust function's `&str` is upgraded to either a use of the slice (no clone needed) or a clone-into-String at the upstream-function entry (the wrapper does no extra clone).

For `&[T]` where T is Clone, the same pattern materialises a `&[T]` view over the MochiSlice's elements:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_takes_slice(xs: MochiSlice) -> i64 {
    let xs = unsafe {
        std::slice::from_raw_parts(xs.ptr as *const i64, xs.len)
    };
    upstream_crate::sum(xs)
}
```

The slice view lives only for the call, the underlying buffer is stable for the duration.

## The Result return type

For return types `Result<T, E>` where E is Clone, the wrapper desugars to a Mochi panic:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_compute(input: i64) -> MochiString {
    match upstream_crate::compute(input) {
        Ok(s) => encode_string(s),
        Err(e) => mochi_runtime::panic(format!("compute failed: {:?}", e)),
    }
}
```

The `mochi_runtime::panic` function is a runtime extern that constructs a Mochi panic value and unwinds via `panic = "abort"` + a runtime-side signal handler that catches the abort and converts it to a Mochi-level panic.

A future sub-phase can introduce a Result-shaped translation surface (`extern fn compute(input: int): result<string, ComputeError>` mapping to a Mochi tagged union). v1 uses the panic shape because Mochi has no native Result type.

## The move-to-handle strategy

For non-Clone types and for types whose ownership semantics require move-not-borrow, the wrapper exposes an opaque handle:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_make_thing(input: i64) -> *mut upstream_crate::Thing {
    let thing = upstream_crate::Thing::new(input);
    Box::into_raw(Box::new(thing))
}

#[no_mangle]
pub extern "C" fn mochi_example_thing_query(t: *mut upstream_crate::Thing) -> i64 {
    let t = unsafe { &*t };
    t.query()
}

#[no_mangle]
pub extern "C" fn mochi_example_thing_consume(t: *mut upstream_crate::Thing) -> i64 {
    let t = unsafe { Box::from_raw(t) };
    t.consume()  // takes self
}

#[no_mangle]
pub extern "C" fn mochi_example_thing_free(t: *mut upstream_crate::Thing) {
    let _ = unsafe { Box::from_raw(t) };
}
```

The Mochi runtime treats the `*mut Thing` as an opaque handle with a registered free callback. The handle's lifetime is GC-managed: when Mochi determines the handle is unreachable, it calls `mochi_example_thing_free`.

For a method that takes `self` by value (consuming the receiver), the wrapper uses `Box::from_raw` to recover ownership, calls the method, and lets Rust drop the box. The handle is invalidated on the Mochi side: a subsequent call on the same handle would dereference freed memory. The bridge marks consuming-methods at type-mapping time and the runtime invalidates the handle on return.

## &mut borrow

A `&mut T` parameter is incompatible with the GC model: Rust requires the mut borrow to be unique (no other references can exist for the borrow's lifetime), but the GC cannot prove uniqueness for an opaque-handle-backed value.

The bridge uses the following rules:

- If `T` is repr(C) and Copy: pass by value; mutation does not persist (a copy).
- If `T` is repr(C) and Clone: clone at the boundary; mutation persists locally but not back to the caller. The user must explicitly request a "write-back" wrapper.
- If `T` is an opaque-handle type: the wrapper takes the handle, performs the mutation through `&mut *handle`, and the next read sees the mutation. This is safe because there can only ever be one Mochi-side reference to the handle (the GC tracks reachability), but the runtime must serialise mutating calls (the wrapper takes a per-handle lock).

The per-handle lock is implemented via a `Mutex<()>` table:

```rust
static HANDLE_LOCKS: OnceLock<Mutex<HashMap<usize, Arc<Mutex<()>>>>> = OnceLock::new();

fn lock_for(handle_addr: usize) -> Arc<Mutex<()>> {
    HANDLE_LOCKS.get_or_init(Default::default).lock().unwrap()
        .entry(handle_addr)
        .or_insert_with(|| Arc::new(Mutex::new(())))
        .clone()
}
```

The wrapper acquires the lock before the mutating call and releases after. This is a memory-safety guard, not a deadlock-prevention measure: re-entrant mutating calls on the same handle deadlock. The user must ensure their Mochi-side call graph does not re-enter.

## Box, Rc, Arc

| Rust type | Translation | Notes |
|-----------|-------------|-------|
| `Box<T>` where T in table | The wrapper unboxes at the parameter boundary; encodes the boxed value as the Mochi-side representation. | No opaque handle. |
| `Box<dyn Trait>` | Refused (SkipDynTrait). | No trait dispatch in v1. |
| `Rc<T>` | Refused. | Rc is single-threaded; Mochi's runtime is multi-threaded. |
| `Arc<T>` where T in table | The wrapper extracts the inner value (cloning if T: Clone, refusing otherwise). | The Arc itself is dropped at the wrapper boundary. |
| `Arc<dyn Trait>` | Refused (SkipDynTrait). | |
| `Mutex<T>` / `RwLock<T>` | Refused. | Synchronisation primitives have no Mochi analogue. |
| `&'a T` where T in table | Borrow-to-clone (or borrow-to-view for slices). | |
| `&'a mut T` | Refused (see above note on &mut). | |
| `&'static T` | Same as `&T` for the in-table case; safe for return types because the static lifetime erases. | |

## Lifetime erasure

The wrapper erases all non-'static lifetimes at its boundary. A Rust function `fn foo<'a>(x: &'a str) -> &'a str` has a lifetime that ties the return to the parameter. After erasure, the wrapper signature becomes `fn foo(x: MochiString) -> MochiString`, which loses the tie: the return MochiString owns its bytes independently of the parameter MochiString.

This is correct because:

- The return value materialises on the Mochi heap (a fresh allocation).
- The parameter's lifetime extends through the call.
- After the call, parameter and return are independent.

It is incorrect to expose a wrapper that returns a borrow tied to a parameter: the borrow would be invalidated when the parameter's MochiString is freed, leading to use-after-free. The bridge refuses such functions (SkipLifetime).

The general rule: any non-'static lifetime in the return position is refused. A `&'a T` return where `'a` ties to a parameter requires the wrapper to either copy the return value (when T: Clone) or refuse. The bridge always picks "copy or refuse" because the copy path is generally cheap and the refuse path is unambiguous.

## Static-lifetime returns

A `&'static str` return is safe because the borrow points to a literal in the binary's read-only data section. The wrapper copies the bytes into a fresh MochiString:

```rust
#[no_mangle]
pub extern "C" fn mochi_example_constant_name() -> MochiString {
    let s: &'static str = upstream_crate::constant_name();
    encode_string(s.to_owned())  // owned copy in the MochiString
}
```

The copy is necessary because Mochi-side strings always own their bytes (the runtime cannot distinguish between "owned bytes" and "borrow into another binary's data section").

## Self-referential types and Pin

Self-referential types (those that hold an internal pointer into themselves, requiring Pin) are refused (SkipPin). The wrapper cannot construct a Pin around an FFI-passed value; the pin guarantees do not survive the boundary.

Common self-referential types refused:

- Generators (`fn() -> impl Generator`).
- Some async-fn return types when they capture self.
- `Pin<Box<dyn Future<Output = T>>>`.

These require either the user to wrap them by hand (storing in a runtime-side handle table that respects Pin) or a future sub-phase that adds Pin-handle awareness.

## Cross-references

- [[05-type-mapping]] for the type-by-type translation rules.
- [[09-abi-stability]] for the FFI layer that the lifetimes-and-ownership rules drive.
- [[02-design-philosophy]] §6 for why borrow-to-clone is the right v1 strategy.
- [[12-risks-and-alternatives]] §A6 for the rejected impl-Fn translation that would have introduced its own lifetime problem.
- [Rustonomicon: Ownership](https://doc.rust-lang.org/nomicon/ownership.html) for the Rust ownership model.
