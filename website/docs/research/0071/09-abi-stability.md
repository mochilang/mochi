---
title: "09. ABI stability"
sidebar_position: 10
sidebar_label: "09. ABI stability"
description: "CPython API/ABI versioning (PEP 387 stability policy, PEP 802 / 809 the abi2026 transition), the abi3 stable ABI (PEP 384), the free-threaded ABI tag (PEP 779 / 803 / cp313t), the manylinux / musllinux wheel platform tags (PEP 600 / 656), the cdylib boundary, opaque handles for non-stable structs."
---

# 09. ABI stability

This note covers the CPython API/ABI surface that the bridge has to track. CPython is a moving target: APIs are deprecated, internal structures change, and the wheel ecosystem has to follow. Mochi's wrapper modules link against CPython and inherit the same constraints.

## The two surfaces: API and ABI

CPython exposes two distinct compatibility surfaces:

- **API** (Application Programming Interface): the C header functions and macros. Source-level compatibility. If your `.c` source compiles against CPython N.x and N+1.x, the API is stable.
- **ABI** (Application Binary Interface): the binary layout of structs (`PyObject`, `PyTypeObject`, `PyMethodDef`), the calling conventions, and the symbol exports. Binary-level compatibility. If a `.so` compiled against CPython N.x loads against N+1.x without recompilation, the ABI is stable.

API compatibility is broader than ABI. Most CPython API changes between minors are ABI-breaking (struct layout changes, new fields, removed exports) but API-preserving (the function signatures still work; just need a recompile).

The bridge has to know which surface it's targeting. The wheel tags encode it.

## PEP 387: CPython's stability policy

PEP 387 governs CPython's API/ABI deprecation policy:

- An API is deprecated for at least 2 minor versions before removal.
- An ABI break in a major minor version requires a new ABI tag.
- The limited API (PEP 384) is a stricter subset; functions in it have stronger stability guarantees.

The bridge's approach: target the limited API (`abi3`) by default for publish; target the full API for consume (we link against the specific CPython we found at runtime, so ABI compatibility is automatic).

## PEP 384: the limited API (abi3)

The limited API is a subset of the CPython C API that promises ABI stability across minor versions. A wheel built against the limited API for Python 3.12 will load on Python 3.13, 3.14, etc., without recompilation.

The wheel tag for limited-API wheels is `cp312-abi3-<platform>`: built against Python 3.12 limited API, ABI-compatible across all 3.12+ versions, for the named platform.

What the limited API does NOT include:

- Direct field access on `PyObject`, `PyTypeObject`, etc. (these structures are opaque under the limited API).
- The full CPython memory allocator (`PyMem_Malloc`, etc.). The limited API has its own allocator interface.
- Some specialised functions (some `_PyXxx` internal functions, some debugging hooks).
- Free-threaded specific functions. See PEP 703 / 779 / 803 below.

What's there:

- Object creation: `PyLong_FromLong`, `PyUnicode_FromString`, `PyList_New`, `PyDict_New`.
- Method calls: `PyObject_GetAttrString`, `PyObject_CallObject`, `PyObject_CallMethod`.
- Type creation: `PyType_FromModuleAndSpec` (this is the limited-API-friendly way to define types from C).
- Module creation: `PyModule_Create2`, `PyModule_AddObject`.
- Error handling: `PyErr_SetString`, `PyErr_Occurred`, `PyErr_Print`.
- GIL: `PyGILState_Ensure`, `PyGILState_Release` (for threads not created by Python).

The Mochi wrapper module synthesiser targets the limited API by default. The synthesised C glue (when needed for C-extension binding) uses only limited-API functions. The result: one wheel per platform works on every 3.12+ minor version.

## PEP 802 / PEP 809: the abi2026 transition

PEP 802 (2024 draft, accepted 2025) and PEP 809 (the renaming and rollout plan) introduce a new ABI versioning scheme.

The current scheme: `cp312`, `cp313`, `cp314`, one ABI tag per CPython minor version. Each tag corresponds to a roughly 1-year release window.

The new scheme (rolling out 2026-Q1): `abi2026`, a date-stamped ABI tag that covers multiple CPython minor versions in a window. The idea is to acknowledge that the ABI changes within a minor (deprecations land) and that the abi3 ladder is too restrictive for some use cases.

PEP 809's rollout:

- 2026-Q1: CPython 3.15 ships with both `cp315` (legacy tag) and `abi2026` (new tag).
- 2027-Q1: CPython 3.16 drops `cp316`, ships only `abi2026` and `abi2027`.
- 2028-Q1: abi2026 reaches end-of-life; older wheels need to be rebuilt against newer abi-year tags.

The bridge tracks the transition: `[python.publish].abi2026 = true` opts in to the new tag. The default remains `cp3XY` until abi2026 is GA.

## Free-threaded CPython: PEP 703 / 779 / 803 / cp313t

PEP 703 (accepted 2023) introduces a build of CPython without the GIL. The build is opt-in: `./configure --disable-gil` produces a "free-threaded" binary that is binary-incompatible with the GIL build.

- The struct layout of `PyObject` differs (reference counting uses biased reference counting).
- The memory allocator differs (per-thread arenas).
- Some C API functions are unsafe to call without explicit thread coordination.

PEP 779 / 803 (2024) define the wheel tag for free-threaded builds: `cp313t`, `cp314t`, etc. Note the `t` suffix; this is the distinguishing marker. A wheel tagged `cp313t-cp313t-<platform>` requires a free-threaded interpreter; it will not load on a normal `cp313`.

The bridge:

- Default: builds for `cp3XY` (GIL build). Free-threaded wheels are an opt-in via `[python].free-threaded = true`.
- Free-threaded mode requires the wrapper module to avoid GIL-only APIs and to use the new thread-safety primitives (PyMutex, atomic refcounts).
- The bridge's wrapper-module synthesiser has a free-threaded flag that switches the emitted C glue to use the safe subset.

The MEP-71 phase plan reserves Phase 17 for free-threaded support; the default through Phase 16 is GIL-only.

## Wheel platform tags

PEP 600 (manylinux), PEP 656 (musllinux), and the platform-specific tags govern the platform compatibility.

**manylinux_X_Y_<arch>**: a Linux build using glibc >= X.Y on architecture `<arch>`. The wheel's compiled extension references only symbols available in glibc X.Y; auditwheel validates this. Common tags:

- `manylinux_2_17_x86_64`: glibc 2.17, x86_64. Compatible with RHEL 7+, Debian 8+, Ubuntu 14.04+.
- `manylinux_2_28_x86_64`: glibc 2.28, x86_64. Compatible with RHEL 8+, Debian 10+, Ubuntu 18.10+.
- `manylinux_2_34_x86_64`: glibc 2.34, x86_64. The 2026 default for new wheels.
- `manylinux_2_34_aarch64`: same on ARM64.

**musllinux_X_Y_<arch>**: a Linux build using musl libc >= X.Y. Alpine Linux is the typical consumer. Tags:

- `musllinux_1_2_x86_64`: musl 1.2.x, x86_64.
- `musllinux_1_2_aarch64`: same on ARM64.

**macosx_X_Y_<arch>**: macOS X.Y on `<arch>`. The `<arch>` is `x86_64`, `arm64`, or `universal2` (fat binary).

**win_amd64**, **win32**, **win_arm64**: Windows on the named architecture.

The Mochi backend computes the right platform tag based on the build environment and validates that the compiled extension actually meets the tag's requirements. Building manylinux wheels requires a glibc-X.Y baseline build environment (typically a manylinux container image).

## The cdylib boundary

For native ext wraps, the synthesised wrapper module is a CPython extension `.so` (Linux), `.dylib` (macOS), or `.pyd` (Windows). The boundary between the Mochi runtime (Go) and the CPython side is a small C glue layer:

```c
// Generated cdylib glue
#define Py_LIMITED_API 0x030C0000   // Limited API for CPython 3.12+
#include <Python.h>

extern void* mochi_runtime_init(void);
extern PyObject* mochi_call(void* runtime, const char* name, PyObject* args);

static PyObject* py_mochi_call(PyObject* self, PyObject* args) {
    const char* name;
    PyObject* call_args;
    if (!PyArg_ParseTuple(args, "sO", &name, &call_args)) return NULL;
    return mochi_call(get_runtime(), name, call_args);
}

static PyMethodDef methods[] = {
    {"mochi_call", py_mochi_call, METH_VARARGS, "Call into Mochi runtime"},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT, "_mochi_native", NULL, -1, methods,
};

PyMODINIT_FUNC PyInit__mochi_native(void) {
    return PyModule_Create(&moduledef);
}
```

The glue is generated, not hand-written. The Mochi runtime side is linked as a Go-built static library (`mochi_runtime.a`) that the glue references. The combination is a `.so` that satisfies CPython's extension-module ABI.

## Opaque handles for non-abi3 structs

Some Python objects have struct layouts that are not stable under the limited API (the limited API treats them as opaque). The bridge handles these via the opaque-handle pattern from [[05-type-mapping]]: the value is held as `PyObject*` and operations go through `PyObject_GetAttrString` / `PyObject_CallMethod` rather than direct field access.

The cost: a method call through `PyObject_CallMethod` is ~10x slower than direct field access. The benefit: the wrapper works across CPython minor versions without recompilation.

The bridge surfaces this as a comment in the generated shim file:

```mochi
// abi3-opaque: field access via PyObject_GetAttrString, ~10x slower than direct
extern fn numpy_array_shape(arr: numpy.NDArray): tuple[int, ...] from python "numpy.ndarray.shape"
```

## Cross-references

- [[05-type-mapping]] for the opaque-handle pattern.
- [[06-pypi-publish-flow]] for the wheel tag computation.
- [[10-gil-and-cextensions]] for free-threaded build details.
- [[11-pyodide-wasi-embedded]] for the WASI and Pyodide ABI angles.
- [PEP 384](https://peps.python.org/pep-0384/) for the limited API.
- [PEP 387](https://peps.python.org/pep-0387/) for the stability policy.
- [PEP 600](https://peps.python.org/pep-0600/), [PEP 656](https://peps.python.org/pep-0656/) for platform tags.
- [PEP 703](https://peps.python.org/pep-0703/) for free-threaded CPython.
- [PEP 779](https://peps.python.org/pep-0779/), [PEP 803](https://peps.python.org/pep-0803/) for the free-threaded ABI tag.
- [PEP 802](https://peps.python.org/pep-0802/), [PEP 809](https://peps.python.org/pep-0809/) for the abi2026 transition.
