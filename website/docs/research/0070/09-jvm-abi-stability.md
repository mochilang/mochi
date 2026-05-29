---
title: "09. JVM ABI stability"
sidebar_position: 10
sidebar_label: "09. JVM ABI stability"
description: "JVM bytecode binary compatibility rules, @JvmName and @JvmOverloads annotations, the Kotlin binary compatibility validator, the versioning policy for the synthesised wrapper, GraalVM Native Image versioning, and the lock --check reproducibility protocol."
---

# 09. JVM ABI stability

The bridge links the Mochi binary against a GraalVM-compiled native image of the Kotlin wrapper. Stability of this link depends on three independent versioning systems: JVM bytecode ABI, Kotlin's own binary compatibility rules, and GraalVM Native Image's C ABI. This note documents all three.

## JVM bytecode binary compatibility

The JVM specification guarantees backward compatibility for `.class` files compiled to an older target version: a `.class` compiled for JVM 17 (`--release 17`) runs on JVM 17, 21, 22, etc. The bridge pins the `[kotlin] jvm-target` to ensure the wrapper JAR is compatible with the GraalVM version's supported JVM class version.

JVM forward compatibility (running a class compiled for JVM 21 on JVM 17) is not guaranteed and not needed: the bridge compiles the wrapper with the same GraalVM CE's `javac`, so the class file version matches the JVM version inside the native image exactly.

**Binary compatibility rules for library updates:** When a Kotlin library releases a new version, the following changes are backward-binary-compatible (safe to upgrade without regenerating the wrapper):

- Adding new functions, classes, or properties.
- Adding default parameter values.
- Adding `@JvmOverloads` to an existing function.
- Widening a function's return type (covariant).

The following changes break binary compatibility and require `mochi pkg lock` to regenerate:

- Removing a function or class from the public API.
- Changing a function's parameter types.
- Narrowing a return type (contravariant).
- Changing a class's `open`/`final`/`abstract` modifier.
- Moving a function between classes.

The bridge detects breakage by comparing the `metadata-sha256` in `mochi.lock` against the current JAR's metadata hash on `mochi pkg lock`. Any difference triggers a full re-ingest.

## `@JvmName` and `@JvmOverloads`

Two Kotlin annotations affect how the bridge sees JVM function signatures:

**`@JvmName("newName")`**: changes the function's JVM bytecode name without affecting the Kotlin name. The bridge reads the JVM-level name from the class file (via the `kotlin.Metadata` `JvmMethodSignature` field if present, or the class file's `method_info` name otherwise). The shim function is named after the Kotlin name (from `@kotlin.Metadata`), but the JNI call uses the JVM name.

**`@JvmOverloads`**: generates one JVM method per default-argument combination. The bridge emits one Mochi `extern fn` per generated overload, with suffix `_N` for the N-argument variant, unless the parameter names differ enough to be unambiguous.

**`@JvmStatic`**: marks companion object functions as JVM static methods. The bridge prefers the static call path (no `Companion.INSTANCE` indirection) when `@JvmStatic` is present.

## Kotlin Binary Compatibility Validator

JetBrains ships the [Kotlin Binary Compatibility Validator](https://github.com/Kotlin/binary-compatibility-validator) Gradle plugin, which dumps the public ABI surface to a `.api` file and fails the build if it changes without a version bump. Libraries that use this plugin (including all JetBrains first-party libraries) have a machine-verifiable ABI changelog.

The bridge does not use the `.api` dump directly (it reads `@kotlin.Metadata` from class files). However, when the bridge detects that a library uses the Binary Compatibility Validator plugin (identified by the presence of a `.api` file in the sources JAR), it cross-checks the ingested surface against the `.api` dump and emits a warning if the two disagree. This catch is a defence-in-depth measure, not a primary path.

## GraalVM Native Image versioning

The GraalVM native image is compiled with a specific GraalVM CE version, pinned in `mochi.lock` as `graalvm-version`. The native image's C ABI (the `graal_create_isolate`, `graal_tear_down_isolate` lifecycle, and the JNI-shaped function entry points) is stable across GraalVM 21.x releases.

Between major GraalVM versions (21 → 22, 22 → 23), the `graal_create_isolate` signature may change. The bridge checks the GraalVM version at link time and emits a compilation error if the host GraalVM version does not match `mochi.lock`.

**`lock --check` reproducibility:** The `mochi pkg lock --check` command recomputes:

1. `jar-sha256` and `jar-blake3` of each JAR against the cached copy.
2. `metadata-sha256` of the `@kotlin.Metadata` annotation content extracted from the JAR.
3. `wrapper-sha256` of the synthesised Kotlin/Java wrapper source directory (SHA-256 of all file contents, sorted by path).
4. `native-image-sha256` of the `libwrap.so` produced by GraalVM.

Any mismatch exits non-zero. This is the CI gate that ensures the binaries in the developer's local environment match those in CI and in the lock file.

## GraalVM native image C ABI

A GraalVM native image shared library exposes three categories of C symbols:

**Lifecycle:**
```c
graal_isolate_t*       graal_create_isolate(graal_create_isolate_params_t*, graal_isolate_t**, graal_isolatethread_t**);
int                    graal_attach_thread(graal_isolate_t*, graal_isolatethread_t**);
int                    graal_detach_thread(graal_isolatethread_t*);
int                    graal_tear_down_isolate(graal_isolatethread_t*);
```

**Entry points (one per `@CEntryPoint`-annotated function):**
```c
jstring   com_example_mylib_fetchUser(graal_isolatethread_t*, jlong id);
jobject   com_example_mylib_getUsers(graal_isolatethread_t*);
jboolean  com_example_mylib_deleteUser(graal_isolatethread_t*, jlong id);
```

Every entry point takes `graal_isolatethread_t*` as the first parameter; this is the thread handle that GraalVM uses for thread attachment. The bridge's Go code (`package3/kotlin/wrapper/jni_caller.go`) maintains a global `graal_isolatethread_t*` that is attached once per goroutine (using `runtime.LockOSThread()`) and passed to every JNI call.

**The `graalvm/` package** in `package3/kotlin/graalvm/` wraps the GraalVM lifecycle in a Go-idiomatic API:

```go
type Isolate struct { ptr *C.graal_isolate_t }

func NewIsolate() (*Isolate, error)
func (i *Isolate) AttachCurrentThread() (*Thread, error)
func (t *Thread) Detach() error
func (i *Isolate) TearDown() error
```

## Cross-references

- [[08-coroutines-bridge]] for the threading model inside the native image.
- [[12-risks-and-alternatives]] §R1 (GraalVM native-image limitations) for the dynamic-class-loading risk.
- [MEP-70 §6](/docs/mep/mep-0070#6-lockfile-kotlin-package) for the `[[kotlin-package]]` lock entries.
