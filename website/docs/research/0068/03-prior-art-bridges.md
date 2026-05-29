---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art"
description: "pythonnet, IKVM.NET, CsWin32, ClangSharp, uniffi, GraalVM polyglot, JNI-style manual bridging, SwiftUI + C headers, diplomat, Kotlin/Native COM interop. What each gets right, what each requires the user to write, and what MEP-68 borrows or diverges from."
---

# 03. Prior-art bridges

This note surveys the prior-art landscape of .NET-to-other-language bridges and broadly applicable binding generators. The goal is to position MEP-68 in the design space: which problems each prior bridge solves, which it leaves unsolved, and which lessons MEP-68 takes forward.

## pythonnet (Python ↔ .NET)

pythonnet (`pythonnet` PyPI package, originating from 2003, maintained by the Python for .NET project) lets Python code call into .NET assemblies via CLR hosting:

```python
import clr
clr.AddReference("System.Windows.Forms")
from System.Windows.Forms import Form, Application

form = Form()
form.Text = "Hello from Python"
Application.Run(form)
```

pythonnet uses the CLR hosting API (`mono_jit_init` on Mono, `hostfxr` on .NET 5+) to embed the .NET runtime in the Python process. No wrapper assembly is required; pythonnet performs runtime reflection to discover types and methods, then dispatches via `MethodInfo.Invoke`.

**What it gets right.** Full .NET type system access via reflection. No wrapper code required on the .NET side. Every public type in every loaded assembly is immediately callable from Python.

**What it requires.** The user calls `clr.AddReference("AssemblyName")` to load each assembly. Beyond that, no boilerplate. The reflection-based dispatch is the "no boilerplate" mechanism.

**Trade-offs.** The reflection-based dispatch (`MethodInfo.Invoke`) incurs a per-call overhead of approximately 1-2 microseconds versus a direct native call via `[UnmanagedCallersOnly]`. For high-frequency calls, this is significant. pythonnet also does not produce a static, auditable, lockfile-pinnable surface: the Python code's behaviour changes if the .NET assembly changes without explicit versioning.

**MEP-68 divergence.** MEP-68 takes CLR hosting from pythonnet (the embedding strategy) but replaces reflection dispatch with `[UnmanagedCallersOnly]` function pointers (the zero-overhead dispatch strategy). MEP-68 also produces a static shim surface pinned in `mochi.lock`; pythonnet's reflection surface is dynamic and unaudited. The "what pythonnet gets right" is the CLR hosting approach; MEP-68 borrows it.

## IKVM.NET (JVM ↔ .NET)

IKVM.NET (originally by Jeroen Frijters, now maintained as a community fork) translates JVM bytecode (`.class` / `.jar`) to .NET MSIL and can also translate .NET MSIL to JVM bytecode. It has been used to run Java programs on the .NET CLR and to expose .NET libraries to Java programs.

```java
// Java calling .NET via IKVM
import cli.System.Console;
public class Hello {
    public static void main(String[] args) {
        Console.WriteLine("Hello from Java via IKVM");
    }
}
```

**What it gets right.** Bidirectional translation model. The user writes Java or .NET code naturally; IKVM handles the translation. No C glue layer needed.

**What it requires.** The IKVM toolchain (the translator, the stub assemblies) must be installed. The translation is lossy: IKVM cannot translate every .NET feature to JVM and vice versa (e.g., `unsafe` blocks, `stackalloc`, `Span<T>`, NativeAOT-only features).

**Trade-offs.** IKVM translates at the bytecode level, not at the source level. This works well for pure-managed code but fails for anything that touches native interop on either side. As of 2025, the community fork is maintained but has not kept pace with .NET 8/9 features.

**MEP-68 divergence.** IKVM's bidirectional IL translation is a different paradigm from MEP-68's CLR hosting + C# shim approach. MEP-68 does not translate bytecode; it calls into the .NET runtime via native function pointers. The lesson from IKVM is that bidirectional bridges are feasible and valuable; the lesson it does NOT teach MEP-68 is "translate bytecode."

## CsWin32 (Windows Win32 ↔ .NET)

CsWin32 (Microsoft, GA 2021, part of the `microsoft/CsWin32` GitHub repo) auto-generates P/Invoke declarations for the Windows Win32 API from machine-readable metadata (the `Windows.Win32.winmd` metadata file that ships with the Windows SDK):

```csharp
// Instead of hand-writing:
[DllImport("user32.dll")]
static extern int MessageBox(IntPtr hWnd, string text, string caption, int type);

// CsWin32 generates this automatically when you reference the NuGet package.
```

The user adds `NuGet: Microsoft.Windows.CsWin32`, annotates a `NativeMethods.txt` file with the function names they want, and CsWin32 generates the P/Invoke declarations at build time via a Roslyn source generator.

**What it gets right.** Machine-readable metadata (the `.winmd` file) as the authoritative source of Win32 API types and signatures. Automatic P/Invoke generation from the metadata. No hand-writing of `DllImport` attributes.

**What it requires.** The `NativeMethods.txt` file listing the functions to expose. This is minimal boilerplate (one function name per line).

**MEP-68 relationship.** CsWin32 is the closest prior art to MEP-68's shim generator in direction and spirit: both use machine-readable metadata as the authoritative surface source, both auto-generate callable stubs, both require minimal user input. The difference: CsWin32 generates P/Invoke stubs pointing to native Win32 DLLs; MEP-68 generates `[UnmanagedCallersOnly]` stubs pointing from native code into the CLR. The direction is reversed. MEP-68 borrows the "metadata as source of truth, auto-generate the glue" principle from CsWin32.

## ClangSharp (C headers ↔ .NET)

ClangSharp (Microsoft, part of the `dotnet/clangsharp` project) auto-generates P/Invoke declarations for C/C++ APIs by parsing C headers via `libclang`. It is used to generate .NET bindings for the Clang and LLVM C APIs.

```xml
<GenerateBindings Include="llvm-c/Core.h" />
```

After running ClangSharp, the user gets a complete set of P/Invoke declarations for every function in `Core.h`.

**What it gets right.** Automatic P/Invoke generation from C headers. No hand-writing required; the user provides a header, gets bindings. Handles pointer types, struct layouts, enums, and function pointers.

**What it requires.** The C header file as input. ClangSharp is a build-time code generator, not a runtime tool.

**MEP-68 relationship.** ClangSharp is the "C headers → .NET P/Invoke" direction; MEP-68 is the "CLR assembly → native function pointers" direction. They are mirror images. ClangSharp shows that auto-generation from a machine-readable description (C headers via libclang, assembly metadata via MetadataReader) is the right approach; MEP-68 takes this principle.

## uniffi (Rust ↔ Swift, Kotlin, Python, Go)

uniffi (Mozilla, GA 2021) generates language bindings from a `.udl` (UniFFI Definition Language) interface description file, producing Rust-side glue and host-side bindings for multiple languages. Although uniffi targets Rust-to-other, it is relevant because it represents the "explicit interface description" approach that MEP-68 rejects.

**What it gets right.** Multi-language fan-out from a single Rust library. Strong typing across the FFI boundary.

**What it requires.** The `.udl` file (authored by the Rust library developer) plus `uniffi_bindgen` invocation per host language. Three layers of configuration.

**MEP-68 divergence.** MEP-68 makes the interface description automatic (reading assembly metadata) rather than hand-authored (`.udl` file). The uniffi approach works when the library developer wants explicit control over the FFI surface; MEP-68 assumes the developer wrote a standard .NET library with no MEP-68-specific knowledge. "No boilerplate on the .NET side" is the invariant.

## GraalVM polyglot (JVM + JavaScript + other languages in one VM)

GraalVM (Oracle, GA 2019 for JVM + JavaScript) allows multiple languages to share object references in the same VM. A Java object can be passed to JavaScript, manipulated there, and returned to Java without serialisation.

**What it gets right.** True polyglot with shared heap. No marshalling across a native boundary.

**What it requires.** All participating languages must run inside GraalVM. A Mochi program would have to run inside GraalVM's JVM, which is not Mochi's execution model.

**MEP-68 divergence.** MEP-68 is a native bridge: Mochi runs as a native binary, the CLR runs as an in-process hosted runtime, and data crosses the native↔managed boundary via the `[UnmanagedCallersOnly]` surface. GraalVM's shared-heap model requires all participants to be inside one VM. MEP-68's model is more composable (Mochi can import multiple runtimes, not just one) and more portable (the CLR hosting API does not require GraalVM's substrate).

## JNI-style manual bridging (Java Native Interface)

JNI (Java, GA 1997) requires every native-to-JVM function to be manually declared with the `Java_<package>_<class>_<method>` naming convention:

```c
JNIEXPORT jstring JNICALL Java_com_example_Foo_greet(JNIEnv *env, jobject obj, jstring name) {
    const char *cname = (*env)->GetStringUTFChars(env, name, NULL);
    // ...
}
```

Every function, parameter type, and return type is hand-written. There is no auto-generation.

**What it gets right.** Production-grade, stable, widely understood.

**What it requires.** Every single bridge function must be manually authored. There is no discovery from class metadata; the JNI programmer must know the method signature.

**MEP-68 divergence.** MEP-68 is "JNI auto-generated from assembly metadata." The `mochi-dotnet-meta` tool reads the assembly metadata and the shim generator writes what a JNI programmer would have written by hand. The difference is scale: a JNI programmer can maintain 10-20 hand-written bridge functions; the bridge generates hundreds.

## SwiftUI + C headers (Apple cross-language via C ABI)

Apple's approach to Swift-to-C/C++ interop uses C headers as the lingua franca: a Swift library that wants to be callable from C or C++ exports a C header (`module.modulemap` + `.h`) via `@_cdecl("function_name")` attributes. The C consumer includes the header and calls as if the function were C.

**What it gets right.** The C ABI as the universal bridge surface. Works across Swift, C, C++, Objective-C.

**What it requires.** The Swift library author must annotate each function with `@_cdecl`. Boilerplate per function on the library side.

**MEP-68 divergence.** MEP-68's `[UnmanagedCallersOnly]` exports are the .NET analogue of Swift's `@_cdecl`. Both annotate a managed/high-level function to expose it with a C-ABI entry point. The difference is that MEP-68 generates the `[UnmanagedCallersOnly]` annotations automatically from assembly metadata; SwiftUI's `@_cdecl` requires manual annotation.

## diplomat (Unicode Consortium)

diplomat (used by ICU4X, GA 2022) generates bindings from Rust to C, C++, JavaScript, Dart, Kotlin by annotating Rust functions with `#[diplomat::bridge]`. It is designed for a Rust-side developer to expose a carefully curated subset of their API to multiple languages.

**What it gets right.** Multi-language fan-out. Precise control over the exposed surface.

**What it requires.** `#[diplomat::bridge]` annotation on every exposed item. The developer controls exactly what is exposed.

**MEP-68 divergence.** diplomat's "Rust author annotates items" model is not applicable to arbitrary NuGet packages. MEP-68 assumes the .NET library author did not know about Mochi and annotated nothing. The `mochi-dotnet-meta` tool discovers the surface from the compiled assembly; no .NET-side annotation is required. diplomat's precision is the right tool when you control both sides; MEP-68's auto-discovery is necessary when you do not.

## Kotlin/Native COM interop

Kotlin/Native's approach to calling Windows APIs uses COM (Component Object Model) interop: the user declares a `@CName`-annotated Kotlin interface that mirrors a COM interface, and the Kotlin/Native runtime dispatches via vtable. This is Windows-only (COM is Windows-specific).

**What it gets right.** Tight integration with Windows COM for the Windows-only use case.

**What it requires.** The COM interface must be declared manually. COM-aware type declarations are verbose.

**MEP-68 divergence.** MEP-68 explicitly rejects COM as the primary path (it is Windows-only; see [[02-design-philosophy]] §3). The CLR hosting approach works on all three platforms MEP-68 targets.

## The MEP-68 niche

Reading the landscape, every prior .NET bridge that is not MEP-68 requires either:

1. Runtime reflection dispatch (pythonnet): flexible but unauditable and with per-call overhead.
2. Bytecode translation (IKVM.NET): bidirectional but lossy.
3. Machine-readable metadata ingest with auto-generation (CsWin32, ClangSharp): the right model, but in the C→.NET direction, not the .NET→native direction.
4. Manual annotation on the library side (uniffi, diplomat, Swift `@_cdecl`): boilerplate per function.
5. Manual declaration on the consumer side (JNI, COM): boilerplate per function.
6. Shared-VM execution (GraalVM): not applicable to native Mochi.

MEP-68 occupies the "CsWin32/ClangSharp model but in reverse": auto-generate native-callable stubs from .NET assembly metadata. The assembly is the machine-readable metadata source (analogous to the `.winmd` file for CsWin32 and the C header for ClangSharp); the `[UnmanagedCallersOnly]` shim is the auto-generated glue (analogous to CsWin32's P/Invoke declarations and ClangSharp's P/Invoke bindings). The direction is reversed: instead of C→.NET, MEP-68 does .NET→native.

No prior bridge combines:
- Auto-generation from compiled assembly metadata (no .NET-side annotation required).
- CLR hosting as the runtime (not reflection dispatch, not bytecode translation).
- A static, lockfile-pinned, auditable shim surface.
- Cross-platform support (Linux, macOS, Windows).
- A NuGet trusted publishing path for the reverse direction.

## Lessons taken forward

- **From pythonnet**: CLR hosting (`hostfxr`) is the correct native .NET embedding mechanism. MEP-68 uses the same CLR hosting API.
- **From CsWin32**: machine-readable metadata as the authoritative source for auto-generated bindings is the right model. MEP-68 applies this model to ECMA-335 assembly metadata.
- **From ClangSharp**: the reverse direction (C header → .NET) confirms that auto-generation from metadata is feasible at scale. MEP-68 scales the same concept.
- **From IKVM.NET**: bidirectional bridges are achievable; the IL-translation approach is a dead end for modern .NET but the architecture goal (both directions in one tool) is correct.
- **From uniffi**: strict interface description separation ("what is the surface" vs "how do you call it") is the right architecture. In MEP-68, `mochi-dotnet-meta` is the "what is the surface" stage; the shim generator is the "how do you call it" stage.
- **From the supply-chain story**: NuGet trusted publishing (GA March 2024) is the only acceptable publish path in 2026.

## Cross-references

- [[02-design-philosophy]] for the rationale of the CLR hosting + `[UnmanagedCallersOnly]` choice.
- [[04-assembly-metadata-ingest]] for the `mochi-dotnet-meta` tool that replaces CsWin32's `.winmd` reader.
- [[09-abi-stability]] for the `[UnmanagedCallersOnly]` ABI details.
- [MEP-68 §Alternatives](/docs/mep/mep-0068#alternatives-considered) for the normative rejection list.
