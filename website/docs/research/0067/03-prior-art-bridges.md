---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "JNI, JNA (Java Native Access), GraalVM Polyglot API, JPype, py4j, Jython, Kotlin/JVM interop, Scala interop, swift-java, GraalVM native-image foreign, Panama API. What each gets right, what each requires the user to write, and what MEP-67 borrows."
---

# 03. Prior-art bridges

This note surveys every major JVM-to-other-language bridge, what it requires from the user, and what MEP-67 borrows from each.

## 1. JNI (Java Native Interface)

JNI is the foundational mechanism for all bidirectional Java-to-native interop. It is part of the Java specification and has been stable since JDK 1.1.

**What it requires**: The user writes a Java class with `native` method declarations, compiles it, runs `javah` (deprecated, superseded by `javac -h`) to generate a C header, implements the C functions, compiles the shared library, loads it via `System.loadLibrary`. On the "native calls Java" side, the user writes C code that calls `JNI_CreateJavaVM`, finds method IDs via `GetMethodID`, and invokes methods via `CallObjectMethod` / `CallIntMethod` / etc.

**What it gets right**: zero per-call overhead (beyond the JNI dispatch). Full access to the Java runtime. Zero additional dependencies. Ships with every JDK.

**What it gets wrong (from a user perspective)**: enormous boilerplate. The C code for a single method call is 10-20 lines of `FindClass`, `GetMethodID`, `NewObject`, `CallObjectMethod`, null-checking, and `ExceptionCheck`. String handling requires `GetStringUTFChars` / `ReleaseStringUTFChars` pairs. Memory management requires `DeleteLocalRef` / `DeleteGlobalRef` to prevent leaks.

**MEP-67 borrows**: the entire JNI call mechanism. The bridge generates the boilerplate automatically. The user never writes `GetMethodID` or `CallObjectMethod`.

## 2. JNA (Java Native Access)

JNA is a Java library that allows Java code to call native shared libraries without writing JNI code. The user writes a Java `interface` that extends `Library`, and JNA uses reflection to generate the native calls at runtime.

```java
public interface LibC extends Library {
    LibC INSTANCE = (LibC) Native.load("c", LibC.class);
    int strlen(String s);
}
// usage: LibC.INSTANCE.strlen("hello") == 5
```

**Direction**: Java calling native (C, Rust, Go). This is the opposite direction from MEP-67.

**What it gets right**: zero native code required on the user side. The Java interface is the full specification.

**What it gets wrong for MEP-67**: JNA is for Java-to-native, not native-to-Java. JNA cannot be used to call Java from Go. JNA also has runtime overhead (reflection-based dispatch for each call) that JNI avoids.

**MEP-67 borrows**: the "interface as specification" model. MEP-67's synthesised `shim.mochi` is the Mochi analog of JNA's Java interface: it specifies the surface, and the bridge fills in the call mechanism.

## 3. GraalVM Polyglot API

GraalVM's Polyglot API (`org.graalvm.polyglot`) allows multiple languages (Java, JavaScript, Python, Ruby, R, LLVM bitcode) to interoperate within a single GraalVM JVM. Programs written in different languages can share values directly without serialisation.

```java
try (Context context = Context.create()) {
    Value pythonFunc = context.eval("python", "lambda x: x * 2");
    int result = pythonFunc.execute(21).asInt(); // 42
}
```

**What it gets right**: true zero-overhead interop within the GraalVM JVM. Full bidirectional. No code generation required.

**What it gets wrong for MEP-67**: requires GraalVM (not stock OpenJDK). Mochi is not a GraalVM language (it does not run on the Truffle framework). The Polyglot API requires the host program to be a Java program running on GraalVM; a Go-built Mochi binary is not. GraalVM CE does not support all Java libraries (especially those that rely on runtime reflection or non-Truffle classloading).

**MEP-67 borrows**: the concept of a language-agnostic value type that bridges across the language boundary. MEP-67's opaque handle type (a `long` holding a JNI global reference address) is a simpler version of GraalVM's `Value`.

## 4. JPype

JPype is a Python library that starts a JVM inside the Python process and exposes Java classes as Python objects.

```python
import jpype
import jpype.imports
jpype.startJVM(classpath=["guava.jar"])
from com.google.common.collect import ImmutableList
lst = ImmutableList.of("a", "b", "c")
print(lst.get(0))  # "a"
```

**What it gets right**: no-boilerplate Java consumption from Python. The import mechanism (`jpype.imports`) allows `from com.google.guava import ImmutableList` which is remarkably close to MEP-67's `import java "com.google.guava:guava" as guava`. JVM startup is once per process. Type coercions between Python and Java primitives are automatic.

**What it gets wrong**: requires a running JVM (which JPype starts). No static type information (Python is dynamically typed; JPype does not generate stubs). Error messages from Java exceptions are often opaque in the Python trace. No publish path (JPype only handles consumption).

**MEP-67 borrows**: the "start a JVM once, import Java classes as host-language values" model. MEP-67's `JNI_CreateJavaVM` is structurally identical to JPype's `startJVM`. The key difference is that MEP-67 generates static type declarations (the `shim.mochi` `extern fn` corpus) for Mochi's type checker, while JPype relies on Python's dynamic dispatch.

## 5. py4j

py4j is a Python-to-Java bridge via a socket protocol. A Java program starts a `GatewayServer` on a local socket; a Python program connects to it via `py4j.java_gateway.JavaGateway`.

```python
from py4j.java_gateway import JavaGateway
gateway = JavaGateway()  # connects to GatewayServer on localhost:25333
ArrayList = gateway.jvm.java.util.ArrayList
lst = ArrayList()
lst.add("hello")
```

**What it gets right**: no JNI required. Works with any JVM (not just GraalVM). Python process remains separate from the JVM (crash isolation).

**What it gets wrong**: the GatewayServer is a separate Java process that the user must start and manage. Every method call crosses a socket boundary (JSON-serialised). Per-call latency is 1-10ms even on localhost. No static types.

**MEP-67 borrows**: nothing directly. py4j's subprocess model is exactly what MEP-67 rejects in favour of JNI embedding.

## 6. Jython

Jython is a Python implementation that runs on the JVM. Python programs run as JVM bytecode and can import Java classes directly.

```python
from java.util import ArrayList
lst = ArrayList()
lst.add("hello")
```

**What it gets right**: seamless Java import. Python programs can use Java libraries without any bridge configuration.

**What it gets wrong**: Jython is CPython-incompatible (no C extensions, no `numpy`, no `pandas`). Jython 2.7 (released 2017) was the last stable release until Jython 3.x work started in 2020s (still not GA as of 2026). Jython is fundamentally a Python-on-JVM implementation, not a bridge; it doesn't help Mochi programs consume Java.

**MEP-67 borrows**: the "import Java as if it were a native module" user experience. MEP-67's `import java "..."` is the Mochi analog of Jython's `from java.util import ArrayList`.

## 7. Kotlin/JVM interop

Kotlin is a JVM language designed with Java interop as a first-class feature. Kotlin classes and Java classes coexist in the same project with zero boilerplate.

```kotlin
import com.google.common.collect.ImmutableList
val list: ImmutableList<String> = ImmutableList.of("a", "b", "c")
println(list[0])  // "a"
```

**What it gets right**: zero-boilerplate Java consumption. Full type safety (Kotlin's type system handles Java's platform types via nullability annotations). Bidirectional: Java can call Kotlin and vice versa.

**What it gets wrong for MEP-67**: Kotlin and Java both compile to JVM bytecode and run on the same JVM. MEP-67 is bridging from a native binary (compiled by the MEP-53/54 pipeline) to the JVM. Kotlin interop cannot help here; Mochi is not a JVM language.

**MEP-67 borrows**: the aspiration. Kotlin's Java interop is the benchmark for "what zero-boilerplate looks like." MEP-67 aims for a comparable experience: `import java "com.google.guava:guava"` should feel as natural as Kotlin's `import com.google.common.collect.ImmutableList`.

## 8. Scala interop

Scala runs on the JVM and interops with Java bidirectionally. Scala's collection library has explicit conversion functions between Scala and Java collections:

```scala
import scala.jdk.CollectionConverters._
val javaList: java.util.List[String] = List("a", "b", "c").asJava
```

**What it gets right**: typed, bidirectional, zero-boilerplate from the user's perspective.

**What it gets wrong for MEP-67**: same as Kotlin. Scala compiles to JVM bytecode. MEP-67 bridges from native code.

**MEP-67 borrows**: the collection conversion model. MEP-67's type-mapping table converts `java.util.List<T>` to Mochi `list<T>` at the JNI boundary, analogous to Scala's `.asScala` conversion.

## 9. swift-java

swift-java (Apple's open-source project, announced WWDC 2024, in development through 2025-2026) generates Swift bindings for Java libraries. The user runs a `swift-java` command-line tool pointing at a JAR; it generates Swift source files with wrapper types.

```swift
import JavaKit
let list = ArrayList<String>()
list.add("hello")
```

**What it gets right**: static type generation from JARs. No-boilerplate consumption from Swift. The generated Swift types mirror the Java class hierarchy.

**What it gets wrong (from MEP-67's perspective)**: the generated Swift source files are verbose (hundreds of lines per class). The tool requires the user to run it separately and commit the generated files. Updating to a new JAR version requires regenerating and recommitting. The generated types are JVM-specific (they use the `JavaKit` runtime that embeds a JVM the same way JNI_CreateJavaVM does).

**MEP-67 borrows**: the "generate static wrapper types from a JAR" model. MEP-67's `mochi pkg lock` step (which runs ReflectTool and synthesises `MochiWrap.java` plus `shim.mochi`) is structurally identical to `swift-java generate`. The key difference is that MEP-67 integrates the generation step into the standard lock/build lifecycle instead of requiring a separate step.

## 10. GraalVM native-image foreign

GraalVM native-image supports "foreign memory access" (based on the Panama MemoryLayout API) and "foreign function calls" (calls to C functions from native-image-compiled code).

**Direction**: native-image compiled code calling C (via the Panama FFI). Not native code calling Java.

**MEP-67 borrows**: the idea of treating the foreign function table as a generated artifact. MEP-67's `MochiWrap.java` is conceptually similar to native-image's native-method registration configuration.

## 11. Panama API (Foreign Function and Memory API)

Project Panama (finalized as `java.lang.foreign` in Java 22, previewed in Java 19-21) provides Java with direct access to native memory and C functions without JNI.

```java
// Java calling a C function without JNI (Java 22+)
try (Arena arena = Arena.ofConfined()) {
    MemorySegment cStr = arena.allocateUtf8String("hello");
    long len = (long) strlen.invokeExact(cStr);
}
```

**Direction**: Java calling native C. This is the opposite direction from MEP-67 (native Go calling Java).

**What it gets right**: eliminates JNI boilerplate for the Java-calls-C direction. `jextract` tool generates Java bindings from C headers, analogous to what MEP-67's ReflectTool does for the other direction.

**MEP-67 borrows**: the concept of `jextract` (automatically generating language bindings from a foreign API description). MEP-67's ReflectTool is the JVM analog of `jextract` operating in the other direction.

## Summary table

| Bridge | Direction | Boilerplate required | JVM embedding | Static types | MEP-67 borrows |
|--------|-----------|---------------------|---------------|--------------|----------------|
| JNI | Both | High | Yes (JNI_CreateJavaVM) | Yes (hand-written) | Call mechanism |
| JNA | Java → native | Low | No | No | "interface as spec" model |
| GraalVM Polyglot | Both (GraalVM only) | Zero | Yes (GraalVM) | Partial | Value type concept |
| JPype | Python ← Java | Low | Yes (startJVM) | No | JVM-once model |
| py4j | Python ← Java | Medium (server process) | No (subprocess) | No | Nothing (rejected model) |
| Jython | Python is JVM | Zero | Yes (JVM is Jython) | No | Import UX |
| Kotlin interop | JVM-to-JVM | Zero | Shared JVM | Yes | Aspiration benchmark |
| Scala interop | JVM-to-JVM | Low | Shared JVM | Yes | Collection conversion |
| swift-java | Swift ← Java | Low (generated files) | Yes (JavaKit) | Yes | Static generation model |
| GraalVM native-image | native → C | None (jextract) | No | Yes (generated) | jextract concept |
| Panama API | Java → native | Low (jextract) | No | Yes | jextract concept |
