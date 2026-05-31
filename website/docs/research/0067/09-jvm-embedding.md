---
title: "09. JVM embedding"
sidebar_position: 10
sidebar_label: "09. JVM embedding"
description: "JNI_CreateJavaVM lifecycle, classpath construction, JVM options passthrough, JVM version negotiation (Java 17+ minimum, Java 21 recommended), single-JVM-per-process constraint, GC pressure and heap sizing, exception handling across the JNI boundary, AttachCurrentThread for non-Java-created threads, and finaliser and GC interaction with JNI global refs."
---

# 09. JVM embedding

This note covers the technical details of embedding a JVM into the Mochi native process via `JNI_CreateJavaVM`.

## 1. JNI_CreateJavaVM overview

`JNI_CreateJavaVM` is a C function exported by the JVM shared library (`libjvm.so` on Linux, `libjvm.dylib` on macOS, `jvm.dll` on Windows). It creates a JVM instance inside the calling process and returns a `JavaVM*` (the VM handle) and a `JNIEnv*` (the environment for the calling thread).

```c
// JNI specification signature
jint JNI_CreateJavaVM(JavaVM **p_vm, void **p_env, void *vm_args);
```

The `vm_args` is a `JavaVMInitArgs` struct:

```c
JavaVMInitArgs vm_args;
vm_args.version = JNI_VERSION_21;  // or JNI_VERSION_17, JNI_VERSION_11
vm_args.nOptions = num_options;
vm_args.options = java_vm_options; // array of JavaVMOption
vm_args.ignoreUnrecognized = JNI_FALSE;
```

Each `JavaVMOption` has a `optionString` field (a C string) and an `extraInfo` pointer (used for advanced options; typically null).

## 2. Locating the JVM shared library

The bridge must dlopen the JVM shared library at runtime (not link against it at compile time, to avoid hard-coding the JDK path). The discovery algorithm:

1. If `[java] java-home` is set in `mochi.toml`, look for `<java-home>/lib/server/libjvm.so` (Linux), `<java-home>/lib/server/libjvm.dylib` (macOS).
2. Otherwise, run `java -XshowSettings:all -version 2>&1` to parse the `java.home` property.
3. On macOS: `/usr/libexec/java_home -v 17+` returns the JDK home.
4. On Linux: `alternatives --config java` or parsing `/etc/alternatives/java`.
5. On Windows: HKEY_LOCAL_MACHINE\SOFTWARE\JavaSoft\Java Runtime Environment registry key.

The bridge caches the discovered JVM path in `~/.cache/mochi/java-jvm-path` and reuses it across lock invocations.

## 3. Classpath construction

The `-Djava.class.path=` JVM option is the primary mechanism for making JARs available to the embedded JVM. The bridge constructs the classpath as:

```
~/.cache/mochi/java-deps/<sha256>/jar  (for each [[java-package]] in mochi.lock)
: <workdir>/java_wrap/<pkg>/MochiWrap.jar  (for each imported JAR)
```

The classpath separator is `:` on POSIX and `;` on Windows. The bridge uses `filepath.ListSeparator` from the Go standard library.

On Java 9+, the classpath is supplemented by module path entries when JPMS modules are present. The bridge adds `--module-path <classpath>` when any JAR in the dependency graph contains a `module-info.class`. However, most Maven Central JARs are distributed as automatic modules (no `module-info.class`); the classpath is sufficient for these.

## 4. JVM version negotiation

The bridge requires Java 17+ (the current LTS as of May 2026, EOL September 2026). Java 21+ is recommended for virtual threads in the async bridge. The version check:

```go
func checkJVMVersion(env *C.JNIEnv) error {
    // Call System.getProperty("java.version") via JNI
    version := callStaticStringMethod(env, "java/lang/System", "getProperty", "(Ljava/lang/String;)Ljava/lang/String;", "java.version")
    major := parseMajorVersion(version)  // e.g., "21.0.2" -> 21
    if major < 17 {
        return fmt.Errorf("Java %d is not supported; MEP-67 requires Java 17+", major)
    }
    if major >= 21 {
        // Enable virtual thread pool for async bridge
        useVirtualThreads = true
    }
    return nil
}
```

## 5. Single-JVM-per-process constraint

The JNI specification (JDK 1.1 and later) states:

> "In JDK/JRE 1.2, creation of multiple VMs in a single process is not supported."

This constraint was never relaxed in subsequent releases. `JNI_CreateJavaVM` returns `JNI_ERR` (`-1`) if called a second time in the same process.

The bridge handles this by calling `JNI_GetCreatedJavaVMs` before `JNI_CreateJavaVM`:

```c
jsize nVMs;
JNI_GetCreatedJavaVMs(NULL, 0, &nVMs);
if (nVMs > 0) {
    // Reuse existing JVM
    JavaVM* existing_vms[1];
    JNI_GetCreatedJavaVMs(existing_vms, 1, &nVMs);
    vm = existing_vms[0];
    (*vm)->GetEnv(vm, (void**)&env, JNI_VERSION_21);
} else {
    // Create new JVM
    JNI_CreateJavaVM(&vm, (void**)&env, &vm_args);
}
```

If an existing JVM is found (e.g., because a third-party native library in the Mochi binary also embedded a JVM), the bridge reuses it. This creates a risk: the existing JVM's classpath may not include the JARs the bridge needs. The bridge handles this by dynamically adding JARs via a URLClassLoader:

```java
// Invoked via JNI after attaching to the existing JVM
URLClassLoader systemLoader = (URLClassLoader) ClassLoader.getSystemClassLoader();
Method addURL = URLClassLoader.class.getDeclaredMethod("addURL", URL.class);
addURL.setAccessible(true);
for (String jar : missingJars) {
    addURL.invoke(systemLoader, new File(jar).toURI().toURL());
}
```

Note: `addURL` on `URLClassLoader` was removed in Java 9 for the system class loader (which is no longer a `URLClassLoader` in Java 9+). On Java 9+, the bridge uses the `Instrumentation` API or a custom class loader hierarchy instead. This is a known complexity; the full solution is deferred to a future phase.

## 6. AttachCurrentThread for non-JVM-created threads

Go goroutines are not created by the JVM; they run on Go's M:N thread model (multiple goroutines multiplexed over OS threads). JNI calls from a goroutine require the OS thread to be attached to the JVM first.

The naive approach (attach/detach per JNI call):

```c
JNIEnv* env;
jint attach_result = (*vm)->AttachCurrentThread(vm, (void**)&env, NULL);
// ... JNI calls ...
(*vm)->DetachCurrentThread(vm);
```

This is correct but slow: `AttachCurrentThread` / `DetachCurrentThread` have measurable overhead (~1-10 microseconds). For tight loops that call Java methods, this overhead dominates.

The bridge uses a thread-local storage (TLS) approach: each OS thread that calls a JNI function gets attached on first call and remembered in TLS. Detachment happens when the OS thread exits (via a destructor registered with `pthread_key_create` on POSIX). This is the same approach JPype and py4j-JVM use.

```c
// Thread-local JNIEnv
static pthread_key_t tls_env_key;
static pthread_once_t tls_key_once = PTHREAD_ONCE_INIT;

static void init_tls_key() {
    pthread_key_create(&tls_env_key, detach_jni_env_destructor);
}

JNIEnv* get_env() {
    pthread_once(&tls_key_once, init_tls_key);
    JNIEnv* env = (JNIEnv*)pthread_getspecific(tls_env_key);
    if (env == NULL) {
        (*global_vm)->AttachCurrentThread(global_vm, (void**)&env, NULL);
        pthread_setspecific(tls_env_key, env);
    }
    return env;
}
```

## 7. Exception handling across the JNI boundary

JNI does not automatically propagate Java exceptions to C. The C code must explicitly check for pending exceptions after each JNI call:

```c
jobject result = (*env)->CallObjectMethod(env, obj, methodID, args...);
if ((*env)->ExceptionCheck(env)) {
    jthrowable ex = (*env)->ExceptionOccurred(env);
    (*env)->ExceptionClear(env);
    // Convert Java exception to Mochi error
    char* message = jthrowable_to_message(env, ex);
    (*env)->DeleteLocalRef(env, ex);
    return mochi_error(message);
}
```

`ExceptionCheck()` is cheaper than `ExceptionOccurred()` (no new local reference); the bridge uses `ExceptionCheck()` for the fast path and `ExceptionOccurred()` only when an exception is present (to extract the message).

The bridge wraps every JNI method call site with exception checking. This is generated automatically by the wrapper synthesiser; the user never writes `ExceptionCheck()`.

## 8. JNI global references and memory management

Every Java object returned across the JNI boundary exists as a JNI local reference in the current frame or a JNI global reference. Local references are automatically freed when the current JNI frame exits (i.e., when the JNI call returns to Java). But objects that need to live longer (across multiple JNI calls, across thread boundaries) must be promoted to global references:

```c
jobject globalRef = (*env)->NewGlobalRef(env, localRef);
(*env)->DeleteLocalRef(env, localRef);  // release the local ref
```

The bridge promotes every opaque handle returned to the Mochi side to a global reference. The handle value is the `jobject` address cast to `jlong`, stored in the Mochi side as an opaque integer. When the Mochi GC finalises the opaque handle value (via Mochi's finaliser mechanism), it calls the corresponding `_free` function in the synthesised shim:

```mochi
extern fn guava_ImmutableList_free(self: guava_ImmutableList) from java "..."
```

The `_free` function calls `DeleteGlobalRef` in the JNI bridge:

```c
void mochi_guava_ImmutableList_free(jlong handle) {
    jobject obj = (jobject)handle;
    JNIEnv* env = get_env();
    (*env)->DeleteGlobalRef(env, obj);
}
```

If the Mochi side does not call `_free` (e.g., due to a bug or a missing finaliser), the global reference leaks and the Java GC cannot collect the object. The bridge generates a finaliser in the Mochi shim using Mochi's `defer` or destructor mechanism (the exact Mochi API depends on the finaliser spec, which is a MEP-67 dependency on the Mochi language spec).

## 9. GC pressure and heap sizing

The embedded JVM shares the process address space with the Go runtime, cgo stack, and the Mochi native code. The JVM's heap competes with Go's heap for memory.

Default JVM heap sizing (`-Xms` / `-Xmx`):

- `-Xms`: initial heap size. JVM default: 1/64 of physical RAM (e.g., 128MB on a 8GB machine). The bridge does not change this.
- `-Xmx`: maximum heap size. JVM default: 1/4 of physical RAM (e.g., 2GB on 8GB). The bridge sets `-Xmx256m` by default (overridable via `[java] jvm-options = ["-Xmx512m"]`).

The JVM's GC runs concurrently (G1GC default since Java 9, ZGC and Shenandoah available). GC pause times compete with Go's GC pauses. On latency-sensitive workloads, enabling ZGC (`-XX:+UseZGC`) reduces pauses at the cost of higher CPU overhead.
