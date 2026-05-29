// Package coroutines implements the MEP-70 Phase-13 coroutines bridge.
//
// Kotlin coroutines expose functions marked `suspend` whose return type is
// Continuation<T>. At the JVM call site, the kotlinx-coroutines runtime
// requires a CoroutineScope to dispatch them. The bridge synthesises two
// patterns for every suspend fn in the type-map:
//
//  1. BlockingDispatcher — calls runBlocking{} inside the GraalVM native
//     image C entry point. Blocks the calling Go goroutine until the
//     coroutine completes. Safe for low-concurrency and CLI use.
//
//  2. EventLoopDispatcher — maintains a per-artifact single-threaded event
//     loop (Dispatchers.Default on a fixed-thread pool). The C entry point
//     posts the coroutine and returns a handle; the caller polls via a
//     separate entry point until the result is ready.
//
// Both patterns are emitted as @CEntryPoint Java stubs that GraalVM compiles
// to the shared library. This file contains:
//   - DispatchMode enum
//   - SuspendFn descriptor (name, parameters, return type, dispatch mode)
//   - EmitBlockingStub / EmitEventLoopStub: generate the Java source fragments
//   - EmitCoroutinesHelper: generate the shared CoroutinesHelper.java file
package coroutines

import (
	"fmt"
	"strings"
)

// DispatchMode controls how a suspend fn is bridged.
type DispatchMode int

const (
	// Blocking dispatches via runBlocking{}: the C entry point blocks until done.
	Blocking DispatchMode = iota
	// EventLoop dispatches on a per-artifact event loop; C entry point returns
	// a future handle.
	EventLoop
)

// Param is a single function parameter.
type Param struct {
	Name     string
	JavaType string // Java type string, e.g. "String", "int", "long"
}

// SuspendFn describes one Kotlin suspend function to bridge.
type SuspendFn struct {
	// CName is the C entry point name (e.g. "mochi_okhttp_fetch_async").
	CName string
	// KotlinFQN is the fully-qualified Kotlin call expression
	// (e.g. "com.squareup.okhttp3.OkHttpClient.fetchAsync").
	KotlinFQN string
	// Params are the non-continuation parameters.
	Params []Param
	// ReturnJavaType is the Java return type (e.g. "String", "void").
	ReturnJavaType string
	// Mode selects the dispatch strategy.
	Mode DispatchMode
}

// EmitBlockingStub returns the Java @CEntryPoint source for a blocking-dispatch
// suspend function.
func EmitBlockingStub(fn SuspendFn) string {
	var b strings.Builder
	b.WriteString("    @CEntryPoint(name = \"")
	b.WriteString(fn.CName)
	b.WriteString("\")\n")
	b.WriteString("    @Uninterruptible(reason = \"Called from unmanaged code\")\n")
	fmt.Fprintf(&b, "    public static %s %s(IsolateThread thread",
		fn.ReturnJavaType, javaMethodName(fn.CName))
	for _, p := range fn.Params {
		fmt.Fprintf(&b, ", %s %s", p.JavaType, p.Name)
	}
	b.WriteString(") {\n")
	b.WriteString("        try {\n")
	indent := "            "
	callArgs := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		callArgs[i] = p.Name
	}
	call := fn.KotlinFQN + "(" + strings.Join(callArgs, ", ") + ")"
	if fn.ReturnJavaType == "void" {
		fmt.Fprintf(&b, "%skotlinx.coroutines.BuildersKt.runBlocking(\n", indent)
		fmt.Fprintf(&b, "%s    kotlinx.coroutines.EmptyCoroutineContext.INSTANCE,\n", indent)
		fmt.Fprintf(&b, "%s    (scope, continuation) -> { %s; return null; }\n", indent, call)
		b.WriteString(indent + ");\n")
	} else {
		fmt.Fprintf(&b, "%sreturn (%s) kotlinx.coroutines.BuildersKt.runBlocking(\n", indent, fn.ReturnJavaType)
		fmt.Fprintf(&b, "%s    kotlinx.coroutines.EmptyCoroutineContext.INSTANCE,\n", indent)
		fmt.Fprintf(&b, "%s    (scope, continuation) -> %s\n", indent, call)
		b.WriteString(indent + ");\n")
	}
	b.WriteString("        } catch (Exception e) {\n")
	b.WriteString("            throw new RuntimeException(e);\n")
	b.WriteString("        }\n")
	b.WriteString("    }\n")
	return b.String()
}

// EmitEventLoopStub returns the Java @CEntryPoint source for an event-loop-dispatch
// suspend function.  It generates two entry points:
//   - <cname>_submit(thread, ...params) → long handleId
//   - <cname>_poll(thread, handleId)   → returns result or throws if pending
func EmitEventLoopStub(fn SuspendFn) string {
	var b strings.Builder
	methodName := javaMethodName(fn.CName)
	callArgs := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		callArgs[i] = p.Name
	}
	call := fn.KotlinFQN + "(" + strings.Join(callArgs, ", ") + ")"

	// submit entry point
	b.WriteString("    @CEntryPoint(name = \"")
	b.WriteString(fn.CName)
	b.WriteString("_submit\")\n")
	b.WriteString("    @Uninterruptible(reason = \"Called from unmanaged code\")\n")
	fmt.Fprintf(&b, "    public static long %sSubmit(IsolateThread thread", methodName)
	for _, p := range fn.Params {
		fmt.Fprintf(&b, ", %s %s", p.JavaType, p.Name)
	}
	b.WriteString(") {\n")
	fmt.Fprintf(&b, "        return CoroutinesHelper.submit(() -> %s);\n", call)
	b.WriteString("    }\n\n")

	// poll entry point
	b.WriteString("    @CEntryPoint(name = \"")
	b.WriteString(fn.CName)
	b.WriteString("_poll\")\n")
	b.WriteString("    @Uninterruptible(reason = \"Called from unmanaged code\")\n")
	fmt.Fprintf(&b, "    public static %s %sPoll(IsolateThread thread, long handleId) {\n",
		fn.ReturnJavaType, methodName)
	b.WriteString("        return CoroutinesHelper.poll(handleId);\n")
	b.WriteString("    }\n")
	return b.String()
}

// EmitCoroutinesHelper generates the CoroutinesHelper.java source file that
// implements the event-loop submit/poll mechanism.
func EmitCoroutinesHelper(packageName string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "package %s;\n\n", packageName)
	b.WriteString(`import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;
import kotlinx.coroutines.GlobalScope;
import kotlinx.coroutines.Dispatchers;
import kotlinx.coroutines.future.FutureKt;

/**
 * CoroutinesHelper manages an event-loop handle registry for suspend function
 * results. Each submitted coroutine gets a unique handle ID; the caller polls
 * until the result is ready.
 */
public class CoroutinesHelper {
    private static final AtomicLong COUNTER = new AtomicLong(1);
    private static final ConcurrentHashMap<Long, CompletableFuture<Object>> FUTURES =
        new ConcurrentHashMap<>();

    /** Submit a suspend-fn supplier to the coroutines event loop.  Returns a handle ID. */
    @SuppressWarnings("unchecked")
    public static <T> long submit(Supplier<T> supplier) {
        long id = COUNTER.getAndIncrement();
        CompletableFuture<Object> future = new CompletableFuture<>();
        FutureKt.future(GlobalScope.INSTANCE, Dispatchers.getDefault(), null,
            (scope, continuation) -> {
                try {
                    future.complete((Object) supplier.get());
                } catch (Exception e) {
                    future.completeExceptionally(e);
                }
                return null;
            });
        FUTURES.put(id, future);
        return id;
    }

    /** Poll for the result of handle ID.  Returns the result if done, or throws PendingException. */
    @SuppressWarnings("unchecked")
    public static <T> T poll(long id) {
        CompletableFuture<Object> f = FUTURES.get(id);
        if (f == null) throw new IllegalArgumentException("unknown handle: " + id);
        if (!f.isDone()) throw new PendingException(id);
        FUTURES.remove(id);
        try {
            return (T) f.get();
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException(e);
        }
    }

    /** Thrown by poll() when the coroutine has not yet completed. */
    public static class PendingException extends RuntimeException {
        public final long handleId;
        public PendingException(long id) {
            super("coroutine " + id + " not yet complete");
            this.handleId = id;
        }
    }
}
`)
	return b.String()
}

// ShimLines returns the shim.mochi lines for a suspend fn in blocking mode:
//
//	extern fn <cname>(params...) : <mochiReturnType> from kotlin "<KotlinFQN>"
func ShimLines(fn SuspendFn, mochiReturnType string) string {
	var parts []string
	for _, p := range fn.Params {
		parts = append(parts, p.Name+": "+javaTypeToMochi(p.JavaType))
	}
	paramsStr := strings.Join(parts, ", ")
	if fn.Mode == EventLoop {
		// Event-loop mode exposes two extern fns per suspend fn.
		submitLine := fmt.Sprintf("extern fn %s_submit(%s): int from kotlin %q",
			fn.CName, paramsStr, fn.KotlinFQN)
		pollLine := fmt.Sprintf("extern fn %s_poll(handle: int): %s from kotlin %q",
			fn.CName, mochiReturnType, fn.KotlinFQN)
		return submitLine + "\n" + pollLine
	}
	return fmt.Sprintf("extern fn %s(%s): %s from kotlin %q",
		fn.CName, paramsStr, mochiReturnType, fn.KotlinFQN)
}

// ─── helpers ──────────────────────────────────────────────────────────────────

// javaMethodName converts a C-style snake_case name to camelCase Java method name.
func javaMethodName(cname string) string {
	parts := strings.Split(cname, "_")
	if len(parts) == 0 {
		return cname
	}
	var b strings.Builder
	for i, p := range parts {
		if i == 0 {
			b.WriteString(p)
		} else if len(p) > 0 {
			b.WriteString(strings.ToUpper(p[:1]))
			b.WriteString(p[1:])
		}
	}
	return b.String()
}

// javaTypeToMochi converts a Java type string to its Mochi equivalent.
func javaTypeToMochi(jt string) string {
	switch jt {
	case "int", "Integer":
		return "int"
	case "long", "Long":
		return "int"
	case "float", "Float":
		return "float"
	case "double", "Double":
		return "float"
	case "boolean", "Boolean":
		return "bool"
	case "String":
		return "string"
	case "void":
		return "unit"
	default:
		return "any"
	}
}
