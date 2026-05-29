// Package asyncbridge handles Task<T> methods in the C# shim.
// It provides detection of async shimgen.ShimMethod entries and emits
// enhanced C# wrapper bodies that avoid deadlocks in single-threaded contexts.
package asyncbridge

import (
	"fmt"
	"strings"

	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

// DispatchMode controls how async .NET methods are bridged to synchronous Mochi calls.
type DispatchMode string

const (
	// DispatchGetAwaiter uses .GetAwaiter().GetResult() directly.
	// Simple, but can deadlock if the method captures a SynchronizationContext.
	DispatchGetAwaiter DispatchMode = "get-awaiter"
	// DispatchManualReset uses ManualResetEventSlim for deadlock-safe blocking.
	// Recommended for UI-context and ASP.NET-hosted environments.
	DispatchManualReset DispatchMode = "manual-reset"
	// DispatchTaskRun wraps in Task.Run(...).GetAwaiter().GetResult() to
	// escape any captured SynchronizationContext. Adds a thread-pool hop.
	DispatchTaskRun DispatchMode = "task-run"
)

// Config holds the async bridge configuration.
type Config struct {
	Mode DispatchMode
}

// DefaultConfig returns the default async bridge configuration.
func DefaultConfig() Config {
	return Config{Mode: DispatchGetAwaiter}
}

// EmitAsyncWrapper emits the C# wrapper body for an async method using the configured mode.
// Returns the C# source lines for the method body (no surrounding braces).
// callExpr is the upstream call expression (without .GetAwaiter etc.).
// returnKind is the Mochi Kind of the return type.
func EmitAsyncWrapper(callExpr string, returnKind typemap.Kind, mode DispatchMode) string {
	isVoid := returnKind == typemap.KindUnit

	switch mode {
	case DispatchGetAwaiter:
		if isVoid {
			return fmt.Sprintf("            %s.GetAwaiter().GetResult();", callExpr)
		}
		return fmt.Sprintf("            return %s.GetAwaiter().GetResult();", callExpr)

	case DispatchManualReset:
		return emitManualReset(callExpr, returnKind, isVoid)

	case DispatchTaskRun:
		if isVoid {
			return fmt.Sprintf("            Task.Run(() => %s).GetAwaiter().GetResult();", callExpr)
		}
		return fmt.Sprintf("            return Task.Run(() => %s).GetAwaiter().GetResult();", callExpr)

	default:
		// Fall back to GetAwaiter for unknown modes.
		if isVoid {
			return fmt.Sprintf("            %s.GetAwaiter().GetResult();", callExpr)
		}
		return fmt.Sprintf("            return %s.GetAwaiter().GetResult();", callExpr)
	}
}

// emitManualReset builds the ManualResetEventSlim-based wrapper body.
func emitManualReset(callExpr string, returnKind typemap.Kind, isVoid bool) string {
	var b strings.Builder
	b.WriteString("            var __mre = new ManualResetEventSlim(false);\n")
	b.WriteString("            Exception? __ex = null;\n")
	if !isVoid {
		csReturnType := kindToCSharpType(returnKind)
		fmt.Fprintf(&b, "            %s __result = default!;\n", csReturnType)
		b.WriteString("            Task.Run(async () => {\n")
		fmt.Fprintf(&b, "                try { __result = await %s.ConfigureAwait(false); }\n", callExpr)
		b.WriteString("                catch (Exception e) { __ex = e; }\n")
		b.WriteString("                finally { __mre.Set(); }\n")
		b.WriteString("            });\n")
		b.WriteString("            __mre.Wait();\n")
		b.WriteString("            if (__ex != null) System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(__ex).Throw();\n")
		b.WriteString("            return __result;")
	} else {
		b.WriteString("            Task.Run(async () => {\n")
		fmt.Fprintf(&b, "                try { await %s.ConfigureAwait(false); }\n", callExpr)
		b.WriteString("                catch (Exception e) { __ex = e; }\n")
		b.WriteString("                finally { __mre.Set(); }\n")
		b.WriteString("            });\n")
		b.WriteString("            __mre.Wait();\n")
		b.WriteString("            if (__ex != null) System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(__ex).Throw();")
	}
	return b.String()
}

// kindToCSharpType maps a typemap.Kind to an approximate C# type name for use
// in local variable declarations in the generated wrapper body.
func kindToCSharpType(k typemap.Kind) string {
	switch k {
	case typemap.KindBool:
		return "bool"
	case typemap.KindByte:
		return "byte"
	case typemap.KindInt:
		return "int"
	case typemap.KindInt64:
		return "long"
	case typemap.KindUInt:
		return "uint"
	case typemap.KindUInt64:
		return "ulong"
	case typemap.KindFloat:
		return "float"
	case typemap.KindFloat64:
		return "double"
	case typemap.KindChar:
		return "char"
	case typemap.KindString:
		return "string"
	case typemap.KindUnit:
		return "void"
	default:
		return "object"
	}
}

// IsAsyncMethod reports whether a shimgen.ShimMethod is async (IsAsync == true).
func IsAsyncMethod(m *shimgen.ShimMethod) bool {
	if m == nil {
		return false
	}
	return m.IsAsync
}

// AsyncMethodCount counts async methods in a shimgen.Shim.
func AsyncMethodCount(s *shimgen.Shim) int {
	if s == nil {
		return 0
	}
	n := 0
	for i := range s.Methods {
		if s.Methods[i].IsAsync {
			n++
		}
	}
	return n
}

// AsyncRewrite pairs a shimgen.ShimMethod with its rewritten C# body for the
// configured DispatchMode.
type AsyncRewrite struct {
	Method     shimgen.ShimMethod
	CSharpBody string
}

// RewriteForMode rewrites the async method bodies in a *shimgen.Shim to use
// the specified DispatchMode instead of the default GetAwaiter.
// Returns a slice of AsyncRewrite, one per async method in the shim.
// Non-async methods are not included.
func RewriteForMode(s *shimgen.Shim, mode DispatchMode) []AsyncRewrite {
	if s == nil {
		return nil
	}
	var out []AsyncRewrite
	for _, m := range s.Methods {
		if !m.IsAsync {
			continue
		}
		callExpr := m.UpstreamCall + "(" + buildArgList(m) + ")"
		var retKind typemap.Kind
		if m.Return != nil {
			retKind = m.Return.Kind
		} else {
			retKind = typemap.KindUnit
		}
		body := EmitAsyncWrapper(callExpr, retKind, mode)
		out = append(out, AsyncRewrite{Method: m, CSharpBody: body})
	}
	return out
}

// buildArgList builds a comma-separated argument list from a ShimMethod's parameters.
func buildArgList(m shimgen.ShimMethod) string {
	args := make([]string, 0, len(m.Params))
	for _, p := range m.Params {
		args = append(args, p.Name)
	}
	return strings.Join(args, ", ")
}

// EmitAsyncRuntimeCS emits an optional AsyncRuntime.cs helper for ManualReset
// and TaskRun modes. For DispatchGetAwaiter, returns empty string (no helper needed).
func EmitAsyncRuntimeCS(mode DispatchMode) string {
	switch mode {
	case DispatchGetAwaiter:
		return ""
	case DispatchManualReset:
		return `// AsyncRuntime.cs -- generated by mochi/package3/dotnet/asyncbridge; do not edit.
// Provides the ManualResetEventSlim-based async dispatch helper.
using System;
using System.Threading;
using System.Threading.Tasks;

namespace MochiShim.AsyncRuntime
{
    /// <summary>
    /// Provides helpers for deadlock-safe blocking on async Task methods.
    /// </summary>
    internal static class AsyncRuntime
    {
        /// <summary>
        /// Blocks synchronously on a Task using ManualResetEventSlim to avoid
        /// SynchronizationContext deadlocks.
        /// </summary>
        internal static T BlockOn<T>(Func<Task<T>> factory)
        {
            var mre = new ManualResetEventSlim(false);
            T result = default!;
            Exception? ex = null;
            Task.Run(async () => {
                try { result = await factory().ConfigureAwait(false); }
                catch (Exception e) { ex = e; }
                finally { mre.Set(); }
            });
            mre.Wait();
            if (ex != null) System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw();
            return result!;
        }

        /// <summary>
        /// Void variant of BlockOn.
        /// </summary>
        internal static void BlockOnVoid(Func<Task> factory)
        {
            var mre = new ManualResetEventSlim(false);
            Exception? ex = null;
            Task.Run(async () => {
                try { await factory().ConfigureAwait(false); }
                catch (Exception e) { ex = e; }
                finally { mre.Set(); }
            });
            mre.Wait();
            if (ex != null) System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(ex).Throw();
        }
    }
}
`
	case DispatchTaskRun:
		return `// AsyncRuntime.cs -- generated by mochi/package3/dotnet/asyncbridge; do not edit.
// Provides the Task.Run-based async dispatch helper (thread-pool hop).
using System;
using System.Threading.Tasks;

namespace MochiShim.AsyncRuntime
{
    /// <summary>
    /// Provides Task.Run-based blocking helpers that escape any captured
    /// SynchronizationContext by hopping to the thread pool.
    /// </summary>
    internal static class AsyncRuntime
    {
        /// <summary>
        /// Runs the async factory on the thread pool and blocks until complete.
        /// </summary>
        internal static T BlockOn<T>(Func<Task<T>> factory)
            => Task.Run(factory).GetAwaiter().GetResult();

        /// <summary>
        /// Void variant of BlockOn.
        /// </summary>
        internal static void BlockOnVoid(Func<Task> factory)
            => Task.Run(factory).GetAwaiter().GetResult();
    }
}
`
	default:
		return ""
	}
}
