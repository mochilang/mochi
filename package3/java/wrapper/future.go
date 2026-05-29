package wrapper

import (
	"fmt"
	"strings"

	"mochi/package3/java/typemap"
)

// FutureWrapMethod holds the generated Java source info for bridging a
// CompletableFuture-returning method to a Go callback via JNI.
type FutureWrapMethod struct {
	// ClassName is the Java class the method belongs to.
	ClassName string
	// OrigName is the original Java method name.
	OrigName string
	// WrapName is the synthesised static JNI method name.
	WrapName string
	// ElemMapping is the type mapping for the future's element type.
	ElemMapping *typemap.Mapping
}

// EmitFutureWrapper generates the Java source for a static bridge method
// that calls the original CompletableFuture-returning method and wires
// the resolution/rejection callbacks to a long-typed Go callback handle.
//
// Generated pattern:
//
//	public static void WrapName(long __handle, long __cbHandle) {
//	    Object obj = MochiRuntime.deref(__handle);
//	    ((OrigClass)obj).origMethod().thenAccept(v ->
//	        MochiRuntime.callback(__cbHandle, /* serialize */ v));
//	}
//
// The Go side registers a callback handle that receives the serialised value.
func EmitFutureWrapper(m FutureWrapMethod) string {
	var b strings.Builder
	fmt.Fprintf(&b, "    // async bridge for %s.%s() -> CompletableFuture\n", m.ClassName, m.OrigName)
	fmt.Fprintf(&b, "    public static void %s(long __handle, long __cbHandle) {\n", m.WrapName)
	fmt.Fprintf(&b, "        Object obj = MochiRuntime.deref(__handle);\n")
	fmt.Fprintf(&b, "        ((%s)obj).%s().thenAccept(v -> {\n", m.ClassName, m.OrigName)
	fmt.Fprintf(&b, "            MochiRuntime.callback(__cbHandle, %s);\n", serializeExpr(m.ElemMapping, "v"))
	fmt.Fprintf(&b, "        });\n")
	fmt.Fprintf(&b, "    }\n")
	return b.String()
}

// serializeExpr returns the Java expression that serialises the resolved
// future value v into the form expected by MochiRuntime.callback.
func serializeExpr(m *typemap.Mapping, varName string) string {
	if m == nil {
		return varName
	}
	switch m.Kind {
	case typemap.KindString:
		return varName
	case typemap.KindInt, typemap.KindLong, typemap.KindBool, typemap.KindFloat, typemap.KindDouble:
		return fmt.Sprintf("String.valueOf(%s)", varName)
	default:
		// For complex types, serialize to JSON string (runtime unmarshals).
		return fmt.Sprintf("String.valueOf(%s)", varName)
	}
}
