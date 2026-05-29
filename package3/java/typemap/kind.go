// Package typemap maps Java type names to Mochi kinds and FFI representations.
package typemap

// Kind is the Mochi type category for a mapped Java type.
type Kind int

const (
	// KindUnknown is the zero value; it should never appear in a valid mapping.
	KindUnknown Kind = iota
	// KindBool: boolean / java.lang.Boolean.
	KindBool
	// KindInt: int, short, byte, char, and their boxed counterparts.
	KindInt
	// KindLong: long / java.lang.Long.
	KindLong
	// KindFloat: float / java.lang.Float.
	KindFloat
	// KindDouble: double / java.lang.Double.
	KindDouble
	// KindString: java.lang.String.
	KindString
	// KindVoid: void return type.
	KindVoid
	// KindBytes: byte[] (raw binary data).
	KindBytes
	// KindList: java.util.List<T>, java.util.ArrayList<T>, or primitive arrays.
	KindList
	// KindMap: java.util.Map<K,V>, java.util.HashMap<K,V>.
	KindMap
	// KindOptional: java.util.Optional<T>.
	KindOptional
	// KindFuture: java.util.concurrent.CompletableFuture<T>.
	KindFuture
	// KindHandle: opaque JNI handle (non-scalar class not otherwise mapped).
	KindHandle
	// KindEnum: Java enum class.
	KindEnum
	// KindRecord: Java record class (simple value object).
	KindRecord
)

// String renders the Kind as a short token.
func (k Kind) String() string {
	switch k {
	case KindBool:
		return "bool"
	case KindInt:
		return "int"
	case KindLong:
		return "long"
	case KindFloat:
		return "float"
	case KindDouble:
		return "double"
	case KindString:
		return "string"
	case KindVoid:
		return "void"
	case KindBytes:
		return "bytes"
	case KindList:
		return "list"
	case KindMap:
		return "map"
	case KindOptional:
		return "optional"
	case KindFuture:
		return "future"
	case KindHandle:
		return "handle"
	case KindEnum:
		return "enum"
	case KindRecord:
		return "record"
	default:
		return "unknown"
	}
}
