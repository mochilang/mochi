// Package typemap is the closed type-mapping table for MEP-68. It consumes
// metacli.TypeRef values (the output of the assembly metadata parser) and
// either returns a structured Mapping describing the Mochi-side type and FFI
// representation, or an errors.SkipReason explaining why no mapping exists.
//
// The table is "closed": every CLR TypeRef kind either has a documented
// mapping rule or a documented refusal class. There is no fallthrough that
// silently produces approximated types.
package typemap

// Kind is the high-level Mochi type a CLR type lowers to.
type Kind int

const (
	// KindUnknown is the zero value; a Mapping with KindUnknown is a
	// programming error and must never escape the package.
	KindUnknown Kind = iota
	KindBool
	KindByte
	KindInt    // int (32-bit, widened to int in Mochi)
	KindInt64
	KindUInt
	KindUInt64
	KindFloat   // float32, widened to float in Mochi
	KindFloat64
	KindChar
	KindString
	KindBytes   // byte[] raw byte slice
	KindUnit    // void / non-generic Task
	KindList    // List<T>, IEnumerable<T>, T[]
	KindMap     // Dictionary<K,V>
	KindSet     // HashSet<T>
	KindOption  // T? / Nullable<T>
	KindTask    // Task<T> async return
	KindTuple   // ValueTuple<T1,T2,...>
	KindRecord  // struct pinned-boxed across the boundary
	KindHandle  // opaque class/interface handle
	KindEnum    // CLR enum mapped to Mochi int enum
)

// String returns the Mochi type name for display and extern emit.
func (k Kind) String() string {
	switch k {
	case KindBool:
		return "bool"
	case KindByte:
		return "byte"
	case KindInt:
		return "int"
	case KindInt64:
		return "int64"
	case KindUInt:
		return "uint"
	case KindUInt64:
		return "uint64"
	case KindFloat:
		return "float"
	case KindFloat64:
		return "float64"
	case KindChar:
		return "char"
	case KindString:
		return "string"
	case KindBytes:
		return "bytes"
	case KindUnit:
		return "unit"
	case KindList:
		return "list"
	case KindMap:
		return "map"
	case KindSet:
		return "set"
	case KindOption:
		return "option"
	case KindTask:
		return "task"
	case KindTuple:
		return "tuple"
	case KindRecord:
		return "record"
	case KindHandle:
		return "handle"
	case KindEnum:
		return "enum"
	default:
		return "unknown"
	}
}
