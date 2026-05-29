package typemap

import "strings"

// Mapping is a successful translation from a Rust type to a Mochi
// type plus its FFI representation. Failed translations return an
// errors.SkipReason; see map.go.
//
// Compound mappings (List, Map, Option, Result, Tuple) carry their
// child Mappings by pointer or slice. Reference types are folded
// into the child Mapping at lowering time, so a Mapping never
// carries borrow metadata.
type Mapping struct {
	Kind Kind

	// Rust is the canonical Rust source rendering of the input type
	// (e.g. "Vec<i64>", "Option<String>", "core::option::Option<T>").
	// Kept for SKIPPED diagnostics and wrapper-crate code generation.
	Rust string

	// Elem is the element Mapping for List, Bytes (nil since Bytes
	// is a primitive), and Option.
	Elem *Mapping

	// Key, Value are populated for Map.
	Key   *Mapping
	Value *Mapping

	// OK, Err are populated for Result.
	OK  *Mapping
	Err *Mapping

	// Fields is populated for Tuple in positional order.
	Fields []Mapping

	// PathID and PathName are populated for Struct, Enum, and Handle.
	// PathID is the rustdoc Item ID; PathName is the rendered Rust
	// path (e.g. "tokio::sync::Mutex"). Both are empty for everything
	// else.
	PathID   string
	PathName string
}

// MochiType returns the Mochi-side rendering of this Mapping. The
// output is used by phase 5 (Mochi extern fn emitter) and is stable
// for golden-fixture diffs.
func (m Mapping) MochiType() string {
	switch m.Kind {
	case KindUnit:
		return "unit"
	case KindBool:
		return "bool"
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
	case KindByte:
		return "byte"
	case KindChar:
		return "char"
	case KindString:
		return "string"
	case KindBytes:
		return "bytes"
	case KindList:
		if m.Elem != nil {
			return "list[" + m.Elem.MochiType() + "]"
		}
		return "list[?]"
	case KindMap:
		if m.Key != nil && m.Value != nil {
			return "map[" + m.Key.MochiType() + "]" + m.Value.MochiType()
		}
		return "map[?]?"
	case KindOption:
		if m.Elem != nil {
			return "?" + m.Elem.MochiType()
		}
		return "?"
	case KindResult:
		ok := "?"
		er := "?"
		if m.OK != nil {
			ok = m.OK.MochiType()
		}
		if m.Err != nil {
			er = m.Err.MochiType()
		}
		return "result[" + ok + "," + er + "]"
	case KindTuple:
		parts := make([]string, len(m.Fields))
		for i, f := range m.Fields {
			parts[i] = f.MochiType()
		}
		return "tuple[" + strings.Join(parts, ",") + "]"
	case KindStruct, KindEnum, KindHandle:
		if m.PathName != "" {
			return m.PathName
		}
		return m.Kind.String()
	}
	return "invalid"
}

// FFIRepr returns the C-ABI rendering used by the phase 4 wrapper
// synthesiser. The rendering deliberately matches the wrapper-side
// header that phase 4 will generate, so the typemap output is the
// single source of truth for layout decisions.
func (m Mapping) FFIRepr() string {
	switch m.Kind {
	case KindUnit:
		return "void"
	case KindBool:
		return "bool"
	case KindInt:
		return "int32_t"
	case KindInt64:
		return "int64_t"
	case KindUInt:
		return "uint32_t"
	case KindUInt64:
		return "uint64_t"
	case KindFloat:
		return "float"
	case KindFloat64:
		return "double"
	case KindByte:
		return "uint8_t"
	case KindChar:
		return "uint32_t"
	case KindString:
		return "MochiString"
	case KindBytes:
		return "MochiSlice"
	case KindList:
		return "MochiSlice"
	case KindMap:
		return "MochiMap"
	case KindOption:
		return "MochiOption"
	case KindResult:
		return "MochiResult"
	case KindTuple:
		return "MochiTuple"
	case KindStruct, KindEnum, KindHandle:
		return "MochiHandle"
	}
	return "void"
}

// IsScalar reports whether the Mapping holds a primitive scalar that
// crosses the FFI boundary by value (i.e. without an indirection
// through MochiString / MochiSlice / MochiHandle).
func (m Mapping) IsScalar() bool {
	switch m.Kind {
	case KindBool, KindInt, KindInt64, KindUInt, KindUInt64,
		KindFloat, KindFloat64, KindByte, KindChar, KindUnit:
		return true
	}
	return false
}
