package typemap

import "strings"

// Mapping is the result of translating one CLR TypeRef to a Mochi type.
// Compound mappings (List, Map, Set, Option, Task, Tuple) carry their
// child Mappings by pointer or slice. Failed translations return a
// SkipReason; see map.go.
type Mapping struct {
	Kind Kind

	// CLRName is the CLR full name (e.g. "System.Collections.Generic.List`1").
	CLRName string

	// MochiType is the rendered Mochi type string (e.g. "list<int>").
	MochiType string

	// FFIReprOverride, when non-empty, overrides the FFIRepr() return value.
	// Used by the primitive table to supply exact C# type names.
	FFIReprOverride string

	// Elem is the element Mapping for KindList, KindSet, KindOption, KindTask.
	Elem *Mapping

	// Key and Value are populated for KindMap.
	Key   *Mapping
	Value *Mapping

	// Inner is populated for KindOption and KindTask when Elem is used,
	// provided as an alias for clarity.
	Inner *Mapping

	// Fields is populated for KindTuple and KindRecord in positional order.
	Fields []Mapping

	// HandleName is the CLR short name used as the extern type name for
	// KindHandle and KindEnum.
	HandleName string
}

// IsScalar reports whether the mapping represents a scalar (no heap
// allocation on the Mochi side).
func (m *Mapping) IsScalar() bool {
	if m == nil {
		return false
	}
	switch m.Kind {
	case KindBool, KindByte, KindInt, KindInt64, KindUInt, KindUInt64,
		KindFloat, KindFloat64, KindChar, KindUnit, KindEnum:
		return true
	}
	return false
}

// FFIRepr returns the C# [UnmanagedCallersOnly]-compatible type representation
// for use in generated shim code. Scalars map to their C# primitive type.
// Strings are represented as "byte*, int" (UTF-8 pointer+length pair).
// Handles and opaque records use "nint".
func (m *Mapping) FFIRepr() string {
	if m == nil {
		return "void"
	}
	if m.FFIReprOverride != "" {
		return m.FFIReprOverride
	}
	switch m.Kind {
	case KindUnit:
		return "void"
	case KindBool:
		return "byte"
	case KindByte:
		return "byte"
	case KindInt:
		return "int"
	case KindInt64:
		return "long"
	case KindUInt:
		return "uint"
	case KindUInt64:
		return "ulong"
	case KindFloat:
		return "float"
	case KindFloat64:
		return "double"
	case KindChar:
		return "char"
	case KindString:
		return "byte*, int"
	case KindBytes:
		return "byte*, int"
	case KindEnum:
		return "int"
	case KindHandle, KindRecord:
		return "nint"
	case KindList, KindSet, KindMap, KindOption, KindTask, KindTuple:
		return "nint"
	}
	return "nint"
}

// renderMochiType builds the Mochi-side type annotation string from a
// Mapping. It is called after construction and stored in MochiType.
func renderMochiType(m *Mapping) string {
	if m == nil {
		return "unknown"
	}
	switch m.Kind {
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
		if m.Elem != nil {
			return "list<" + renderMochiType(m.Elem) + ">"
		}
		return "list<?>"
	case KindSet:
		if m.Elem != nil {
			return "set<" + renderMochiType(m.Elem) + ">"
		}
		return "set<?>"
	case KindMap:
		if m.Key != nil && m.Value != nil {
			return "map<" + renderMochiType(m.Key) + "," + renderMochiType(m.Value) + ">"
		}
		return "map<?,?>"
	case KindOption:
		if m.Elem != nil {
			return "?" + renderMochiType(m.Elem)
		}
		return "?"
	case KindTask:
		if m.Elem != nil {
			return "task<" + renderMochiType(m.Elem) + ">"
		}
		return "task"
	case KindTuple:
		parts := make([]string, len(m.Fields))
		for i := range m.Fields {
			parts[i] = renderMochiType(&m.Fields[i])
		}
		return "(" + strings.Join(parts, ",") + ")"
	case KindRecord:
		if m.HandleName != "" {
			return m.HandleName
		}
		return "record"
	case KindHandle:
		if m.HandleName != "" {
			return m.HandleName
		}
		return "handle"
	case KindEnum:
		if m.HandleName != "" {
			return m.HandleName
		}
		return "enum"
	}
	return "unknown"
}
