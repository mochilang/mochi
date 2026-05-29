// Package emit produces Mochi-side source files that bind a Phase 4 C# shim
// project into the Mochi module namespace. It generates two artefacts per
// upstream NuGet package:
//
//   - <pkg>_extern.mochi: `extern fun` declarations targeting the shim's
//     [UnmanagedCallersOnly] symbols, plus `extern type` declarations for
//     every opaque handle type referenced in the surface.
//   - <pkg>.mochi: a user-facing module that re-exports each extern under
//     its short Mochi name, so callers write `json.Serialize(obj)` rather
//     than the full mangled symbol.
//
// The split keeps the FFI layer cleanly separated from the API the user
// actually imports.
package emit

import (
	"strings"

	"mochi/package3/dotnet/typemap"
)

// MochiRender lowers a typemap.Mapping into Mochi-syntax source text.
// Generic containers use angle brackets and comma-separated args
// (list<T>, map<K, V>, option<T>); tuples use parens.
// User-defined class/interface/enum types render as the HandleName.
func MochiRender(m *typemap.Mapping) string {
	if m == nil {
		return "()"
	}
	switch m.Kind {
	case typemap.KindBool:
		return "bool"
	case typemap.KindByte:
		return "byte"
	case typemap.KindInt:
		return "int"
	case typemap.KindInt64:
		return "int64"
	case typemap.KindUInt:
		return "uint"
	case typemap.KindUInt64:
		return "uint64"
	case typemap.KindFloat:
		return "float"
	case typemap.KindFloat64:
		return "float64"
	case typemap.KindChar:
		return "char"
	case typemap.KindString:
		return "string"
	case typemap.KindBytes:
		return "bytes"
	case typemap.KindUnit:
		return "()"
	case typemap.KindList:
		if m.Elem != nil {
			return "list<" + MochiRender(m.Elem) + ">"
		}
		return "list<any>"
	case typemap.KindMap:
		key := "any"
		val := "any"
		if m.Key != nil {
			key = MochiRender(m.Key)
		}
		if m.Value != nil {
			val = MochiRender(m.Value)
		}
		return "map<" + key + ", " + val + ">"
	case typemap.KindSet:
		if m.Elem != nil {
			return "set<" + MochiRender(m.Elem) + ">"
		}
		return "set<any>"
	case typemap.KindOption:
		if m.Elem != nil {
			return "option<" + MochiRender(m.Elem) + ">"
		}
		return "option<any>"
	case typemap.KindTask:
		if m.Inner != nil {
			if m.Inner.Kind == typemap.KindUnit {
				return "async"
			}
			return "async<" + MochiRender(m.Inner) + ">"
		}
		return "async"
	case typemap.KindTuple:
		parts := make([]string, len(m.Fields))
		for i := range m.Fields {
			parts[i] = MochiRender(&m.Fields[i])
		}
		return "(" + strings.Join(parts, ", ") + ")"
	case typemap.KindRecord:
		if m.HandleName != "" {
			return m.HandleName
		}
		return "record"
	case typemap.KindHandle:
		if m.HandleName != "" {
			return m.HandleName
		}
		return "handle"
	case typemap.KindEnum:
		if m.HandleName != "" {
			return m.HandleName + "_enum"
		}
		return "enum"
	}
	return "any"
}

// typeNames walks a Mapping and returns every HandleName for handle/record/enum
// types. Used to collect extern type declarations.
func typeNames(m *typemap.Mapping) []string {
	if m == nil {
		return nil
	}
	var out []string
	switch m.Kind {
	case typemap.KindHandle, typemap.KindRecord, typemap.KindEnum:
		if m.HandleName != "" {
			out = append(out, m.HandleName)
		}
	case typemap.KindList, typemap.KindSet, typemap.KindOption:
		out = append(out, typeNames(m.Elem)...)
	case typemap.KindTask:
		out = append(out, typeNames(m.Inner)...)
	case typemap.KindMap:
		out = append(out, typeNames(m.Key)...)
		out = append(out, typeNames(m.Value)...)
	case typemap.KindTuple:
		for i := range m.Fields {
			out = append(out, typeNames(&m.Fields[i])...)
		}
	}
	return out
}
