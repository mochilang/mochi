// Package emit produces Mochi-side source files that bind a Phase 4
// wrapper crate into the Mochi module namespace. It generates two
// artefacts per upstream crate:
//
//   - <crate>_extern.mochi: `extern fun` declarations targeting the
//     wrapper crate's #[no_mangle] extern-C symbols, plus
//     `extern type` declarations for every opaque handle type
//     referenced in the surface.
//   - <crate>.mochi: a user-facing module that re-exports each
//     extern under its short Mochi name (the last segment of the
//     rustdoc path), so callers write `hex.encode(buf)` rather than
//     `mochi_hex_encode(buf)`.
//
// The split keeps the FFI layer cleanly separated from the API the
// user actually imports. Phase 6 (`import rust "<crate>@<semver>" as
// <alias>` grammar) treats the alias module as the import target.
package emit

import (
	"strings"

	"mochi/package3/rust/typemap"
)

// MochiRender lowers a typemap.Mapping into Mochi-syntax source.
// Generic containers use angle brackets and comma-separated args
// (`list<T>`, `map<K, V>`, `Option<T>`, `Result<T, E>`); tuples use
// parens. User-defined struct / enum / handle types render as the
// last segment of their Rust path (a corresponding `extern type`
// declaration is emitted alongside the function decls).
//
// All sized integer kinds collapse to `int`; both f32 and f64 to
// `float`. Mochi's `int` is 64-bit, so any precision narrowing
// happens on the C-ABI side via the wrapper crate, not at the
// Mochi surface.
func MochiRender(m typemap.Mapping) string {
	switch m.Kind {
	case typemap.KindUnit:
		return "unit"
	case typemap.KindBool:
		return "bool"
	case typemap.KindInt, typemap.KindInt64,
		typemap.KindUInt, typemap.KindUInt64,
		typemap.KindByte, typemap.KindChar:
		return "int"
	case typemap.KindFloat, typemap.KindFloat64:
		return "float"
	case typemap.KindString:
		return "string"
	case typemap.KindBytes:
		return "bytes"
	case typemap.KindList:
		if m.Elem == nil {
			return "list<any>"
		}
		return "list<" + MochiRender(*m.Elem) + ">"
	case typemap.KindMap:
		if m.Key == nil || m.Value == nil {
			return "map<any, any>"
		}
		return "map<" + MochiRender(*m.Key) + ", " + MochiRender(*m.Value) + ">"
	case typemap.KindOption:
		if m.Elem == nil {
			return "Option<any>"
		}
		return "Option<" + MochiRender(*m.Elem) + ">"
	case typemap.KindResult:
		ok := "any"
		err := "any"
		if m.OK != nil {
			ok = MochiRender(*m.OK)
		}
		if m.Err != nil {
			err = MochiRender(*m.Err)
		}
		return "Result<" + ok + ", " + err + ">"
	case typemap.KindTuple:
		parts := make([]string, len(m.Fields))
		for i, f := range m.Fields {
			parts[i] = MochiRender(f)
		}
		return "(" + strings.Join(parts, ", ") + ")"
	case typemap.KindStruct, typemap.KindEnum, typemap.KindHandle:
		return lastSegment(m.PathName)
	}
	return "any"
}

// typeNames walks a Mapping and returns every user-defined struct /
// enum / handle type name (last path segment) that appears. Used to
// gate `extern type` declarations.
func typeNames(m typemap.Mapping) []string {
	var out []string
	switch m.Kind {
	case typemap.KindStruct, typemap.KindEnum, typemap.KindHandle:
		if n := lastSegment(m.PathName); n != "" {
			out = append(out, n)
		}
	case typemap.KindList, typemap.KindOption:
		if m.Elem != nil {
			out = append(out, typeNames(*m.Elem)...)
		}
	case typemap.KindMap:
		if m.Key != nil {
			out = append(out, typeNames(*m.Key)...)
		}
		if m.Value != nil {
			out = append(out, typeNames(*m.Value)...)
		}
	case typemap.KindResult:
		if m.OK != nil {
			out = append(out, typeNames(*m.OK)...)
		}
		if m.Err != nil {
			out = append(out, typeNames(*m.Err)...)
		}
	case typemap.KindTuple:
		for _, f := range m.Fields {
			out = append(out, typeNames(f)...)
		}
	}
	return out
}

func lastSegment(p string) string {
	i := strings.LastIndex(p, "::")
	if i < 0 {
		return p
	}
	return p[i+2:]
}
