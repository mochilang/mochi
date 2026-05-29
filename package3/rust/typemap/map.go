package typemap

import (
	"fmt"
	"strings"

	"mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
)

// primitiveKind is the closed table of Rust primitive names to Mochi
// kinds. Anything outside this table is treated as "unmodelled
// primitive" and returns SkipUnknown.
var primitiveKind = map[string]Kind{
	"bool":  KindBool,
	"i8":    KindInt,
	"i16":   KindInt,
	"i32":   KindInt,
	"i64":   KindInt64,
	"i128":  KindInt64,
	"u8":    KindByte,
	"u16":   KindUInt,
	"u32":   KindUInt,
	"u64":   KindUInt64,
	"u128":  KindUInt64,
	"isize": KindInt64,
	"usize": KindUInt64,
	"f32":   KindFloat,
	"f64":   KindFloat64,
	"char":  KindChar,
	"str":   KindString,
	"never": KindUnit,
	"!":     KindUnit,
}

// Map lowers a rustdoc.Type into a Mapping or returns an
// errors.SkipReason explaining why no mapping exists. The Direction
// parameter affects borrowed-reference handling: output positions
// reject non-'static lifetimes, while input positions accept them
// and copy through the FFI.
//
// Map's return convention: when SkipReason == errors.SkipUnknown
// AND detail == "", the Mapping is valid. Any non-zero SkipReason
// indicates failure. (SkipUnknown is otherwise valid for genuine
// unknown-variant failures; the detail string distinguishes them.)
func Map(t rustdoc.Type, dir Direction) (*Mapping, errors.SkipReason, string) {
	// The unit type `()` arrives from rustdoc as {"tuple": []}; the
	// rustdoc.Type.Kind() helper reports that as "empty" because it
	// gates on len > 0. Recognise the non-nil empty slice as unit
	// before the main switch.
	if t.Tuple != nil && len(t.Tuple) == 0 {
		return &Mapping{Kind: KindUnit, Rust: "()"}, errors.SkipUnknown, ""
	}
	switch t.Kind() {
	case "primitive":
		return mapPrimitive(t.Primitive)
	case "resolved_path":
		return mapPath(t.ResolvedPath, dir)
	case "tuple":
		return mapTuple(t.Tuple, dir)
	case "slice":
		if t.Slice == nil {
			return nil, errors.SkipUnknown, "slice missing element type"
		}
		return mapSlice(*t.Slice, dir)
	case "array":
		return mapArray(t.Array, dir)
	case "borrowed_ref":
		return mapBorrowed(t.BorrowedRef, dir)
	case "raw_pointer":
		return nil, errors.SkipRawPointer, "raw pointer requires unsafe capability opt-in"
	case "generic":
		return nil, errors.SkipGeneric, fmt.Sprintf("unresolved generic %q; declare under [rust.monomorphise]", t.Generic)
	case "dyn_trait":
		return nil, errors.SkipDynTrait, "dyn Trait has no Mochi surface in v1"
	case "impl_trait":
		return nil, errors.SkipImplTrait, "impl Trait return position requires explicit monomorphisation"
	case "qualified_path":
		return nil, errors.SkipQualifiedPath, "qualified path (<T as Trait>::Item) has no Mochi surface"
	case "function_pointer":
		return nil, errors.SkipUnknown, "function pointer types are not mapped in v1"
	case "infer":
		return nil, errors.SkipUnknown, "inference placeholder cannot appear in a public signature"
	case "pat":
		return nil, errors.SkipUnknown, "pattern types are an unstable rustc feature"
	case "empty":
		return nil, errors.SkipUnknown, "empty type variant"
	}
	if strings.HasPrefix(t.Kind(), "unknown:") {
		return nil, errors.SkipUnknown, fmt.Sprintf("unmodelled rustdoc-types variant %s", t.Kind())
	}
	return nil, errors.SkipUnknown, fmt.Sprintf("unhandled type kind %s", t.Kind())
}

func mapPrimitive(name string) (*Mapping, errors.SkipReason, string) {
	if name == "()" {
		return &Mapping{Kind: KindUnit, Rust: "()"}, errors.SkipUnknown, ""
	}
	k, ok := primitiveKind[name]
	if !ok {
		return nil, errors.SkipUnknown, fmt.Sprintf("unmodelled primitive %q", name)
	}
	return &Mapping{Kind: k, Rust: name}, errors.SkipUnknown, ""
}

func mapPath(p *rustdoc.PathType, dir Direction) (*Mapping, errors.SkipReason, string) {
	if p == nil {
		return nil, errors.SkipUnknown, "resolved_path missing payload"
	}
	last := LastSegment(p.Path)
	switch last {
	case "String":
		return &Mapping{Kind: KindString, Rust: p.Path}, errors.SkipUnknown, ""
	case "Vec":
		elem, sr, det := mapPathArg(p, 0, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		if elem.Kind == KindByte {
			return &Mapping{Kind: KindBytes, Rust: p.Path}, errors.SkipUnknown, ""
		}
		return &Mapping{Kind: KindList, Rust: p.Path, Elem: elem}, errors.SkipUnknown, ""
	case "Option":
		elem, sr, det := mapPathArg(p, 0, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		return &Mapping{Kind: KindOption, Rust: p.Path, Elem: elem}, errors.SkipUnknown, ""
	case "Result":
		ok, sr, det := mapPathArg(p, 0, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		er, sr2, det2 := mapPathArg(p, 1, dir)
		if sr2 != errors.SkipUnknown || det2 != "" {
			return nil, sr2, det2
		}
		return &Mapping{Kind: KindResult, Rust: p.Path, OK: ok, Err: er}, errors.SkipUnknown, ""
	case "HashMap", "BTreeMap":
		k, sr, det := mapPathArg(p, 0, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		v, sr2, det2 := mapPathArg(p, 1, dir)
		if sr2 != errors.SkipUnknown || det2 != "" {
			return nil, sr2, det2
		}
		return &Mapping{Kind: KindMap, Rust: p.Path, Key: k, Value: v}, errors.SkipUnknown, ""
	case "HashSet", "BTreeSet":
		elem, sr, det := mapPathArg(p, 0, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		return &Mapping{Kind: KindList, Rust: p.Path, Elem: elem}, errors.SkipUnknown, ""
	case "Box", "Rc", "Arc":
		return mapPathArg(p, 0, dir)
	case "Cow":
		return nil, errors.SkipCow, "Cow<T> is not directly mappable; pass owned type"
	case "OsString", "OsStr", "PathBuf", "Path", "CString", "CStr":
		return nil, errors.SkipOsString, fmt.Sprintf("%s requires platform-specific encoding", last)
	case "Pin":
		return nil, errors.SkipPin, "Pin<T> requires custom lifetime contracts"
	}
	return &Mapping{Kind: KindStruct, Rust: p.Path, PathID: p.ID, PathName: p.Path}, errors.SkipUnknown, ""
}

// mapPathArg pulls the idx-th type argument from a PathType's
// AngleBracketed args list (skipping lifetime args) and recursively
// maps it.
func mapPathArg(p *rustdoc.PathType, idx int, dir Direction) (*Mapping, errors.SkipReason, string) {
	if p.Args == nil || p.Args.AngleBracketed == nil {
		return nil, errors.SkipUnknown, "missing generic arguments"
	}
	typeArgs := make([]rustdoc.GenericArg, 0, len(p.Args.AngleBracketed.Args))
	for _, a := range p.Args.AngleBracketed.Args {
		if a.Type != nil {
			typeArgs = append(typeArgs, a)
		}
	}
	if idx >= len(typeArgs) {
		return nil, errors.SkipUnknown, fmt.Sprintf("missing generic argument %d", idx)
	}
	return Map(*typeArgs[idx].Type, dir)
}

func mapTuple(elems []rustdoc.Type, dir Direction) (*Mapping, errors.SkipReason, string) {
	if len(elems) == 0 {
		return &Mapping{Kind: KindUnit, Rust: "()"}, errors.SkipUnknown, ""
	}
	fields := make([]Mapping, 0, len(elems))
	parts := make([]string, 0, len(elems))
	for i, e := range elems {
		m, sr, det := Map(e, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, fmt.Sprintf("tuple field %d: %s", i, det)
		}
		fields = append(fields, *m)
		parts = append(parts, m.Rust)
	}
	return &Mapping{Kind: KindTuple, Rust: "(" + strings.Join(parts, ", ") + ")", Fields: fields}, errors.SkipUnknown, ""
}

func mapSlice(elem rustdoc.Type, dir Direction) (*Mapping, errors.SkipReason, string) {
	em, sr, det := Map(elem, dir)
	if sr != errors.SkipUnknown || det != "" {
		return nil, sr, det
	}
	if em.Kind == KindByte {
		return &Mapping{Kind: KindBytes, Rust: "[u8]"}, errors.SkipUnknown, ""
	}
	return &Mapping{Kind: KindList, Rust: "[" + em.Rust + "]", Elem: em}, errors.SkipUnknown, ""
}

func mapArray(a *rustdoc.ArrayType, dir Direction) (*Mapping, errors.SkipReason, string) {
	if a == nil {
		return nil, errors.SkipUnknown, "array missing payload"
	}
	em, sr, det := Map(a.Type, dir)
	if sr != errors.SkipUnknown || det != "" {
		return nil, sr, det
	}
	if em.Kind == KindByte {
		return &Mapping{Kind: KindBytes, Rust: fmt.Sprintf("[u8; %s]", a.Len)}, errors.SkipUnknown, ""
	}
	return &Mapping{Kind: KindList, Rust: fmt.Sprintf("[%s; %s]", em.Rust, a.Len), Elem: em}, errors.SkipUnknown, ""
}

func mapBorrowed(b *rustdoc.BorrowedRefType, dir Direction) (*Mapping, errors.SkipReason, string) {
	if b == nil {
		return nil, errors.SkipUnknown, "borrowed_ref missing payload"
	}
	if b.IsMutable {
		return nil, errors.SkipUnknown, "&mut T requires capability opt-in (no v1 mapping)"
	}
	if dir == DirectionOut && b.Lifetime != "" && b.Lifetime != "'static" {
		return nil, errors.SkipLifetime, fmt.Sprintf("output borrows %s; only 'static borrows can be returned", b.Lifetime)
	}
	inner, sr, det := Map(b.Type, dir)
	if sr != errors.SkipUnknown || det != "" {
		return nil, sr, det
	}
	return inner, errors.SkipUnknown, ""
}

// LastSegment returns the last "::"-delimited segment of a Rust
// path. Used to canonicalise std/alloc/core qualifications.
func LastSegment(path string) string {
	if i := strings.LastIndex(path, "::"); i >= 0 {
		return path[i+2:]
	}
	return path
}
