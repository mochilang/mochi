package typemap

import (
	"fmt"
	"strings"

	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/metacli"
)

// Direction specifies whether a type appears in an input or output position.
type Direction int

const (
	DirectionIn  Direction = iota
	DirectionOut
)

// String renders a Direction token.
func (d Direction) String() string {
	if d == DirectionOut {
		return "out"
	}
	return "in"
}

// primitiveTable is the closed table of CLR primitive type names to Mochi Kinds.
var primitiveTable = map[string]Kind{
	"System.Boolean": KindBool,
	"System.Byte":    KindByte,
	"System.SByte":   KindInt,
	"System.Int16":   KindInt,
	"System.Int32":   KindInt,
	"System.Int64":   KindInt64,
	"System.UInt16":  KindUInt,
	"System.UInt32":  KindUInt,
	"System.UInt64":  KindUInt64,
	"System.Single":  KindFloat,
	"System.Double":  KindFloat64,
	"System.Char":    KindChar,
	"System.String":  KindString,
	"System.Void":    KindUnit,
}

// primitiveFFI maps CLR primitive names to their C# FFI representation.
var primitiveFFI = map[string]string{
	"System.Boolean": "byte",
	"System.Byte":    "byte",
	"System.SByte":   "sbyte",
	"System.Int16":   "short",
	"System.Int32":   "int",
	"System.Int64":   "long",
	"System.UInt16":  "ushort",
	"System.UInt32":  "uint",
	"System.UInt64":  "ulong",
	"System.Single":  "float",
	"System.Double":  "double",
	"System.Char":    "char",
	"System.String":  "byte*, int",
	"System.Void":    "void",
}

// collectionTable1 maps generic collection types with a single type argument.
// Key: CLR full name (without backtick), value: KindList or KindSet.
var collectionTable1 = map[string]Kind{
	"System.Collections.Generic.List`1":             KindList,
	"System.Collections.Generic.IList`1":            KindList,
	"System.Collections.Generic.IEnumerable`1":      KindList,
	"System.Collections.Generic.IReadOnlyList`1":    KindList,
	"System.Collections.Generic.ICollection`1":      KindList,
	"System.Collections.Generic.IReadOnlyCollection`1": KindList,
	"System.Collections.Generic.HashSet`1":          KindSet,
	"System.Collections.Generic.ISet`1":             KindSet,
}

// collectionTable2 maps generic collection types with two type arguments (K,V).
var collectionTable2 = map[string]bool{
	"System.Collections.Generic.Dictionary`2":            true,
	"System.Collections.Generic.IDictionary`2":           true,
	"System.Collections.Generic.IReadOnlyDictionary`2":   true,
	"System.Collections.Generic.SortedDictionary`2":      true,
}

// valueTupleNames is the set of ValueTuple CLR names.
var valueTupleNames = map[string]bool{
	"System.ValueTuple`2": true,
	"System.ValueTuple`3": true,
	"System.ValueTuple`4": true,
	"System.ValueTuple`5": true,
	"System.ValueTuple`6": true,
	"System.ValueTuple`7": true,
}

// Map translates a metacli.TypeRef to a Mapping or returns an
// errors.SkipReason explaining why no mapping exists.
//
// Return convention: when SkipReason == errors.SkipUnknown AND detail == "",
// the Mapping is valid. Any non-zero SkipReason indicates refusal.
func Map(t metacli.TypeRef, dir Direction) (*Mapping, errors.SkipReason, string) {
	// Refuse unsafe/unsupported kinds first.
	switch t.Kind {
	case metacli.TypeRefPointer:
		return nil, errors.SkipPointer, "unsafe pointer type requires capabilities opt-in"
	case metacli.TypeRefByRef:
		return nil, errors.SkipRefType, "ref/out parameter requires special marshalling"
	case metacli.TypeRefDynamic:
		return nil, errors.SkipDynamic, "dynamic type has no static surface"
	case metacli.TypeRefDelegate:
		return nil, errors.SkipDelegate, "delegate / Action / Func type is not bridged in v1"
	case metacli.TypeRefGenericParam:
		return nil, errors.SkipGeneric, fmt.Sprintf("unresolved generic parameter %q; declare under [dotnet.monomorphise]", t.FullName)
	}

	// Void shortcut.
	if t.Kind == metacli.TypeRefVoid || t.FullName == "System.Void" {
		return &Mapping{Kind: KindUnit, CLRName: "System.Void", MochiType: "unit"}, errors.SkipUnknown, ""
	}

	// Array types.
	if t.ArrayRank > 0 || t.Kind == metacli.TypeRefArray {
		return mapArray(t, dir)
	}

	// Primitive table lookup.
	if k, ok := primitiveTable[t.FullName]; ok {
		ffi := primitiveFFI[t.FullName]
		m := &Mapping{Kind: k, CLRName: t.FullName}
		m.FFIReprOverride = ffi
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// Nullable<T> or IsNullable flag.
	if t.IsNullable || t.FullName == "System.Nullable`1" {
		return mapNullable(t, dir)
	}

	// Generic instantiation.
	if t.Kind == metacli.TypeRefGenericInst {
		return mapGenericInst(t, dir)
	}

	// CLR struct -> KindRecord.
	if t.Kind == metacli.TypeRefStruct {
		handle := shortName(t.FullName)
		m := &Mapping{Kind: KindRecord, CLRName: t.FullName, HandleName: handle}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// CLR enum.
	if t.Kind == metacli.TypeRefEnum {
		handle := shortName(t.FullName)
		m := &Mapping{Kind: KindEnum, CLRName: t.FullName, HandleName: handle}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// Non-generic Task (async void equivalent) — check before generic handle path.
	if t.FullName == "System.Threading.Tasks.Task" {
		m := &Mapping{Kind: KindUnit, CLRName: t.FullName}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// CLR class or interface -> KindHandle.
	if t.Kind == metacli.TypeRefClass || t.Kind == metacli.TypeRefInterface {
		return mapHandle(t)
	}

	return nil, errors.SkipUnknown, fmt.Sprintf("unhandled CLR TypeRef kind %q for %q", t.Kind, t.FullName)
}

// mapArray maps array types. byte[] -> KindBytes; T[] -> KindList<T>.
func mapArray(t metacli.TypeRef, dir Direction) (*Mapping, errors.SkipReason, string) {
	// byte[] is KindBytes.
	if t.FullName == "System.Byte" || t.FullName == "System.Byte[]" {
		m := &Mapping{Kind: KindBytes, CLRName: "System.Byte[]"}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}
	// Generic T[] — need the element TypeRef.
	// For array kinds, the FullName is the element type.
	elemRef := metacli.TypeRef{FullName: t.FullName, Kind: t.Kind}
	// If t is a TypeRefArray with TypeArgs, use them.
	if len(t.TypeArgs) > 0 {
		elemRef = t.TypeArgs[0]
	} else if t.Kind == metacli.TypeRefArray {
		// The element is described by FullName alone with a non-array kind.
		elemRef = metacli.TypeRef{FullName: t.FullName, Kind: metacli.TypeRefClass}
	}
	elem, sr, det := Map(elemRef, dir)
	if sr != errors.SkipUnknown || det != "" {
		return nil, sr, det
	}
	if elem.Kind == KindByte {
		m := &Mapping{Kind: KindBytes, CLRName: t.FullName + "[]"}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}
	m := &Mapping{Kind: KindList, CLRName: t.FullName + "[]", Elem: elem}
	m.MochiType = renderMochiType(m)
	return m, errors.SkipUnknown, ""
}

// mapNullable maps Nullable<T> or T? types to KindOption<T>.
func mapNullable(t metacli.TypeRef, dir Direction) (*Mapping, errors.SkipReason, string) {
	if len(t.TypeArgs) == 0 {
		return nil, errors.SkipUnknown, fmt.Sprintf("Nullable type %q missing type argument", t.FullName)
	}
	inner, sr, det := Map(t.TypeArgs[0], dir)
	if sr != errors.SkipUnknown || det != "" {
		return nil, sr, det
	}
	m := &Mapping{Kind: KindOption, CLRName: t.FullName, Elem: inner, Inner: inner}
	m.MochiType = renderMochiType(m)
	return m, errors.SkipUnknown, ""
}

// mapGenericInst maps generic instantiations: collections, Task<T>, tuples,
// and special types like Span<T>.
func mapGenericInst(t metacli.TypeRef, dir Direction) (*Mapping, errors.SkipReason, string) {
	name := t.FullName

	// Span<T> / ReadOnlySpan<T>.
	if name == "System.Span`1" || name == "System.ReadOnlySpan`1" {
		return nil, errors.SkipSpan, fmt.Sprintf("%s is a stack-only type and cannot cross the FFI boundary", name)
	}

	// ValueTask<T>.
	if name == "System.Threading.Tasks.ValueTask`1" {
		return nil, errors.SkipValueTask, "ValueTask<T> is not supported in v1; use Task<T> instead"
	}

	// Action<T...> / Func<T...>.
	if strings.HasPrefix(name, "System.Action`") || strings.HasPrefix(name, "System.Func`") {
		return nil, errors.SkipDelegate, fmt.Sprintf("%s is a delegate type not bridged in v1", name)
	}

	// Nullable<T>.
	if name == "System.Nullable`1" {
		return mapNullable(t, dir)
	}

	// Task<T>.
	if name == "System.Threading.Tasks.Task`1" {
		if len(t.TypeArgs) == 0 {
			return nil, errors.SkipUnknown, "Task<T> missing type argument"
		}
		inner, sr, det := Map(t.TypeArgs[0], dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		m := &Mapping{Kind: KindTask, CLRName: name, Elem: inner, Inner: inner}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// Non-generic Task (async void).
	if name == "System.Threading.Tasks.Task" {
		m := &Mapping{Kind: KindUnit, CLRName: name}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// ValueTuple<T...>.
	if valueTupleNames[name] {
		return mapTuple(t, dir)
	}

	// Single-arg collection types (List<T>, IEnumerable<T>, HashSet<T>, ...).
	if kind, ok := collectionTable1[name]; ok {
		if len(t.TypeArgs) == 0 {
			return nil, errors.SkipUnknown, fmt.Sprintf("%s missing type argument", name)
		}
		elem, sr, det := Map(t.TypeArgs[0], dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		// byte[] equivalent.
		if kind == KindList && elem.Kind == KindByte {
			m := &Mapping{Kind: KindBytes, CLRName: name}
			m.MochiType = renderMochiType(m)
			return m, errors.SkipUnknown, ""
		}
		m := &Mapping{Kind: kind, CLRName: name, Elem: elem}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// Two-arg dictionary types.
	if collectionTable2[name] {
		if len(t.TypeArgs) < 2 {
			return nil, errors.SkipUnknown, fmt.Sprintf("%s missing key/value type arguments", name)
		}
		k, sr, det := Map(t.TypeArgs[0], dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, det
		}
		v, sr2, det2 := Map(t.TypeArgs[1], dir)
		if sr2 != errors.SkipUnknown || det2 != "" {
			return nil, sr2, det2
		}
		m := &Mapping{Kind: KindMap, CLRName: name, Key: k, Value: v}
		m.MochiType = renderMochiType(m)
		return m, errors.SkipUnknown, ""
	}

	// Unknown generic instantiation: treat as opaque handle.
	handle := shortName(name)
	m := &Mapping{Kind: KindHandle, CLRName: name, HandleName: handle}
	m.MochiType = renderMochiType(m)
	return m, errors.SkipUnknown, ""
}

// mapTuple maps ValueTuple<T1,...,Tn> to KindTuple.
func mapTuple(t metacli.TypeRef, dir Direction) (*Mapping, errors.SkipReason, string) {
	if len(t.TypeArgs) == 0 {
		return nil, errors.SkipUnknown, fmt.Sprintf("%s missing type arguments", t.FullName)
	}
	fields := make([]Mapping, 0, len(t.TypeArgs))
	for i, arg := range t.TypeArgs {
		fm, sr, det := Map(arg, dir)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, fmt.Sprintf("tuple field %d: %s", i, det)
		}
		fields = append(fields, *fm)
	}
	m := &Mapping{Kind: KindTuple, CLRName: t.FullName, Fields: fields}
	m.MochiType = renderMochiType(m)
	return m, errors.SkipUnknown, ""
}

// mapHandle maps a class or interface TypeRef to KindHandle.
func mapHandle(t metacli.TypeRef) (*Mapping, errors.SkipReason, string) {
	handle := shortName(t.FullName)
	m := &Mapping{Kind: KindHandle, CLRName: t.FullName, HandleName: handle}
	m.MochiType = renderMochiType(m)
	return m, errors.SkipUnknown, ""
}

// shortName returns the last dot-separated segment of a CLR full name,
// stripping any backtick+arity suffix (e.g. "List`1" -> "List").
func shortName(full string) string {
	if i := strings.LastIndex(full, "."); i >= 0 {
		full = full[i+1:]
	}
	if i := strings.Index(full, "`"); i >= 0 {
		full = full[:i]
	}
	return full
}
