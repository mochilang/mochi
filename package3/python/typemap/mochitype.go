package typemap

import (
	"sort"
	"strings"
)

// Kind classifies a MochiType. The set is deliberately small; the Mochi side
// of the bridge speaks a closed subset of the Mochi type grammar.
type Kind int

const (
	// KindUnknown is the zero value and must not appear in committed mappings.
	KindUnknown Kind = iota
	// KindScalar is a single named scalar (int, float, bool, string, bytes, None).
	KindScalar
	// KindList is `list<T>`.
	KindList
	// KindTuple is `tuple<A, B, ...>` (heterogeneous form). Use KindList for the
	// homogeneous `tuple[T, ...]` form.
	KindTuple
	// KindMap is `map<K, V>` (K must be a scalar string or integer type).
	KindMap
	// KindSet is `set<T>`.
	KindSet
	// KindFun is `fun(P0, P1, ...): R`.
	KindFun
	// KindAsync is `async T`, derived from `Awaitable[T]`.
	KindAsync
	// KindStream is `stream<T>`, derived from `AsyncIterator[T]`.
	KindStream
	// KindOptional is `T?`, derived from `Optional[T]` or `T | None`.
	KindOptional
	// KindSum is a PEP 604 union `T1 | T2 | ...` with no None branch.
	KindSum
	// KindRecord is a Mochi record (TypedDict or frozen @dataclass).
	KindRecord
	// KindInterface is a Mochi interface (Python Protocol).
	KindInterface
	// KindRef is a reference to a user-defined class by name.
	KindRef
	// KindTypeVar is a bound polymorphic type variable left in the surface for
	// generic functions; Phase 5 chooses whether to monomorphise.
	KindTypeVar
)

// String renders the Kind as a stable token.
func (k Kind) String() string {
	switch k {
	case KindScalar:
		return "scalar"
	case KindList:
		return "list"
	case KindTuple:
		return "tuple"
	case KindMap:
		return "map"
	case KindSet:
		return "set"
	case KindFun:
		return "fun"
	case KindAsync:
		return "async"
	case KindStream:
		return "stream"
	case KindOptional:
		return "optional"
	case KindSum:
		return "sum"
	case KindRecord:
		return "record"
	case KindInterface:
		return "interface"
	case KindRef:
		return "ref"
	case KindTypeVar:
		return "typevar"
	default:
		return "unknown"
	}
}

// MochiType is the lowered representation of a Python type expression.
// Every field is optional and only meaningful for specific Kinds. The
// constructors below produce well-formed values.
type MochiType struct {
	Kind Kind
	// Name is the scalar / ref / typevar identifier ("int", "string", "MyClass", "T").
	Name string
	// Params is the parameter list for parameterised kinds:
	//   list<T>      -> Params = [T]
	//   tuple<A, B>  -> Params = [A, B]
	//   map<K, V>    -> Params = [K, V]
	//   set<T>       -> Params = [T]
	//   fun(P0): R   -> Params = [P0, ..., R]   (return type is the last entry)
	//   async T      -> Params = [T]
	//   stream<T>    -> Params = [T]
	//   optional T   -> Params = [T]
	//   sum T1 | T2  -> Params = [T1, T2, ...]
	Params []MochiType
	// Fields are the named members of a record.
	Fields []MochiField
	// Methods are the surface methods of an interface.
	Methods []MochiMethod
}

// MochiField is one named record field.
type MochiField struct {
	Name string
	Type MochiType
}

// MochiMethod is one interface method.
type MochiMethod struct {
	Name   string
	Params []MochiType
	Return MochiType
	IsAsync bool
}

// NewScalar constructs a scalar type.
func NewScalar(name string) MochiType { return MochiType{Kind: KindScalar, Name: name} }

// NewRef constructs a reference to a user-defined type.
func NewRef(name string) MochiType { return MochiType{Kind: KindRef, Name: name} }

// NewTypeVar constructs a bound type variable.
func NewTypeVar(name string) MochiType { return MochiType{Kind: KindTypeVar, Name: name} }

// NewList constructs `list<T>`.
func NewList(t MochiType) MochiType { return MochiType{Kind: KindList, Params: []MochiType{t}} }

// NewSet constructs `set<T>`.
func NewSet(t MochiType) MochiType { return MochiType{Kind: KindSet, Params: []MochiType{t}} }

// NewMap constructs `map<K, V>`.
func NewMap(k, v MochiType) MochiType {
	return MochiType{Kind: KindMap, Params: []MochiType{k, v}}
}

// NewTuple constructs `tuple<A, B, ...>`.
func NewTuple(elems ...MochiType) MochiType {
	return MochiType{Kind: KindTuple, Params: append([]MochiType(nil), elems...)}
}

// NewFun constructs `fun(P0, ...): R`. The return type is appended as the
// final Params element.
func NewFun(ret MochiType, params ...MochiType) MochiType {
	all := append([]MochiType(nil), params...)
	all = append(all, ret)
	return MochiType{Kind: KindFun, Params: all}
}

// NewAsync constructs `async T`.
func NewAsync(t MochiType) MochiType { return MochiType{Kind: KindAsync, Params: []MochiType{t}} }

// NewStream constructs `stream<T>`.
func NewStream(t MochiType) MochiType { return MochiType{Kind: KindStream, Params: []MochiType{t}} }

// NewOptional constructs `T?`. If t is already optional, it is returned as-is.
func NewOptional(t MochiType) MochiType {
	if t.Kind == KindOptional {
		return t
	}
	return MochiType{Kind: KindOptional, Params: []MochiType{t}}
}

// NewSum constructs `T1 | T2 | ...`. If there's only one branch it is
// returned directly. A None branch is converted into a wrapping Optional.
func NewSum(branches ...MochiType) MochiType {
	none := false
	var nonNone []MochiType
	for _, b := range branches {
		if b.Kind == KindScalar && b.Name == "None" {
			none = true
			continue
		}
		nonNone = append(nonNone, b)
	}
	if len(nonNone) == 0 {
		return NewScalar("None")
	}
	var inner MochiType
	if len(nonNone) == 1 {
		inner = nonNone[0]
	} else {
		inner = MochiType{Kind: KindSum, Params: nonNone}
	}
	if none {
		return NewOptional(inner)
	}
	return inner
}

// Return returns the return-type component of a KindFun MochiType. Panics if
// Kind is not KindFun.
func (m MochiType) Return() MochiType {
	if m.Kind != KindFun {
		panic("typemap: Return called on non-fun MochiType")
	}
	if len(m.Params) == 0 {
		return NewScalar("None")
	}
	return m.Params[len(m.Params)-1]
}

// FunParams returns the parameter slice of a KindFun (excluding the return).
func (m MochiType) FunParams() []MochiType {
	if m.Kind != KindFun {
		panic("typemap: FunParams called on non-fun MochiType")
	}
	if len(m.Params) == 0 {
		return nil
	}
	return m.Params[:len(m.Params)-1]
}

// Render produces the canonical Mochi-side string representation of the
// MochiType. The output is stable across releases and is used in generated
// extern declarations.
func (m MochiType) Render() string {
	var b strings.Builder
	m.renderInto(&b)
	return b.String()
}

func (m MochiType) renderInto(b *strings.Builder) {
	switch m.Kind {
	case KindScalar, KindRef, KindTypeVar:
		b.WriteString(m.Name)
	case KindList:
		b.WriteString("list<")
		m.Params[0].renderInto(b)
		b.WriteString(">")
	case KindSet:
		b.WriteString("set<")
		m.Params[0].renderInto(b)
		b.WriteString(">")
	case KindMap:
		b.WriteString("map<")
		m.Params[0].renderInto(b)
		b.WriteString(", ")
		m.Params[1].renderInto(b)
		b.WriteString(">")
	case KindTuple:
		b.WriteString("tuple<")
		for i, p := range m.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			p.renderInto(b)
		}
		b.WriteString(">")
	case KindAsync:
		b.WriteString("async ")
		m.Params[0].renderInto(b)
	case KindStream:
		b.WriteString("stream<")
		m.Params[0].renderInto(b)
		b.WriteString(">")
	case KindOptional:
		m.Params[0].renderInto(b)
		b.WriteString("?")
	case KindSum:
		for i, p := range m.Params {
			if i > 0 {
				b.WriteString(" | ")
			}
			p.renderInto(b)
		}
	case KindFun:
		b.WriteString("fun(")
		params := m.FunParams()
		for i, p := range params {
			if i > 0 {
				b.WriteString(", ")
			}
			p.renderInto(b)
		}
		b.WriteString("): ")
		m.Return().renderInto(b)
	case KindRecord:
		b.WriteString("record ")
		b.WriteString(m.Name)
		b.WriteString(" { ")
		names := make([]string, 0, len(m.Fields))
		index := make(map[string]MochiField, len(m.Fields))
		for _, f := range m.Fields {
			names = append(names, f.Name)
			index[f.Name] = f
		}
		sort.Strings(names)
		for i, n := range names {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(n)
			b.WriteString(": ")
			index[n].Type.renderInto(b)
		}
		b.WriteString(" }")
	case KindInterface:
		b.WriteString("interface ")
		b.WriteString(m.Name)
		b.WriteString(" { ")
		names := make([]string, 0, len(m.Methods))
		index := make(map[string]MochiMethod, len(m.Methods))
		for _, mm := range m.Methods {
			names = append(names, mm.Name)
			index[mm.Name] = mm
		}
		sort.Strings(names)
		for i, n := range names {
			if i > 0 {
				b.WriteString("; ")
			}
			mm := index[n]
			if mm.IsAsync {
				b.WriteString("async ")
			}
			b.WriteString(n)
			b.WriteString("(")
			for j, p := range mm.Params {
				if j > 0 {
					b.WriteString(", ")
				}
				p.renderInto(b)
			}
			b.WriteString("): ")
			mm.Return.renderInto(b)
		}
		b.WriteString(" }")
	default:
		b.WriteString("<unknown>")
	}
}

// Equal reports structural equality. Two MochiTypes are equal when their
// Kinds, Names, and recursive Params / Fields / Methods all match.
func (m MochiType) Equal(other MochiType) bool {
	if m.Kind != other.Kind || m.Name != other.Name {
		return false
	}
	if len(m.Params) != len(other.Params) {
		return false
	}
	for i := range m.Params {
		if !m.Params[i].Equal(other.Params[i]) {
			return false
		}
	}
	if len(m.Fields) != len(other.Fields) {
		return false
	}
	for i := range m.Fields {
		if m.Fields[i].Name != other.Fields[i].Name {
			return false
		}
		if !m.Fields[i].Type.Equal(other.Fields[i].Type) {
			return false
		}
	}
	if len(m.Methods) != len(other.Methods) {
		return false
	}
	for i := range m.Methods {
		a, c := m.Methods[i], other.Methods[i]
		if a.Name != c.Name || a.IsAsync != c.IsAsync {
			return false
		}
		if !a.Return.Equal(c.Return) {
			return false
		}
		if len(a.Params) != len(c.Params) {
			return false
		}
		for j := range a.Params {
			if !a.Params[j].Equal(c.Params[j]) {
				return false
			}
		}
	}
	return true
}
