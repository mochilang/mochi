package types

import (
	"fmt"

	"mochi/parser"
)

// --- Type System ---

type Type interface {
	String() string
}

type IntType struct{}

func (IntType) String() string { return "int" }

// Int64Type specifically represents 64-bit integers. It unifies with IntType
// for most operations but allows the Go compiler to emit int64 values where
// precision matters (e.g. now()).
type Int64Type struct{}

func (Int64Type) String() string { return "int64" }

type FloatType struct{}

func (FloatType) String() string { return "float" }

type BigIntType struct{}

func (BigIntType) String() string { return "bigint" }

type BigRatType struct{}

func (BigRatType) String() string { return "bigrat" }

type StringType struct{}

func (StringType) String() string { return "string" }

type BoolType struct{}

func (BoolType) String() string { return "bool" }

type UnitType struct{}

func (UnitType) String() string { return "unit" }

type ListType struct {
	Elem Type
}

func (t ListType) String() string { return "[" + t.Elem.String() + "]" }

type MapType struct {
	Key   Type
	Value Type
}

func (t MapType) String() string {
	return fmt.Sprintf("{%s: %s}", t.Key.String(), t.Value.String())
}

type OptionType struct {
	Elem Type
}

func (t OptionType) String() string { return fmt.Sprintf("option[%s]", t.Elem.String()) }

type GroupType struct {
	Key  Type
	Elem Type
}

func (GroupType) String() string { return "group" }

// StructField is a single declared field of a StructType. The slice of
// fields on StructType is ordered by declaration sequence so JSON
// encoding and pretty-printing stay deterministic without a parallel
// `Order` slice (MEP 4 P10).
type StructField struct {
	Name string
	Type Type
}

// StructType is the type of a nominal record. Fields are stored as a
// single ordered slice that doubles as the iteration order and the
// lookup table (via helper methods). The previous representation kept
// `Fields map[string]Type` plus `Order []string` in parallel, which
// could drift; MEP 4 P10 consolidates them.
type StructType struct {
	Name    string
	Fields  []StructField
	Methods map[string]Method
}

type Method struct {
	Decl *parser.FunStmt
	Type FuncType
}

func (t StructType) String() string { return t.Name }

// FieldType returns the declared type of the named field, or nil and
// false if the field is not present.
func (t StructType) FieldType(name string) (Type, bool) {
	for _, f := range t.Fields {
		if f.Name == name {
			return f.Type, true
		}
	}
	return nil, false
}

// HasField reports whether the named field is declared on the struct.
func (t StructType) HasField(name string) bool {
	_, ok := t.FieldType(name)
	return ok
}

// FieldNames returns the field names in declaration order.
func (t StructType) FieldNames() []string {
	names := make([]string, len(t.Fields))
	for i, f := range t.Fields {
		names[i] = f.Name
	}
	return names
}

// FieldMap returns a fresh lookup map of field name to declared type.
// Callers that perform many lookups against the same struct should
// cache the result.
func (t StructType) FieldMap() map[string]Type {
	m := make(map[string]Type, len(t.Fields))
	for _, f := range t.Fields {
		m[f.Name] = f.Type
	}
	return m
}

// WithField returns a copy of t with the named field's type set to
// ftype. If the field is already declared the type is updated in
// place (preserving order); otherwise the field is appended.
func (t StructType) WithField(name string, ftype Type) StructType {
	fields := make([]StructField, len(t.Fields))
	copy(fields, t.Fields)
	for i, f := range fields {
		if f.Name == name {
			fields[i].Type = ftype
			out := t
			out.Fields = fields
			return out
		}
	}
	out := t
	out.Fields = append(fields, StructField{Name: name, Type: ftype})
	return out
}

// NewStructType is a convenience constructor for an ordered field set
// passed as alternating (name, type) pairs.
func NewStructType(name string, fields ...StructField) StructType {
	out := StructType{Name: name}
	out.Fields = append([]StructField(nil), fields...)
	return out
}

type UnionType struct {
	Name string
	// Variants stores the per-variant struct type keyed by variant name.
	// Order preserves the declaration order of variants and is the
	// canonical iteration sequence (MEP 4 P11). Variants is kept in
	// sync but is not authoritative for ordering.
	Variants map[string]StructType
	Order    []string
}

func (t UnionType) String() string { return t.Name }

type AnyType struct{}

func (AnyType) String() string { return "any" }

// TypeVar is the polymorphism kind used to represent a generic type
// parameter. Its methods take a pointer receiver because identity, not
// value, is what distinguishes two type variables: a fresh `*TypeVar`
// with the same `Name` as an existing one must compare unequal under
// unification. Every other kind in this package is a value type with
// value receivers. If a future kind needs the same identity semantics
// (a `RowVar` for row polymorphism is the obvious candidate, planned
// under MEP 11), it should follow the same convention so callers can
// rely on a single discriminator (`x.(*TypeVar)` vs `x.(SomeValueKind)`).
// See MEP 4 §6 problem 14 and MEP 12.
type TypeVar struct {
	Name string
}

func (t *TypeVar) String() string { return t.Name }

// FuncType is the type of a function value. Params is the fixed prefix
// of parameter types. Variadic is the element type of the trailing
// varargs sequence, or nil if the function is not variadic (MEP 4 P13).
// Pure is the inferred purity flag (MEP 4 P7).
type FuncType struct {
	Params   []Type
	Return   Type
	Pure     bool
	Variadic Type
	// TypeParams lists the names of TypeVars quantified at this
	// signature (MEP-12). A non-empty value means the function is
	// generic: the call site freshens these names via Instantiate before
	// unifying arguments. Non-generic functions leave the field nil.
	TypeParams []string
}

func (f FuncType) String() string {
	s := "fun("
	for i, p := range f.Params {
		if i > 0 {
			s += ", "
		}
		s += p.String()
	}
	if f.Variadic != nil {
		if len(f.Params) > 0 {
			s += ", ..." + f.Variadic.String()
		} else {
			s += "..." + f.Variadic.String()
		}
	}
	s += ")"
	if f.Return != nil && f.Return.String() != "unit" {
		s += ": " + f.Return.String()
	}
	if f.Pure {
		s += " [pure]"
	}
	return s
}

type Subst map[string]Type
