package types

import "testing"

// MEP-11.1 fixtures the Subtype predicate against the rules in
// website/docs/mep/mep-0011.md §Specification.

func TestSubtype_Reflexivity(t *testing.T) {
	cases := []Type{
		IntType{}, Int64Type{}, BigIntType{}, BigRatType{}, FloatType{},
		StringType{}, BoolType{}, UnitType{}, AnyType{},
		ListType{Elem: IntType{}},
		MapType{Key: StringType{}, Value: IntType{}},
		OptionType{Elem: IntType{}},
	}
	for _, c := range cases {
		if !Subtype(c, c) {
			t.Errorf("Subtype(%s, %s) want true", c, c)
		}
	}
}

func TestSubtype_AnyIsTop(t *testing.T) {
	// MEP-11 T-Top: every concrete type widens to any.
	if !Subtype(IntType{}, AnyType{}) {
		t.Error("Subtype(int, any) want true")
	}
	if !Subtype(ListType{Elem: IntType{}}, AnyType{}) {
		t.Error("Subtype([int], any) want true")
	}
	// MEP-10 A1 closeout: any does NOT silently flow into a concrete
	// type. An explicit cast is required at the call site.
	if Subtype(AnyType{}, IntType{}) {
		t.Error("Subtype(any, int) want false (MEP-10 A1)")
	}
	if Subtype(AnyType{}, ListType{Elem: IntType{}}) {
		t.Error("Subtype(any, [int]) want false (MEP-10 A1)")
	}
}

func TestSubtype_NumericTower(t *testing.T) {
	// MEP-11 T-NumTower: int <: int64 <: bigint <: bigrat, float <: bigrat.
	want := []struct {
		s, t Type
		ok   bool
	}{
		{IntType{}, Int64Type{}, true},
		{IntType{}, BigIntType{}, true},
		{IntType{}, BigRatType{}, true},
		{Int64Type{}, BigIntType{}, true},
		{Int64Type{}, BigRatType{}, true},
		{BigIntType{}, BigRatType{}, true},
		{FloatType{}, BigRatType{}, true},

		// Widening only flows in one direction.
		{Int64Type{}, IntType{}, false},
		{BigIntType{}, IntType{}, false},
		{BigRatType{}, FloatType{}, false},
		{FloatType{}, IntType{}, false},
		{IntType{}, FloatType{}, false},
	}
	for _, c := range want {
		got := Subtype(c.s, c.t)
		if got != c.ok {
			t.Errorf("Subtype(%s, %s) = %v want %v", c.s, c.t, got, c.ok)
		}
	}
}

func TestSubtype_ListCovariant(t *testing.T) {
	// T-List-Read: covariant in read position.
	if !Subtype(ListType{Elem: IntType{}}, ListType{Elem: BigIntType{}}) {
		t.Error("[int] <: [bigint] want true")
	}
	if Subtype(ListType{Elem: BigIntType{}}, ListType{Elem: IntType{}}) {
		t.Error("[bigint] <: [int] want false")
	}
	// [int] <: [any] but [any] is not <: [int].
	if !Subtype(ListType{Elem: IntType{}}, ListType{Elem: AnyType{}}) {
		t.Error("[int] <: [any] want true")
	}
	if Subtype(ListType{Elem: AnyType{}}, ListType{Elem: IntType{}}) {
		t.Error("[any] <: [int] want false")
	}
}

func TestSubtype_MapInvariant(t *testing.T) {
	// T-Map-Inv: maps are invariant in both children.
	if Subtype(
		MapType{Key: StringType{}, Value: IntType{}},
		MapType{Key: StringType{}, Value: BigIntType{}},
	) {
		t.Error("{string:int} <: {string:bigint} want false (invariant)")
	}
	if Subtype(
		MapType{Key: StringType{}, Value: IntType{}},
		MapType{Key: StringType{}, Value: AnyType{}},
	) {
		t.Error("{string:int} <: {string:any} want false (invariant)")
	}
	if !Subtype(
		MapType{Key: StringType{}, Value: IntType{}},
		MapType{Key: StringType{}, Value: IntType{}},
	) {
		t.Error("{string:int} <: {string:int} want true")
	}
}

func TestSubtype_OptionCovariant(t *testing.T) {
	// T-Option-Cov: option[S] <: option[T] when S <: T.
	if !Subtype(OptionType{Elem: IntType{}}, OptionType{Elem: BigIntType{}}) {
		t.Error("option[int] <: option[bigint] want true")
	}
	if Subtype(OptionType{Elem: BigIntType{}}, OptionType{Elem: IntType{}}) {
		t.Error("option[bigint] <: option[int] want false")
	}
}

func TestSubtype_FunctionVariance(t *testing.T) {
	// T-Fun: contravariant args, covariant result.
	// (bigint) -> int  <:  (int) -> bigint
	wider := FuncType{Params: []Type{BigIntType{}}, Return: IntType{}}
	narrower := FuncType{Params: []Type{IntType{}}, Return: BigIntType{}}
	if !Subtype(wider, narrower) {
		t.Error("(bigint)->int <: (int)->bigint want true (T-Fun)")
	}
	if Subtype(narrower, wider) {
		t.Error("(int)->bigint <: (bigint)->int want false")
	}
}

func TestSubtype_VariantToUnion(t *testing.T) {
	// T-Variant: a declared variant struct is a subtype of its union.
	leaf := StructType{Name: "Leaf"}
	node := StructType{Name: "Node"}
	tree := UnionType{
		Name:     "Tree",
		Variants: map[string]StructType{"Leaf": leaf, "Node": node},
		Order:    []string{"Leaf", "Node"},
	}
	if !Subtype(leaf, tree) {
		t.Error("Leaf <: Tree want true")
	}
	other := StructType{Name: "Other"}
	if Subtype(other, tree) {
		t.Error("Other <: Tree want false (not a declared variant)")
	}
}

func TestSubtype_StructNominal(t *testing.T) {
	// T-Struct-Nominal: identity by name only.
	a := StructType{Name: "P", Fields: []StructField{{Name: "x", Type: IntType{}}}}
	b := StructType{Name: "P", Fields: []StructField{{Name: "x", Type: IntType{}}}}
	c := StructType{Name: "Q", Fields: []StructField{{Name: "x", Type: IntType{}}}}
	if !Subtype(a, b) {
		t.Error("P <: P want true")
	}
	if Subtype(a, c) {
		t.Error("P <: Q want false (nominal)")
	}
}

func TestSubtype_StringBoolUnit(t *testing.T) {
	if Subtype(StringType{}, IntType{}) {
		t.Error("string <: int want false")
	}
	if Subtype(BoolType{}, IntType{}) {
		t.Error("bool <: int want false")
	}
	if Subtype(UnitType{}, AnyType{}) != true {
		t.Error("unit <: any want true")
	}
}
