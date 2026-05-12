package types

import "testing"

// Pin the post-MEP 4 P15 contract: equalTypes is a closed switch over the
// known kinds (no reflect.DeepEqual fallback), so adding a new kind is a
// deliberate edit rather than a silent acceptance. Each kind here has a
// matching-equal case and a cross-kind not-equal case to lock the
// per-kind behaviour.
func TestEqualTypesByKind(t *testing.T) {
	tv1 := &TypeVar{Name: "T"}
	tv2 := &TypeVar{Name: "T"}

	cases := []struct {
		name string
		a, b Type
		want bool
	}{
		// Reflexive primitives.
		{"int=int", IntType{}, IntType{}, true},
		// P1 (task #104): carve-out removed. equalTypes is strict on kind.
		{"int=int64 (strict)", IntType{}, Int64Type{}, false},
		{"int=float", IntType{}, FloatType{}, false},
		{"float=float", FloatType{}, FloatType{}, true},
		{"bigint=bigint", BigIntType{}, BigIntType{}, true},
		{"bigrat=bigrat", BigRatType{}, BigRatType{}, true},
		{"string=string", StringType{}, StringType{}, true},
		{"string=int", StringType{}, IntType{}, false},
		{"bool=bool", BoolType{}, BoolType{}, true},
		{"unit=unit", UnitType{}, UnitType{}, true},
		{"unit=int", UnitType{}, IntType{}, false},
		{"any=any", AnyType{}, AnyType{}, true},
		{"any=int", AnyType{}, IntType{}, false},
		// Structural kinds.
		{"list elem matches", ListType{Elem: IntType{}}, ListType{Elem: IntType{}}, true},
		{"list elem differs", ListType{Elem: IntType{}}, ListType{Elem: StringType{}}, false},
		{"map matches", MapType{Key: StringType{}, Value: IntType{}}, MapType{Key: StringType{}, Value: IntType{}}, true},
		{"map value differs", MapType{Key: StringType{}, Value: IntType{}}, MapType{Key: StringType{}, Value: StringType{}}, false},
		{"option matches", OptionType{Elem: IntType{}}, OptionType{Elem: IntType{}}, true},
		{"option elem differs", OptionType{Elem: IntType{}}, OptionType{Elem: StringType{}}, false},
		{"group matches", GroupType{Key: StringType{}, Elem: IntType{}}, GroupType{Key: StringType{}, Elem: IntType{}}, true},
		// Struct/union name-based equality and struct-in-union carve-out.
		{
			"struct same name",
			StructType{Name: "User", Fields: []StructField{{Name: "id", Type: IntType{}}}},
			StructType{Name: "User", Fields: []StructField{{Name: "id", Type: IntType{}}}},
			true,
		},
		{
			"struct different name",
			StructType{Name: "User"},
			StructType{Name: "Admin"},
			false,
		},
		{
			"struct equals union it is a variant of",
			StructType{Name: "Circle"},
			UnionType{Name: "Shape", Variants: map[string]StructType{"Circle": {Name: "Circle"}}},
			true,
		},
		{
			"union equals struct that is one of its variants",
			UnionType{Name: "Shape", Variants: map[string]StructType{"Square": {Name: "Square"}}},
			StructType{Name: "Square"},
			true,
		},
		{
			"struct does not equal unrelated union",
			StructType{Name: "Triangle"},
			UnionType{Name: "Shape", Variants: map[string]StructType{"Circle": {Name: "Circle"}}},
			false,
		},
		// Functions: arity, variadicity, return, and per-param equality.
		{
			"func match",
			FuncType{Params: []Type{IntType{}}, Return: BoolType{}},
			FuncType{Params: []Type{IntType{}}, Return: BoolType{}},
			true,
		},
		{
			"func variadic mismatch",
			FuncType{Params: []Type{IntType{}}, Return: BoolType{}, Variadic: IntType{}},
			FuncType{Params: []Type{IntType{}}, Return: BoolType{}},
			false,
		},
		{
			"func return mismatch",
			FuncType{Params: []Type{IntType{}}, Return: BoolType{}},
			FuncType{Params: []Type{IntType{}}, Return: IntType{}},
			false,
		},
		// TypeVar identity by pointer, not name.
		{"typevar same pointer", tv1, tv1, true},
		{"typevar same name different pointer", tv1, tv2, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := equalTypes(tc.a, tc.b); got != tc.want {
				t.Fatalf("equalTypes(%s, %s) = %v, want %v", tc.a, tc.b, got, tc.want)
			}
			// Symmetry: equalTypes must agree in both directions.
			if got := equalTypes(tc.b, tc.a); got != tc.want {
				t.Fatalf("equalTypes(%s, %s) = %v (reverse), want %v", tc.b, tc.a, got, tc.want)
			}
		})
	}
}
