package types

import "testing"

// MEP-10 B2: numeric mix lattice. The join must be symmetric in its
// operands so `bigint - float` and `float - bigint` produce the same
// result type.

func TestNumericJoin_Symmetric(t *testing.T) {
	pairs := []struct {
		a, b Type
		want Type
	}{
		{IntType{}, IntType{}, IntType{}},
		{IntType{}, Int64Type{}, Int64Type{}},
		{IntType{}, BigIntType{}, BigIntType{}},
		{IntType{}, FloatType{}, FloatType{}},
		{IntType{}, BigRatType{}, BigRatType{}},

		{Int64Type{}, Int64Type{}, Int64Type{}},
		{Int64Type{}, BigIntType{}, BigIntType{}},
		{Int64Type{}, FloatType{}, FloatType{}},
		{Int64Type{}, BigRatType{}, BigRatType{}},

		{BigIntType{}, BigIntType{}, BigIntType{}},
		{BigIntType{}, FloatType{}, BigRatType{}}, // meet at bigrat
		{BigIntType{}, BigRatType{}, BigRatType{}},

		{FloatType{}, FloatType{}, FloatType{}},
		{FloatType{}, BigRatType{}, BigRatType{}},

		{BigRatType{}, BigRatType{}, BigRatType{}},
	}
	for _, p := range pairs {
		got, ok := numericJoin(p.a, p.b)
		if !ok {
			t.Errorf("join(%s, %s) returned !ok", p.a, p.b)
			continue
		}
		if got.String() != p.want.String() {
			t.Errorf("join(%s, %s) = %s want %s", p.a, p.b, got, p.want)
		}
		// Symmetry obligation: swap operands and assert same result.
		got2, _ := numericJoin(p.b, p.a)
		if got.String() != got2.String() {
			t.Errorf("join(%s, %s) = %s but join(%s, %s) = %s",
				p.a, p.b, got, p.b, p.a, got2)
		}
	}
}

func TestNumericJoin_RejectsNonNumeric(t *testing.T) {
	cases := []struct{ a, b Type }{
		{IntType{}, StringType{}},
		{BoolType{}, IntType{}},
		{ListType{Elem: IntType{}}, IntType{}},
	}
	for _, c := range cases {
		if _, ok := numericJoin(c.a, c.b); ok {
			t.Errorf("join(%s, %s) should reject non-numeric", c.a, c.b)
		}
	}
}
