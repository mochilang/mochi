package types_test

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

// TestUnionTypeOrder pins MEP-4 P11: UnionType.Order preserves the
// declaration order of variants. A future change that drops the Order
// field or populates it inconsistently must update this test.
func TestUnionTypeOrder(t *testing.T) {
	src := `
type Shape = Circle(r: float) | Square(side: float) | Triangle(a: int, b: int, c: int)
`
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) != 0 {
		t.Fatalf("check: %v", errs)
	}
	ut, ok := env.GetUnion("Shape")
	if !ok {
		t.Fatalf("union Shape not in env")
	}
	want := []string{"Circle", "Square", "Triangle"}
	if got := ut.Order; len(got) != len(want) {
		t.Fatalf("Order len=%d, want %d (got %v)", len(got), len(want), got)
	}
	for i, name := range want {
		if ut.Order[i] != name {
			t.Errorf("Order[%d] = %q, want %q", i, ut.Order[i], name)
		}
	}
	for _, name := range want {
		if _, ok := ut.Variants[name]; !ok {
			t.Errorf("variant %q missing from Variants map", name)
		}
	}
}
