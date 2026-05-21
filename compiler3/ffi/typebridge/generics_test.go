package typebridge

import (
	"go/types"
	"testing"
)

// TestConstraintAny confirms LookupConstraint returns ConstraintAny
// for the predeclared `any` and for an explicit empty interface.
func TestConstraintAny(t *testing.T) {
	got, ok := LookupConstraint(types.Universe.Lookup("any").Type())
	if !ok || got != ConstraintAny {
		t.Fatalf("any -> %v, ok=%v", got, ok)
	}
	iface := types.NewInterfaceType(nil, nil)
	iface.Complete()
	got, ok = LookupConstraint(iface)
	if !ok || got != ConstraintAny {
		t.Fatalf("interface{} -> %v, ok=%v", got, ok)
	}
}

// TestConstraintComparable confirms the predeclared `comparable`
// type is recognised.
func TestConstraintComparable(t *testing.T) {
	got, ok := LookupConstraint(types.Universe.Lookup("comparable").Type())
	if !ok || got != ConstraintComparable {
		t.Fatalf("comparable -> %v, ok=%v", got, ok)
	}
}

// TestConstraintOrdered loads the `cmp` package and looks up
// Ordered; the lookup must return ConstraintOrdered.
func TestConstraintOrdered(t *testing.T) {
	pkgs := loadStdPackages(t, "cmp")
	obj := pkgs[0].Scope().Lookup("Ordered")
	if obj == nil {
		t.Skip("cmp.Ordered not present in this Go install")
	}
	got, ok := LookupConstraint(obj.Type())
	if !ok || got != ConstraintOrdered {
		t.Fatalf("cmp.Ordered -> %v, ok=%v", got, ok)
	}
}

// TestConstraintUnknown checks that arbitrary named types do not
// produce a false positive.
func TestConstraintUnknown(t *testing.T) {
	pkgs := loadStdPackages(t, "io")
	rdr := findExportedType(t, pkgs[0], "Reader")
	got, ok := LookupConstraint(rdr)
	if ok {
		t.Fatalf("io.Reader unexpectedly recognised as constraint: %v", got)
	}
}

// TestGenericInstanceCarriesTypeArgs loads slices.Index and asserts
// that a *types.Named instance carries TypeArgs. This is the
// resolver hook for Phase 2 generic call-site instantiation.
func TestGenericInstanceCarriesTypeArgs(t *testing.T) {
	// We exercise generic instantiation through a synthetic package
	// since stdlib generic instantiations would require a call site.
	src := `package g
type Pair[K comparable, V any] struct {
	Key K
	Val V
}
var P Pair[string, int]
`
	pkg := loadFromSource(t, "g", src)
	obj := pkg.Scope().Lookup("P")
	if obj == nil {
		t.Fatal("P missing")
	}
	mt := GoToMochi(obj.Type())
	if mt.Kind != KindNamed {
		t.Fatalf("kind = %s", mt.Kind)
	}
	if len(mt.TypeArgs) != 2 {
		t.Fatalf("type args = %d, want 2", len(mt.TypeArgs))
	}
	if mt.TypeArgs[0].Kind != KindString {
		t.Errorf("first arg = %s, want string", mt.TypeArgs[0].Kind)
	}
	if mt.TypeArgs[1].Kind != KindInt {
		t.Errorf("second arg = %s, want int", mt.TypeArgs[1].Kind)
	}
	if MochiToGo(mt) != "g.Pair[string, int]" {
		t.Errorf("MochiToGo = %q", MochiToGo(mt))
	}
}

// TestGenericFreeTypeParamMapsToKindTypeParam verifies that an
// uninstantiated *types.TypeParam maps to KindTypeParam.
func TestGenericFreeTypeParamMapsToKindTypeParam(t *testing.T) {
	src := `package g
func Map[T any](xs []T) []T { return xs }
`
	pkg := loadFromSource(t, "g", src)
	obj := pkg.Scope().Lookup("Map")
	sig := obj.Type().(*types.Signature)
	tp := sig.TypeParams().At(0)
	mt := GoToMochi(tp)
	if mt.Kind != KindTypeParam {
		t.Fatalf("type param kind = %s", mt.Kind)
	}
	if mt.Name != "T" {
		t.Fatalf("name = %q", mt.Name)
	}
}
