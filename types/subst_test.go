package types

import "testing"

// MEP-12.1 fixtures the substitution algebra and the new Unify against
// the rules in website/docs/mep/mep-0012.md §Algorithm.

func TestApply_LeavesFreeVars(t *testing.T) {
	tv := &TypeVar{Name: "T"}
	sub := Subst{}
	if got := sub.Apply(tv); got != tv {
		t.Errorf("free var should be returned unchanged")
	}
}

func TestApply_SubstitutesBoundVar(t *testing.T) {
	tv := &TypeVar{Name: "T"}
	sub := Subst{"T": IntType{}}
	got := sub.Apply(tv)
	if _, ok := got.(IntType); !ok {
		t.Errorf("Apply did not substitute T ↦ int, got %s", got)
	}
}

func TestApply_ChainsTransitively(t *testing.T) {
	sub := Subst{
		"T": &TypeVar{Name: "U"},
		"U": StringType{},
	}
	got := sub.Apply(&TypeVar{Name: "T"})
	if _, ok := got.(StringType); !ok {
		t.Errorf("Apply did not chain T -> U -> string, got %s", got)
	}
}

func TestApply_WalksList(t *testing.T) {
	sub := Subst{"T": IntType{}}
	got := sub.Apply(ListType{Elem: &TypeVar{Name: "T"}})
	lt, ok := got.(ListType)
	if !ok {
		t.Fatalf("want ListType, got %T", got)
	}
	if _, ok := lt.Elem.(IntType); !ok {
		t.Errorf("want [int], got %s", got)
	}
}

func TestApply_WalksFunc(t *testing.T) {
	sub := Subst{"T": IntType{}, "U": StringType{}}
	got := sub.Apply(FuncType{
		Params: []Type{&TypeVar{Name: "T"}},
		Return: &TypeVar{Name: "U"},
	})
	ft := got.(FuncType)
	if _, ok := ft.Params[0].(IntType); !ok {
		t.Errorf("want param int, got %s", ft.Params[0])
	}
	if _, ok := ft.Return.(StringType); !ok {
		t.Errorf("want return string, got %s", ft.Return)
	}
}

func TestCompose_AppliesLeftToRight(t *testing.T) {
	left := Subst{"U": StringType{}}
	right := Subst{"T": &TypeVar{Name: "U"}}
	c := left.Compose(right)
	got := c.Apply(&TypeVar{Name: "T"})
	if _, ok := got.(StringType); !ok {
		t.Errorf("compose did not chain T -> U -> string, got %s", got)
	}
}

func TestBind_OccursCheck(t *testing.T) {
	sub := Subst{}
	err := sub.Bind("T", ListType{Elem: &TypeVar{Name: "T"}})
	if err == nil {
		t.Error("expected occurs check failure for T ↦ [T]")
	}
}

func TestBind_OccursCheckThroughSubstitution(t *testing.T) {
	// T ↦ U, then U ↦ [T] should fail the occurs check transitively.
	sub := Subst{"U": &TypeVar{Name: "T"}}
	err := sub.Bind("T", ListType{Elem: &TypeVar{Name: "U"}})
	if err == nil {
		t.Error("expected occurs check failure for transitive cycle")
	}
}

func TestBind_SelfIsNoOp(t *testing.T) {
	sub := Subst{}
	if err := sub.Bind("T", &TypeVar{Name: "T"}); err != nil {
		t.Errorf("self-bind should succeed silently, got %v", err)
	}
	if _, ok := sub["T"]; ok {
		t.Errorf("self-bind should not record a mapping")
	}
}

func TestUnify_TypeVarOnLeft(t *testing.T) {
	sub, err := Unify(&TypeVar{Name: "T"}, IntType{}, nil)
	if err != nil {
		t.Fatalf("unify failed: %v", err)
	}
	if _, ok := sub["T"].(IntType); !ok {
		t.Errorf("want T ↦ int, got %v", sub)
	}
}

func TestUnify_TypeVarOnRight(t *testing.T) {
	sub, err := Unify(IntType{}, &TypeVar{Name: "T"}, nil)
	if err != nil {
		t.Fatalf("unify failed: %v", err)
	}
	if _, ok := sub["T"].(IntType); !ok {
		t.Errorf("want T ↦ int, got %v", sub)
	}
}

func TestUnify_PropagatesAcrossArgs(t *testing.T) {
	// Simulates `pair(1, 1)` where both arguments bind T to int.
	sub, err := Unify(&TypeVar{Name: "T"}, IntType{}, nil)
	if err != nil {
		t.Fatalf("first unify failed: %v", err)
	}
	sub, err = Unify(&TypeVar{Name: "T"}, IntType{}, sub)
	if err != nil {
		t.Fatalf("second unify failed: %v", err)
	}
	if _, ok := sub["T"].(IntType); !ok {
		t.Errorf("want T ↦ int, got %v", sub)
	}
}

func TestUnify_ConflictAcrossArgs(t *testing.T) {
	// Simulates `pair(1, "a")` where T binds to two different types.
	sub, err := Unify(&TypeVar{Name: "T"}, IntType{}, nil)
	if err != nil {
		t.Fatalf("first unify failed: %v", err)
	}
	_, err = Unify(&TypeVar{Name: "T"}, StringType{}, sub)
	if err == nil {
		t.Error("expected T047 conflict for T ↦ int then T ↦ string")
	}
}

func TestUnify_AnyAbsorbs(t *testing.T) {
	// AnyType keeps the legacy escape-hatch behaviour; the strict
	// rule lives in Subtype.
	if _, err := Unify(AnyType{}, IntType{}, nil); err != nil {
		t.Errorf("unify(any, int) want success: %v", err)
	}
	if _, err := Unify(IntType{}, AnyType{}, nil); err != nil {
		t.Errorf("unify(int, any) want success: %v", err)
	}
}

func TestUnify_ListRecurses(t *testing.T) {
	sub, err := Unify(
		ListType{Elem: &TypeVar{Name: "T"}},
		ListType{Elem: IntType{}},
		nil,
	)
	if err != nil {
		t.Fatalf("unify failed: %v", err)
	}
	if _, ok := sub["T"].(IntType); !ok {
		t.Errorf("want T ↦ int via list, got %v", sub)
	}
}

func TestUnify_FuncStructural(t *testing.T) {
	sub, err := Unify(
		FuncType{Params: []Type{&TypeVar{Name: "T"}}, Return: &TypeVar{Name: "T"}},
		FuncType{Params: []Type{IntType{}}, Return: IntType{}},
		nil,
	)
	if err != nil {
		t.Fatalf("unify failed: %v", err)
	}
	if _, ok := sub["T"].(IntType); !ok {
		t.Errorf("want T ↦ int, got %v", sub)
	}
}

func TestUnify_OccursCheckRejects(t *testing.T) {
	_, err := Unify(
		&TypeVar{Name: "T"},
		ListType{Elem: &TypeVar{Name: "T"}},
		nil,
	)
	if err == nil {
		t.Error("expected occurs check failure for T ↦ [T]")
	}
}

func TestUnify_Mismatch(t *testing.T) {
	if _, err := Unify(IntType{}, StringType{}, nil); err == nil {
		t.Error("expected mismatch error for int vs string")
	}
}

func TestUnify_DoesNotMutateInput(t *testing.T) {
	sub := Subst{}
	if _, err := Unify(&TypeVar{Name: "T"}, IntType{}, sub); err != nil {
		t.Fatalf("unify failed: %v", err)
	}
	if _, ok := sub["T"]; ok {
		t.Errorf("Unify must not mutate the input substitution")
	}
}
