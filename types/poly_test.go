package types

import (
	"reflect"
	"testing"
)

// MEP-5 P11 / MEP-12 §Algorithm: generalise / instantiate primitives.

func TestFreshTypeVar_UniqueAcrossCalls(t *testing.T) {
	ResetFreshCounter()
	a := FreshTypeVar("T")
	b := FreshTypeVar("T")
	if a == b {
		t.Error("FreshTypeVar must return distinct pointers")
	}
	if a.Name == b.Name {
		t.Errorf("FreshTypeVar names collided: %s vs %s", a.Name, b.Name)
	}
}

func TestInstantiate_ReplacesNamedParams(t *testing.T) {
	ResetFreshCounter()
	scheme := FuncType{
		Params: []Type{&TypeVar{Name: "T"}, &TypeVar{Name: "U"}},
		Return: &TypeVar{Name: "T"},
	}
	got, sub := Instantiate(scheme, []string{"T", "U"})
	ft := got.(FuncType)
	tv0 := ft.Params[0].(*TypeVar)
	tv1 := ft.Params[1].(*TypeVar)
	if tv0.Name == "T" || tv1.Name == "U" {
		t.Errorf("Instantiate left original names: %v", ft)
	}
	if !reflect.DeepEqual(ft.Return, tv0) {
		t.Errorf("return should share the fresh T variable, got %s vs %s",
			ft.Return, tv0)
	}
	if _, ok := sub["T"]; !ok {
		t.Errorf("substitution missing T binding: %v", sub)
	}
}

func TestInstantiate_LeavesUnboundVariables(t *testing.T) {
	scheme := ListType{Elem: &TypeVar{Name: "X"}}
	got, _ := Instantiate(scheme, []string{"T"})
	if !reflect.DeepEqual(got, scheme) {
		t.Errorf("non-parameter var X should not be rewritten, got %s", got)
	}
}

func TestInstantiate_EmptyParams(t *testing.T) {
	scheme := IntType{}
	got, sub := Instantiate(scheme, nil)
	if !reflect.DeepEqual(got, scheme) {
		t.Errorf("Instantiate with no params should return t unchanged")
	}
	if len(sub) != 0 {
		t.Errorf("empty params should produce empty substitution")
	}
}

func TestFreeTypeVars_Sorted(t *testing.T) {
	t1 := &TypeVar{Name: "T"}
	t2 := &TypeVar{Name: "A"}
	got := FreeTypeVars(FuncType{Params: []Type{t1, t2}, Return: t1}, nil)
	want := []string{"A", "T"}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("want %v sorted, got %v", want, got)
	}
}

func TestFreeTypeVars_SkipsBound(t *testing.T) {
	sub := Subst{"T": IntType{}}
	got := FreeTypeVars(ListType{Elem: &TypeVar{Name: "T"}}, sub)
	if len(got) != 0 {
		t.Errorf("bound T should not appear in free vars, got %v", got)
	}
}

func TestFreeTypeVars_FollowsTransitiveSubst(t *testing.T) {
	sub := Subst{"T": &TypeVar{Name: "U"}}
	got := FreeTypeVars(&TypeVar{Name: "T"}, sub)
	if !reflect.DeepEqual(got, []string{"U"}) {
		t.Errorf("want [U] after chasing T -> U, got %v", got)
	}
}

func TestGeneralise_DropsCapturedNames(t *testing.T) {
	parent := NewEnv(nil)
	parent.SetVar("outer", &TypeVar{Name: "T"}, false)
	env := NewEnv(parent)

	body := FuncType{
		Params: []Type{&TypeVar{Name: "T"}, &TypeVar{Name: "U"}},
		Return: &TypeVar{Name: "U"},
	}
	params, _ := Generalise(body, env)
	if reflect.DeepEqual(params, []string{"T", "U"}) {
		t.Errorf("T is captured by outer scope; should not be generalised: %v", params)
	}
	want := []string{"U"}
	if !reflect.DeepEqual(params, want) {
		t.Errorf("want %v, got %v", want, params)
	}
}
