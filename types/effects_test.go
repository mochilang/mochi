package types_test

import (
	"testing"
	"mochi/types"
)

func TestEffectSet_Basics(t *testing.T) {
	if !types.EmptyEffects.IsEmpty() {
		t.Fatalf("EmptyEffects should be empty")
	}
	s := types.NewEffectSet(types.EffectIO, types.EffectTime)
	if !s.Has(types.EffectIO) || !s.Has(types.EffectTime) {
		t.Fatalf("expected io+time, got %s", s.String())
	}
	if s.Has(types.EffectFS) {
		t.Fatalf("expected no fs label")
	}
	if got, want := s.String(), "io, time"; got != want {
		t.Fatalf("String=%q want %q", got, want)
	}
	if got, want := types.EmptyEffects.String(), "pure"; got != want {
		t.Fatalf("empty.String=%q want %q", got, want)
	}
}

func TestEffectSet_Union_Subset(t *testing.T) {
	a := types.NewEffectSet(types.EffectIO)
	b := types.NewEffectSet(types.EffectFS)
	c := a.Union(b)
	if !a.IsSubset(c) || !b.IsSubset(c) {
		t.Fatalf("union missing labels: %s", c.String())
	}
	if c.IsSubset(a) {
		t.Fatalf("c={io,fs} should not be subset of {io}")
	}
}

func TestParseEffectLabel(t *testing.T) {
	for _, name := range []string{"io", "fs", "net", "time", "meta"} {
		if _, ok := types.ParseEffectLabel(name); !ok {
			t.Fatalf("expected %q to parse", name)
		}
	}
	if _, ok := types.ParseEffectLabel("nope"); ok {
		t.Fatalf("unknown label should fail")
	}
}

func TestFuncTypePure(t *testing.T) {
	pure := types.FuncType{Params: []types.Type{}, Return: types.IntType{}}
	if !pure.Pure() {
		t.Fatalf("empty Effects should be pure")
	}
	impure := types.FuncType{Params: []types.Type{}, Return: types.IntType{}, Effects: types.NewEffectSet(types.EffectIO)}
	if impure.Pure() {
		t.Fatalf("io should not be pure")
	}
}
