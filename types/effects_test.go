package types_test

import (
	"mochi/parser"
	"mochi/types"
	"testing"
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

// TestInferFunctionEffects exercises the MEP-15 Stage 2 body walker
// via the public Check pipeline. Each row registers a single function
// and asserts the effect set that propagates back through the env
// after Check completes the fixpoint sweep.
func TestInferFunctionEffects(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want types.EffectSet
	}{
		{
			name: "pure_arith",
			src:  "fun add(x: int, y: int): int { return x + y }",
			want: types.EmptyEffects,
		},
		{
			name: "print_propagates_io",
			src:  "fun greet(name: string) { print(name) }",
			want: types.NewEffectSet(types.EffectIO),
		},
		{
			name: "now_propagates_time",
			src:  "fun stamp(): int { return now() }",
			want: types.NewEffectSet(types.EffectTime),
		},
		{
			name: "eval_propagates_meta",
			src:  "fun dyn(s: string): any { return eval(s) }",
			want: types.NewEffectSet(types.EffectMeta),
		},
		{
			name: "transitive_via_helper",
			src: `
fun shout(s: string) { print(s) }
fun greet(name: string) { shout(name) }
`,
			want: types.NewEffectSet(types.EffectIO),
		},
		{
			name: "union_of_branches",
			src: `
fun snapshot(name: string): int {
  print(name)
  return now()
}
`,
			want: types.NewEffectSet(types.EffectIO, types.EffectTime),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog, err := parser.ParseString(tc.src)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("Check: %v", errs)
			}
			target := "greet"
			switch tc.name {
			case "pure_arith":
				target = "add"
			case "now_propagates_time":
				target = "stamp"
			case "eval_propagates_meta":
				target = "dyn"
			case "union_of_branches":
				target = "snapshot"
			}
			t2, err := env.GetVar(target)
			if err != nil {
				t.Fatalf("GetVar(%q): %v", target, err)
			}
			ft, ok := t2.(types.FuncType)
			if !ok {
				t.Fatalf("expected FuncType for %q, got %T", target, t2)
			}
			if ft.Effects != tc.want {
				t.Fatalf("%s: Effects=%s want %s", tc.name, ft.Effects.String(), tc.want.String())
			}
		})
	}
}
