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

// TestDeclaredEffects_Accepted exercises MEP-15 Stage 2b: a function
// that annotates an effect set the body actually produces should
// type-check cleanly, and the declared set should appear on the
// FuncType so callers see the published contract.
func TestDeclaredEffects_Accepted(t *testing.T) {
	cases := []struct {
		name string
		src  string
		fn   string
		want types.EffectSet
	}{
		{"exact match", "fun greet(name: string) ! io { print(name) }", "greet", types.NewEffectSet(types.EffectIO)},
		{"wider than body", "fun maybe_print(name: string) ! io, time { }", "maybe_print", types.NewEffectSet(types.EffectIO, types.EffectTime)},
		{"declared without body effects", "fun reserve() ! fs { }", "reserve", types.NewEffectSet(types.EffectFS)},
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
			ty, err := env.GetVar(tc.fn)
			if err != nil {
				t.Fatalf("GetVar: %v", err)
			}
			ft := ty.(types.FuncType)
			if ft.Effects != tc.want {
				t.Fatalf("Effects=%s want %s", ft.Effects, tc.want)
			}
		})
	}
}

// TestDeclaredEffects_Rejected exercises T065: when the inferred set
// exceeds the declared annotation, type-check must fail.
func TestDeclaredEffects_Rejected(t *testing.T) {
	src := "fun stamp(): int ! io { return now() }"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	errs := types.Check(prog, env)
	if len(errs) == 0 {
		t.Fatalf("expected T065 diagnostic, got none")
	}
	found := false
	for _, e := range errs {
		if msg := e.Error(); contains(msg, "T065") {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected T065 in %v", errs)
	}
}

// TestDeclaredEffects_UnknownLabel exercises T064: an unknown label
// in the annotation must surface a diagnostic rather than silently
// expand the closed vocabulary.
func TestDeclaredEffects_UnknownLabel(t *testing.T) {
	src := "fun broken() ! quantum { }"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	errs := types.Check(prog, env)
	found := false
	for _, e := range errs {
		if contains(e.Error(), "T064") {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected T064 in %v", errs)
	}
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && indexOf(s, sub) >= 0
}

func indexOf(s, sub string) int {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return i
		}
	}
	return -1
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
