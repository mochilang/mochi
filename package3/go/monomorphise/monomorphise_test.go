package monomorphise

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

func TestParseSpecsHappyPath(t *testing.T) {
	frags := []string{
		"item = \"golang.org/x/exp/slices.Sort\"\nT = \"int64\"",
		"item = \"encoding/json.Unmarshal\"\nT = \"string\"",
	}
	set, err := ParseSpecs(frags)
	if err != nil {
		t.Fatalf("ParseSpecs error: %v", err)
	}
	if len(set.Specs) != 2 {
		t.Fatalf("Specs len = %d, want 2", len(set.Specs))
	}
	if set.Specs[0].Item != "golang.org/x/exp/slices.Sort" {
		t.Errorf("Specs[0].Item = %q", set.Specs[0].Item)
	}
	if got := set.Specs[0].TypeArgs; len(got) != 1 || got[0] != "int64" {
		t.Errorf("Specs[0].TypeArgs = %v", got)
	}
}

func TestParseSpecsMultipleTypeArgs(t *testing.T) {
	set, err := ParseSpecs([]string{"item = \"x.Map\"\nT = \"string, int64\""})
	if err != nil {
		t.Fatalf("ParseSpecs error: %v", err)
	}
	if got := set.Specs[0].TypeArgs; len(got) != 2 || got[0] != "string" || got[1] != "int64" {
		t.Errorf("TypeArgs = %v", got)
	}
}

func TestParseSpecsIgnoresBlankAndComment(t *testing.T) {
	frag := "# leading comment\n\nitem = \"a.B\"\n# inline\nT = \"int\"\n"
	set, err := ParseSpecs([]string{frag})
	if err != nil {
		t.Fatalf("ParseSpecs error: %v", err)
	}
	if set.Specs[0].Item != "a.B" {
		t.Errorf("Item = %q", set.Specs[0].Item)
	}
}

func TestParseSpecsRejectsMalformedLine(t *testing.T) {
	_, err := ParseSpecs([]string{"justwords"})
	if !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("want ErrMonomorphise, got %v", err)
	}
}

func TestParseSpecsRejectsUnknownKey(t *testing.T) {
	_, err := ParseSpecs([]string{"item = \"a.B\"\nT = \"int\"\nUnknown = \"x\""})
	if !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("want ErrMonomorphise, got %v", err)
	}
}

func TestParseSpecsRejectsMissingDot(t *testing.T) {
	_, err := ParseSpecs([]string{"item = \"missingdot\"\nT = \"int\""})
	if !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("want ErrMonomorphise, got %v", err)
	}
}

func TestParseSpecsRejectsEmptyT(t *testing.T) {
	_, err := ParseSpecs([]string{"item = \"a.B\"\nT = \"\""})
	if !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("want ErrMonomorphise, got %v", err)
	}
}

func TestSpecAccessors(t *testing.T) {
	s := Spec{Item: "encoding/json.Decoder"}
	if got := s.PackagePath(); got != "encoding/json" {
		t.Errorf("PackagePath = %q", got)
	}
	if got := s.Ident(); got != "Decoder" {
		t.Errorf("Ident = %q", got)
	}
}

func TestSpecAccessorsNoDot(t *testing.T) {
	s := Spec{Item: "bareword"}
	if got := s.PackagePath(); got != "" {
		t.Errorf("PackagePath = %q, want empty", got)
	}
	if got := s.Ident(); got != "bareword" {
		t.Errorf("Ident = %q", got)
	}
}

func TestValidateEmptyOK(t *testing.T) {
	set := &SpecSet{}
	if err := set.Validate(); err != nil {
		t.Fatalf("Validate on empty set: %v", err)
	}
}

func TestValidateRejectsEmptyTypeArg(t *testing.T) {
	set := &SpecSet{Specs: []Spec{{Item: "a.B", TypeArgs: []string{"int", "   "}}}}
	if err := set.Validate(); !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("want ErrMonomorphise, got %v", err)
	}
}

func TestResolveHappyPath(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "golang.org/x/exp/slices",
		Funcs: []apisurface.Func{
			{
				Name:       "Sort",
				TypeParams: []apisurface.TypeParam{{Name: "T", Constraint: "any"}},
				Params:     []apisurface.Param{{Name: "xs", Type: "[]T"}},
			},
		},
	}
	set := &SpecSet{Specs: []Spec{
		{Item: "golang.org/x/exp/slices.Sort", TypeArgs: []string{"int64"}},
	}}
	out, missing, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(missing) != 0 {
		t.Errorf("missing = %v", missing)
	}
	if len(out) != 1 {
		t.Fatalf("instances len = %d", len(out))
	}
	if out[0].Func.Name != "Sort" {
		t.Errorf("Func.Name = %q", out[0].Func.Name)
	}
	if out[0].SymbolSuffix != "int64" {
		t.Errorf("SymbolSuffix = %q", out[0].SymbolSuffix)
	}
}

func TestResolveReportsMissing(t *testing.T) {
	pkg := apisurface.Package{ImportPath: "x"}
	set := &SpecSet{Specs: []Spec{{Item: "x.NotThere", TypeArgs: []string{"int"}}}}
	_, missing, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(missing) != 1 {
		t.Fatalf("missing = %v", missing)
	}
}

func TestResolveReportsArityMismatch(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Funcs: []apisurface.Func{
			{Name: "F", TypeParams: []apisurface.TypeParam{{Name: "T"}, {Name: "U"}}},
		},
	}
	set := &SpecSet{Specs: []Spec{{Item: "x.F", TypeArgs: []string{"int"}}}}
	_, missing, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(missing) != 1 || !strings.Contains(missing[0], "needs 2") {
		t.Fatalf("missing = %v", missing)
	}
}

func TestResolveSkipsNonGenericFunc(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Funcs:      []apisurface.Func{{Name: "F"}},
	}
	set := &SpecSet{Specs: []Spec{{Item: "x.F", TypeArgs: []string{"int"}}}}
	_, missing, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(missing) != 1 {
		t.Fatalf("expected non-generic F to be unmatched; missing = %v", missing)
	}
}

func TestResolveMatchesGenericTypeMethod(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Types: []apisurface.Type{
			{
				Name:       "Stack",
				TypeParams: []apisurface.TypeParam{{Name: "T"}},
				Methods: []apisurface.Func{
					{
						Name:       "Push",
						TypeParams: []apisurface.TypeParam{{Name: "T"}},
						Params:     []apisurface.Param{{Name: "v", Type: "T"}},
					},
				},
			},
		},
	}
	set := &SpecSet{Specs: []Spec{{Item: "x.Stack.Push", TypeArgs: []string{"int64"}}}}
	out, missing, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(missing) != 0 {
		t.Errorf("missing = %v", missing)
	}
	if len(out) != 1 || out[0].Func.Name != "Push" {
		t.Fatalf("instances = %+v", out)
	}
}

func TestResolveDeterministicOrder(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Funcs: []apisurface.Func{
			{Name: "A", TypeParams: []apisurface.TypeParam{{Name: "T"}}},
			{Name: "B", TypeParams: []apisurface.TypeParam{{Name: "T"}}},
		},
	}
	set := &SpecSet{Specs: []Spec{
		{Item: "x.B", TypeArgs: []string{"int"}},
		{Item: "x.A", TypeArgs: []string{"string"}},
		{Item: "x.A", TypeArgs: []string{"int"}},
	}}
	out, _, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	want := []string{"x.A", "x.A", "x.B"}
	for i, w := range want {
		if out[i].Spec.Item != w {
			t.Errorf("out[%d].Spec.Item = %q, want %q", i, out[i].Spec.Item, w)
		}
	}
	if out[0].SymbolSuffix != "int" || out[1].SymbolSuffix != "string" {
		t.Errorf("suffix tie-break wrong: %q, %q", out[0].SymbolSuffix, out[1].SymbolSuffix)
	}
}

func TestResolveNilSetReturnsEmpty(t *testing.T) {
	out, missing, err := Resolve(apisurface.Package{}, nil)
	if err != nil {
		t.Fatalf("err = %v", err)
	}
	if len(out) != 0 || len(missing) != 0 {
		t.Errorf("nil set should yield empty: out=%v missing=%v", out, missing)
	}
}

func TestSanitiseSuffix(t *testing.T) {
	cases := []struct {
		in   []string
		want string
	}{
		{[]string{"MyStruct"}, "MyStruct"},
		{[]string{"encoding/json.Decoder"}, "encoding_json_Decoder"},
		{[]string{"string", "int64"}, "string_int64"},
		{[]string{"*foo.Bar"}, "_foo_Bar"},
		{[]string{"[]byte"}, "__byte"},
	}
	for _, c := range cases {
		if got := sanitiseSuffix(c.in); got != c.want {
			t.Errorf("sanitiseSuffix(%v) = %q, want %q", c.in, got, c.want)
		}
	}
}

func TestRenderInstanceHappyPath(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "golang.org/x/exp/slices.Sort", TypeArgs: []string{"int64"}},
		Func: apisurface.Func{
			Name:       "Sort",
			TypeParams: []apisurface.TypeParam{{Name: "T", Constraint: "any"}},
			Params:     []apisurface.Param{{Name: "xs", Type: "[]T"}},
		},
		SymbolSuffix: "int64",
	}
	out, err := RenderInstance(inst, "slices_v0_0_0", "src_slices")
	if err != nil {
		t.Fatalf("RenderInstance: %v", err)
	}
	wantSubs := []string{
		"mochi_slices_v0_0_0_Sort_int64",
		"//export mochi_slices_v0_0_0_Sort_int64",
		"xs []int64",
		"src_slices.Sort[int64](xs)",
	}
	for _, w := range wantSubs {
		if !strings.Contains(out, w) {
			t.Errorf("missing %q in output:\n%s", w, out)
		}
	}
}

func TestRenderInstanceSingleResult(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "x.F", TypeArgs: []string{"int"}},
		Func: apisurface.Func{
			Name:       "F",
			TypeParams: []apisurface.TypeParam{{Name: "T"}},
			Params:     []apisurface.Param{{Name: "v", Type: "T"}},
			Results:    []apisurface.Param{{Type: "T"}},
		},
		SymbolSuffix: "int",
	}
	out, err := RenderInstance(inst, "m", "src")
	if err != nil {
		t.Fatalf("RenderInstance: %v", err)
	}
	if !strings.Contains(out, ") int {") {
		t.Errorf("expected single-result type ') int {': %s", out)
	}
	if !strings.Contains(out, "return src.F[int](v)") {
		t.Errorf("expected `return src.F[int](v)`: %s", out)
	}
}

func TestRenderInstanceMultipleResults(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "x.G", TypeArgs: []string{"int"}},
		Func: apisurface.Func{
			Name:       "G",
			TypeParams: []apisurface.TypeParam{{Name: "T"}},
			Params:     []apisurface.Param{{Name: "v", Type: "T"}},
			Results:    []apisurface.Param{{Type: "T"}, {Type: "error"}},
		},
		SymbolSuffix: "int",
	}
	out, err := RenderInstance(inst, "m", "src")
	if err != nil {
		t.Fatalf("RenderInstance: %v", err)
	}
	if !strings.Contains(out, "(int, error)") {
		t.Errorf("expected `(int, error)`: %s", out)
	}
}

func TestRenderInstanceNoResults(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "x.H", TypeArgs: []string{"int"}},
		Func: apisurface.Func{
			Name:       "H",
			TypeParams: []apisurface.TypeParam{{Name: "T"}},
			Params:     []apisurface.Param{{Name: "v", Type: "T"}},
		},
		SymbolSuffix: "int",
	}
	out, err := RenderInstance(inst, "m", "src")
	if err != nil {
		t.Fatalf("RenderInstance: %v", err)
	}
	// No "return" keyword for void wrappers.
	if strings.Contains(out, "return src.H") {
		t.Errorf("zero-result wrapper should not have return: %s", out)
	}
	if !strings.Contains(out, "\tsrc.H[int](v)\n") {
		t.Errorf("expected bare call line: %s", out)
	}
}

func TestRenderInstanceUnnamedParam(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "x.F", TypeArgs: []string{"int"}},
		Func: apisurface.Func{
			Name:       "F",
			TypeParams: []apisurface.TypeParam{{Name: "T"}},
			Params:     []apisurface.Param{{Type: "T"}, {Type: "T"}},
		},
		SymbolSuffix: "int",
	}
	out, err := RenderInstance(inst, "m", "src")
	if err != nil {
		t.Fatalf("RenderInstance: %v", err)
	}
	if !strings.Contains(out, "p0 int, p1 int") {
		t.Errorf("expected synthetic param names p0, p1: %s", out)
	}
	if !strings.Contains(out, "src.F[int](p0, p1)") {
		t.Errorf("expected call site to use synthetic names: %s", out)
	}
}

func TestRenderInstanceRejectsArityMismatch(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "x.F", TypeArgs: []string{"int", "string"}},
		Func: apisurface.Func{
			Name:       "F",
			TypeParams: []apisurface.TypeParam{{Name: "T"}},
		},
	}
	if _, err := RenderInstance(inst, "m", "src"); !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("want ErrMonomorphise, got %v", err)
	}
}

func TestRenderInstanceRejectsMissingModule(t *testing.T) {
	inst := Instance{Spec: Spec{Item: "x.F", TypeArgs: []string{"int"}}, Func: apisurface.Func{Name: "F", TypeParams: []apisurface.TypeParam{{Name: "T"}}}}
	if _, err := RenderInstance(inst, "", "src"); !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("missing module: want ErrMonomorphise, got %v", err)
	}
	if _, err := RenderInstance(inst, "m", ""); !errors.Is(err, ErrMonomorphise) {
		t.Fatalf("missing alias: want ErrMonomorphise, got %v", err)
	}
}

func TestConcretiseTypePreservesLongerIdents(t *testing.T) {
	tpMap := map[string]string{"T": "int", "U": "string"}
	// "T" appears in "[]T" but also in "Truthy"; only the bare T should be replaced.
	got := concretiseType("[]T", tpMap)
	if got != "[]int" {
		t.Errorf("[]T -> %q", got)
	}
	got = concretiseType("Truthy", tpMap)
	if got != "Truthy" {
		t.Errorf("Truthy -> %q, want unchanged", got)
	}
	got = concretiseType("map[T]U", tpMap)
	if got != "map[int]string" {
		t.Errorf("map[T]U -> %q", got)
	}
	got = concretiseType("func(T) (U, error)", tpMap)
	if got != "func(int) (string, error)" {
		t.Errorf("func(T) (U, error) -> %q", got)
	}
}

func TestConcretiseTypeEmpty(t *testing.T) {
	if got := concretiseType("", map[string]string{"T": "int"}); got != "" {
		t.Errorf("empty in -> %q", got)
	}
	if got := concretiseType("[]int", nil); got != "[]int" {
		t.Errorf("nil tpMap -> %q", got)
	}
}

func TestReplaceIdentBoundaryDoesNotEatPartialMatches(t *testing.T) {
	got := replaceIdentBoundary("TX TT T2 []T T", "T", "int")
	// Only the bare standalone "T" tokens (the two at the end) should change.
	want := "TX TT T2 []int int"
	if got != want {
		t.Errorf("got %q, want %q", got, want)
	}
}
