package resolve

import (
	"strings"
	"testing"

	"mochi/compiler3/ffi/typebridge"
)

// TestResolveStrings is the Phase 2 acceptance check: resolving
// "strings" returns a PackageBinding with every exported function
// represented and ToUpper specifically lowered through the bridge.
func TestResolveStrings(t *testing.T) {
	r := New()
	pb, err := r.Resolve("strings")
	if err != nil {
		t.Fatalf("Resolve strings: %v", err)
	}
	if pb.ImportPath != "strings" {
		t.Errorf("import path = %q", pb.ImportPath)
	}
	if pb.Name != "strings" {
		t.Errorf("name = %q", pb.Name)
	}
	if pb.MochiVersion != MochiResolverVersion {
		t.Errorf("mochi version = %q", pb.MochiVersion)
	}

	upper := pb.LookupFunc("ToUpper")
	if upper == nil {
		t.Fatal("ToUpper missing")
	}
	if upper.GoCallExpr != "strings.ToUpper" {
		t.Errorf("call expr = %q", upper.GoCallExpr)
	}
	if upper.Signature.Kind != typebridge.KindFunc {
		t.Fatalf("sig kind = %s", upper.Signature.Kind)
	}
	if len(upper.Signature.Params) != 1 || upper.Signature.Params[0].Kind != typebridge.KindString {
		t.Errorf("ToUpper params = %+v", upper.Signature.Params)
	}
	if len(upper.Signature.Results) != 1 || upper.Signature.Results[0].Kind != typebridge.KindString {
		t.Errorf("ToUpper results = %+v", upper.Signature.Results)
	}

	// A few more well-known signatures to verify the walk is uniform.
	for _, want := range []string{"Contains", "HasPrefix", "Split", "Join", "ToLower", "TrimSpace", "Replace"} {
		if pb.LookupFunc(want) == nil {
			t.Errorf("strings.%s missing", want)
		}
	}

	// Variadic Join: Sprintf etc. live in fmt; Join itself is not
	// variadic but Split returns []string. Validate the result shape.
	split := pb.LookupFunc("Split")
	if split == nil {
		t.Fatal("Split missing")
	}
	if split.Signature.Results[0].Kind != typebridge.KindList {
		t.Errorf("Split result = %+v", split.Signature.Results[0])
	}
	if split.Signature.Results[0].Elem == nil || split.Signature.Results[0].Elem.Kind != typebridge.KindString {
		t.Errorf("Split result Elem = %+v", split.Signature.Results[0].Elem)
	}

	// Builder/Reader/Replacer are exported types with methods.
	builder := pb.LookupType("Builder")
	if builder == nil {
		t.Fatal("Builder missing")
	}
	if len(builder.Methods) == 0 {
		t.Errorf("Builder has no methods")
	}
	// At least WriteString and String should be present.
	gotMethods := map[string]bool{}
	for _, m := range builder.Methods {
		gotMethods[m.Name] = true
	}
	for _, want := range []string{"String", "WriteString", "Len", "Reset"} {
		if !gotMethods[want] {
			t.Errorf("Builder missing method %s", want)
		}
	}
}

// TestResolveIo exercises an interface-heavy package: io.Reader and
// io.Writer must appear with the right method counts.
func TestResolveIo(t *testing.T) {
	r := New()
	pb, err := r.Resolve("io")
	if err != nil {
		t.Fatal(err)
	}
	rdr := pb.LookupType("Reader")
	if rdr == nil {
		t.Fatal("io.Reader missing")
	}
	if !rdr.IsInterface {
		t.Errorf("io.Reader IsInterface = false")
	}
	if len(rdr.Methods) != 1 || rdr.Methods[0].Name != "Read" {
		t.Errorf("io.Reader methods = %+v", rdr.Methods)
	}
	wtr := pb.LookupType("Writer")
	if wtr == nil {
		t.Fatal("io.Writer missing")
	}
	if !wtr.IsInterface {
		t.Errorf("io.Writer IsInterface = false")
	}
}

// TestResolveGenericFunc checks that a generic stdlib function (slices.Index)
// produces a FuncBinding with non-empty TypeParams and an attached
// ErrGenericNotInstantiated warning.
func TestResolveGenericFunc(t *testing.T) {
	r := New()
	pb, err := r.Resolve("slices")
	if err != nil {
		t.Fatal(err)
	}
	idx := pb.LookupFunc("Index")
	if idx == nil {
		t.Skip("slices.Index not present in this Go install")
	}
	if len(idx.TypeParams) == 0 {
		t.Errorf("slices.Index has no TypeParams: %+v", idx)
	}
	found := false
	for _, e := range pb.Errors {
		if e.Symbol == "Index" && e.Reason == ErrGenericNotInstantiated {
			found = true
		}
	}
	if !found {
		t.Errorf("no ErrGenericNotInstantiated for slices.Index")
	}
}

// TestResolveErrorsPackage exercises constants and vars: the errors
// package exposes neither vars nor consts, but errors.As/Is are
// expected funcs.
func TestResolveErrorsPackage(t *testing.T) {
	r := New()
	pb, err := r.Resolve("errors")
	if err != nil {
		t.Fatal(err)
	}
	for _, want := range []string{"As", "Is", "Unwrap", "New"} {
		if pb.LookupFunc(want) == nil {
			t.Errorf("errors.%s missing", want)
		}
	}
}

// TestResolveStatsAndStable confirms the binding walks symbols in
// stable (alphabetic) order, so cache contents are reproducible
// across resolve calls.
func TestResolveStatsAndStable(t *testing.T) {
	r := New()
	a, err := r.Resolve("strings")
	if err != nil {
		t.Fatal(err)
	}
	b, err := r.Resolve("strings")
	if err != nil {
		t.Fatal(err)
	}
	if a.Stats() != b.Stats() {
		t.Errorf("stats not stable across resolves:\n  a=%s\n  b=%s", a.Stats(), b.Stats())
	}
	if len(a.Funcs) != len(b.Funcs) {
		t.Errorf("func count drifted: %d vs %d", len(a.Funcs), len(b.Funcs))
	}
	for i := range a.Funcs {
		if a.Funcs[i].Name != b.Funcs[i].Name {
			t.Errorf("func order drift at %d: %s vs %s", i, a.Funcs[i].Name, b.Funcs[i].Name)
		}
	}
}

// TestResolveUnknownPackage confirms a missing package surfaces as a
// hard error rather than an empty binding.
func TestResolveUnknownPackage(t *testing.T) {
	r := New()
	_, err := r.Resolve("mochi/this/package/does/not/exist/anywhere")
	if err == nil {
		t.Fatal("expected error resolving missing package")
	}
	msg := err.Error()
	expected := []string{"no type info", "no packages", "packages.Load", "is not in std", "no required module", "no Go files", "empty package"}
	matched := false
	for _, e := range expected {
		if strings.Contains(msg, e) {
			matched = true
			break
		}
	}
	if !matched {
		t.Errorf("error message = %q (none of %v matched)", msg, expected)
	}
}
