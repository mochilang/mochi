package tinygo

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

func TestProfileIsValid(t *testing.T) {
	if !ProfileStandard.IsValid() {
		t.Errorf("standard should be valid")
	}
	if !ProfileEmbedded.IsValid() {
		t.Errorf("embedded should be valid")
	}
	if Profile("garbage").IsValid() {
		t.Errorf("garbage should be invalid")
	}
}

func TestCheckPackageStandardIsNoop(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"reflect", "unsafe"},
	}
	if got := CheckPackage(ProfileStandard, pkg); got != nil {
		t.Errorf("standard profile should yield no violations, got %v", got)
	}
}

func TestCheckPackageDetectsBannedImports(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"fmt", "reflect", "encoding/gob", "io"},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	if len(got) != 2 {
		t.Fatalf("want 2 violations, got %v", got)
	}
	if got[0].Kind != "import" || got[0].Where != "encoding/gob" {
		t.Errorf("first violation = %+v", got[0])
	}
	if got[1].Where != "reflect" {
		t.Errorf("second violation = %+v", got[1])
	}
}

func TestCheckPackageDetectsBannedTypeInParam(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Funcs: []apisurface.Func{
			{Name: "F", Params: []apisurface.Param{{Name: "v", Type: "reflect.Value"}}},
		},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	if len(got) != 1 {
		t.Fatalf("got %v", got)
	}
	if got[0].Kind != "param-type" {
		t.Errorf("kind = %q", got[0].Kind)
	}
	if got[0].Where != "x.F" {
		t.Errorf("where = %q", got[0].Where)
	}
}

func TestCheckPackageDetectsBannedTypeInResult(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Funcs: []apisurface.Func{
			{Name: "G", Results: []apisurface.Param{{Type: "*unsafe.Pointer"}}},
		},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	if len(got) != 1 || got[0].Kind != "result-type" {
		t.Fatalf("got %v", got)
	}
}

func TestCheckPackageStripWrappersHandlesSliceMapPointer(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Funcs: []apisurface.Func{
			{Name: "Sl", Params: []apisurface.Param{{Type: "[]reflect.Value"}}},
			{Name: "Mp", Params: []apisurface.Param{{Type: "map[string]reflect.Type"}}},
			{Name: "Pt", Params: []apisurface.Param{{Type: "*reflect.Value"}}},
			{Name: "Ar", Params: []apisurface.Param{{Type: "[4]reflect.Value"}}},
			{Name: "Va", Params: []apisurface.Param{{Type: "...reflect.Value"}}},
		},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	if len(got) != 5 {
		t.Fatalf("want 5 violations, got %v", got)
	}
}

func TestCheckPackageMethodViolation(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Types: []apisurface.Type{
			{
				Name: "S",
				Methods: []apisurface.Func{
					{Name: "M", Params: []apisurface.Param{{Type: "reflect.Value"}}},
				},
			},
		},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	if len(got) != 1 || got[0].Where != "x.S.M" {
		t.Fatalf("got %v", got)
	}
}

func TestCheckPackageInterfaceMethodViolation(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Types: []apisurface.Type{
			{
				Name: "I",
				InterfaceMethods: []apisurface.Func{
					{Name: "M", Results: []apisurface.Param{{Type: "runtime/cgo.Handle"}}},
				},
			},
		},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	if len(got) != 1 || got[0].Where != "x.I.M" {
		t.Fatalf("got %v", got)
	}
}

func TestCheckPackageSortStable(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "x",
		Imports:    []string{"runtime/cgo", "encoding/gob"},
		Funcs: []apisurface.Func{
			{Name: "Z", Params: []apisurface.Param{{Type: "reflect.Value"}}},
			{Name: "A", Params: []apisurface.Param{{Type: "reflect.Value"}}},
		},
	}
	got := CheckPackage(ProfileEmbedded, pkg)
	// Imports first (kind sort), then param-types alphabetised by Where.
	if got[0].Kind != "import" || got[0].Where != "encoding/gob" {
		t.Errorf("first = %+v", got[0])
	}
	if got[2].Kind != "param-type" || got[2].Where != "x.A" {
		t.Errorf("third = %+v", got[2])
	}
}

func TestIsCompatible(t *testing.T) {
	pure := apisurface.Package{ImportPath: "x", Funcs: []apisurface.Func{{Name: "F"}}}
	if !IsCompatible(ProfileEmbedded, pure) {
		t.Errorf("pure package should be compatible")
	}
	if !IsCompatible(ProfileStandard, apisurface.Package{Imports: []string{"reflect"}}) {
		t.Errorf("standard profile is always compatible")
	}
	bad := apisurface.Package{ImportPath: "x", Imports: []string{"reflect"}}
	if IsCompatible(ProfileEmbedded, bad) {
		t.Errorf("reflect import should fail compatibility")
	}
}

func TestLinknameSpecValidate(t *testing.T) {
	good := LinknameSpec{LocalName: "mochi_x_F", TargetSymbol: "x.F"}
	if err := good.Validate(); err != nil {
		t.Errorf("good spec: %v", err)
	}
	bad := LinknameSpec{TargetSymbol: "x.F"}
	if err := bad.Validate(); !errors.Is(err, ErrTinygo) {
		t.Errorf("missing LocalName: %v", err)
	}
	bad = LinknameSpec{LocalName: "mochi_x_F"}
	if err := bad.Validate(); !errors.Is(err, ErrTinygo) {
		t.Errorf("missing TargetSymbol: %v", err)
	}
	bad = LinknameSpec{LocalName: "mochi_x_F", TargetSymbol: "missingdot"}
	if err := bad.Validate(); !errors.Is(err, ErrTinygo) {
		t.Errorf("missing dot: %v", err)
	}
}

func TestRenderLinknameContainsCanonicalParts(t *testing.T) {
	spec := LinknameSpec{
		LocalName:    "mochi_x_F",
		TargetSymbol: "example.com/src.F",
		Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
		Results:      []apisurface.Param{{Type: "int64"}},
	}
	out, err := RenderLinkname(spec)
	if err != nil {
		t.Fatalf("RenderLinkname: %v", err)
	}
	for _, want := range []string{
		"//go:linkname mochi_x_F example.com/src.F",
		"func mochi_x_F(v int64) int64",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("missing %q in:\n%s", want, out)
		}
	}
	// Linkname wrappers have no body.
	if strings.Contains(out, "{") {
		t.Errorf("linkname wrapper should be body-less, got:\n%s", out)
	}
}

func TestRenderLinknameZeroResult(t *testing.T) {
	spec := LinknameSpec{
		LocalName:    "mochi_x_H",
		TargetSymbol: "x.H",
		Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
	}
	out, err := RenderLinkname(spec)
	if err != nil {
		t.Fatalf("%v", err)
	}
	if !strings.HasSuffix(strings.TrimSpace(out), "func mochi_x_H(v int64)") {
		t.Errorf("zero-result signature wrong:\n%s", out)
	}
}

func TestRenderLinknameMultiResult(t *testing.T) {
	spec := LinknameSpec{
		LocalName:    "mochi_x_G",
		TargetSymbol: "x.G",
		Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
		Results:      []apisurface.Param{{Type: "int64"}, {Type: "error"}},
	}
	out, err := RenderLinkname(spec)
	if err != nil {
		t.Fatalf("%v", err)
	}
	if !strings.Contains(out, "(int64, error)") {
		t.Errorf("multi-result missing tuple:\n%s", out)
	}
}

func TestRenderLinknameVariadic(t *testing.T) {
	spec := LinknameSpec{
		LocalName:    "mochi_x_Printf",
		TargetSymbol: "x.Printf",
		Params: []apisurface.Param{
			{Name: "format", Type: "string"},
			{Name: "args", Type: "any"},
		},
		Variadic: true,
	}
	out, err := RenderLinkname(spec)
	if err != nil {
		t.Fatalf("%v", err)
	}
	if !strings.Contains(out, "args ...any") {
		t.Errorf("variadic param signature wrong:\n%s", out)
	}
}

func TestRenderLinknameUnnamedParam(t *testing.T) {
	spec := LinknameSpec{
		LocalName:    "mochi_x_F",
		TargetSymbol: "x.F",
		Params:       []apisurface.Param{{Type: "int64"}, {Type: "string"}},
	}
	out, err := RenderLinkname(spec)
	if err != nil {
		t.Fatalf("%v", err)
	}
	if !strings.Contains(out, "p0 int64, p1 string") {
		t.Errorf("unnamed param synth wrong:\n%s", out)
	}
}

func TestRenderLinknameRejectsInvalidSpec(t *testing.T) {
	_, err := RenderLinkname(LinknameSpec{})
	if !errors.Is(err, ErrTinygo) {
		t.Fatalf("want ErrTinygo, got %v", err)
	}
}

func TestRenderFileContainsBuildTagAndImport(t *testing.T) {
	out, err := RenderFile("wrap", []LinknameSpec{{
		LocalName:    "mochi_x_F",
		TargetSymbol: "example.com/src.F",
		Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
		Results:      []apisurface.Param{{Type: "int64"}},
	}})
	if err != nil {
		t.Fatalf("RenderFile: %v", err)
	}
	for _, want := range []string{
		"//go:build !mochi_no_linkname",
		"package wrap",
		"import _ \"unsafe\"",
		"//go:linkname mochi_x_F example.com/src.F",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("missing %q in:\n%s", want, out)
		}
	}
}

func TestRenderFileRejectsEmptyPkgName(t *testing.T) {
	_, err := RenderFile("", nil)
	if !errors.Is(err, ErrTinygo) {
		t.Errorf("want ErrTinygo, got %v", err)
	}
}

func TestRenderFileBubblesSpecError(t *testing.T) {
	_, err := RenderFile("wrap", []LinknameSpec{{LocalName: "x"}})
	if !errors.Is(err, ErrTinygo) {
		t.Errorf("want ErrTinygo, got %v", err)
	}
}

func TestBannedImportListIsSorted(t *testing.T) {
	for i := 1; i < len(BannedImports); i++ {
		if BannedImports[i-1] >= BannedImports[i] {
			t.Errorf("BannedImports not sorted at index %d: %q >= %q", i, BannedImports[i-1], BannedImports[i])
		}
	}
}
