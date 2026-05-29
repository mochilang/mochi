package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
	"mochi/package3/go/typemap"
)

func TestFlattenModule(t *testing.T) {
	cases := map[string]string{
		"github.com/spf13/cobra":   "github_com_spf13_cobra",
		"gopkg.in/yaml.v3":         "gopkg_in_yaml_v3",
		"example.com/foo":          "example_com_foo",
		"a/b-c":                    "a_b_c",
		"123/foo":                  "_123_foo",
	}
	for in, want := range cases {
		if got := flattenModule(in); got != want {
			t.Errorf("flattenModule(%q) = %q; want %q", in, got, want)
		}
	}
}

func TestExportSymbol(t *testing.T) {
	got := exportSymbol("github_com_spf13_cobra", "cobra", "Execute")
	want := "mochi_go_github_com_spf13_cobra_cobra_Execute"
	if got != want {
		t.Errorf("exportSymbol = %q; want %q", got, want)
	}
}

func TestPkgAlias(t *testing.T) {
	cases := map[string]string{
		"github.com/spf13/cobra": "cobra",
		"gopkg.in/yaml.v3":       "yaml.v3",
		"":                       "pkg",
	}
	for in, want := range cases {
		if got := pkgAlias(in); got != want {
			t.Errorf("pkgAlias(%q) = %q; want %q", in, got, want)
		}
	}
}

func TestIsErrorType(t *testing.T) {
	if !isErrorType(apisurface.BasicType{Name: "error"}) {
		t.Errorf("error not detected")
	}
	if isErrorType(apisurface.BasicType{Name: "int"}) {
		t.Errorf("int incorrectly flagged as error")
	}
	if isErrorType(apisurface.NamedType{Name: "MyError"}) {
		t.Errorf("named MyError incorrectly flagged as error")
	}
}

func TestBaselineCTypes(t *testing.T) {
	cases := []struct {
		in   typemap.MochiType
		wantParam string
		wantRes string
		ok   bool
	}{
		{typemap.ScalarType{Name: "int"}, "C.long", "C.long", true},
		{typemap.ScalarType{Name: "float"}, "C.double", "C.double", true},
		{typemap.ScalarType{Name: "bool"}, "C.int", "C.int", true},
		{typemap.ScalarType{Name: "string"}, "*C.char", "*C.char", true},
		{typemap.ScalarType{Name: "bytes"}, "MochiSlice", "MochiSlice", true},
		{typemap.HandleType{Name: "io.Reader"}, "", "", false},
		{typemap.ListType{Elem: typemap.ScalarType{Name: "int"}}, "", "", false},
	}
	for _, tc := range cases {
		got, ok := baselineParamCType(tc.in)
		if ok != tc.ok {
			t.Errorf("param ok(%v) = %v; want %v", tc.in, ok, tc.ok)
		}
		if got != tc.wantParam {
			t.Errorf("param(%v) = %q; want %q", tc.in, got, tc.wantParam)
		}
		got, ok = baselineResultCType(tc.in)
		if ok != tc.ok {
			t.Errorf("result ok(%v) = %v; want %v", tc.in, ok, tc.ok)
		}
		if got != tc.wantRes {
			t.Errorf("result(%v) = %q; want %q", tc.in, got, tc.wantRes)
		}
	}
}

// buildEmitter constructs a tiny fixture with one package and the
// supplied funcs. Returns the emitter, surface, and mapper.
func buildEmitter(t *testing.T, pkgPath, pkgName string, funcs []apisurface.Func) *Emitter {
	t.Helper()
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Module:        "example.com/m",
		Packages: []apisurface.Package{
			{ImportPath: pkgPath, Name: pkgName, Funcs: funcs},
		},
	}
	s, err := apisurface.Load(file, apisurface.LoadOptions{})
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	m := typemap.NewMapper(s)
	e, err := NewEmitter(s, m, "example.com/m", "v0.0.1")
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	return e
}

func TestEmitScalarPassthrough(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:    "Add",
			Params:  []apisurface.Param{{Name: "x", Type: "int"}, {Name: "y", Type: "int"}},
			Results: []apisurface.Param{{Type: "int"}},
		},
	})
	r, err := e.Emit()
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	if len(r.Funcs) != 1 || r.Funcs[0].GoName != "Add" {
		t.Fatalf("funcs = %+v", r.Funcs)
	}
	src := r.Files["wrap.go"]
	if !strings.Contains(src, "//export mochi_go_example_com_m_m_Add") {
		t.Errorf("missing export symbol; src: %s", src)
	}
	if !strings.Contains(src, "m.Add(int64(x), int64(y))") {
		t.Errorf("missing call expression; src: %s", src)
	}
	if !strings.Contains(src, "return C.long(res)") {
		t.Errorf("missing return conv; src: %s", src)
	}
	if err := ParseGenerated("wrap.go", src); err != nil {
		t.Errorf("wrap.go does not parse: %v", err)
	}
}

func TestEmitStringReturn(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:    "Greet",
			Params:  []apisurface.Param{{Name: "who", Type: "string"}},
			Results: []apisurface.Param{{Type: "string"}},
		},
	})
	r, _ := e.Emit()
	if len(r.Funcs) != 1 {
		t.Fatalf("funcs = %+v", r.Funcs)
	}
	src := r.Files["wrap.go"]
	if !strings.Contains(src, "C.GoString(who)") {
		t.Errorf("missing string-in conv; src: %s", src)
	}
	if !strings.Contains(src, "C.CString(res)") {
		t.Errorf("missing string-out conv; src: %s", src)
	}
	if !strings.Contains(src, "runtime.KeepAlive(who)") {
		t.Errorf("missing keepalive for string param; src: %s", src)
	}
	if !strings.Contains(src, "mochi_go_example_com_m_string_free") {
		t.Errorf("missing module-scoped string_free; src: %s", src)
	}
	if err := ParseGenerated("wrap.go", src); err != nil {
		t.Errorf("parse: %v", err)
	}
}

func TestEmitErrorReturn(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:    "Run",
			Params:  []apisurface.Param{},
			Results: []apisurface.Param{{Type: "error"}},
		},
	})
	r, _ := e.Emit()
	if len(r.Funcs) != 1 || !r.Funcs[0].HasError {
		t.Fatalf("HasError missing; %+v", r.Funcs)
	}
	src := r.Files["wrap.go"]
	if !strings.Contains(src, "out_err **C.char") {
		t.Errorf("missing out_err param; src: %s", src)
	}
	if !strings.Contains(src, "return C.MochiStatus(0)") {
		t.Errorf("missing success status; src: %s", src)
	}
	if !strings.Contains(src, "return C.MochiStatus(1)") {
		t.Errorf("missing failure status; src: %s", src)
	}
	if err := ParseGenerated("wrap.go", src); err != nil {
		t.Errorf("parse: %v", err)
	}
}

func TestEmitValuePlusError(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:    "Lookup",
			Params:  []apisurface.Param{{Name: "k", Type: "string"}},
			Results: []apisurface.Param{{Type: "int"}, {Type: "error"}},
		},
	})
	r, _ := e.Emit()
	if !r.Funcs[0].HasError {
		t.Fatalf("HasError missing")
	}
	if len(r.Funcs[0].Results) != 1 {
		t.Errorf("Results = %d; want 1 (error stripped)", len(r.Funcs[0].Results))
	}
	src := r.Files["wrap.go"]
	if err := ParseGenerated("wrap.go", src); err != nil {
		t.Errorf("parse: %v", err)
	}
}

func TestEmitSkipsGenericFunc(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:       "Map",
			TypeParams: []apisurface.TypeParam{{Name: "T"}},
			Params:     []apisurface.Param{{Name: "x", Type: "T"}},
			Results:    []apisurface.Param{{Type: "T"}},
		},
	})
	r, _ := e.Emit()
	if len(r.Funcs) != 0 {
		t.Errorf("generic func not skipped; got %+v", r.Funcs)
	}
	if len(r.Skipped) == 0 || !strings.Contains(r.Skipped[0].Reason, "generic") {
		t.Errorf("skip reason = %v", r.Skipped)
	}
}

func TestEmitSkipsMethod(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:     "Read",
			Receiver: "Reader",
			Params:   []apisurface.Param{{Name: "p", Type: "[]byte"}},
			Results:  []apisurface.Param{{Type: "int"}, {Type: "error"}},
		},
	})
	r, _ := e.Emit()
	if len(r.Funcs) != 0 {
		t.Errorf("method not skipped; got %+v", r.Funcs)
	}
	if len(r.Skipped) == 0 || r.Skipped[0].Kind != "method" {
		t.Errorf("skip kind = %v", r.Skipped)
	}
}

func TestEmitSkipsUnsupportedParam(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", []apisurface.Func{
		{
			Name:    "Pipe",
			Params:  []apisurface.Param{{Name: "ch", Type: "chan int"}},
			Results: []apisurface.Param{},
		},
	})
	r, _ := e.Emit()
	if len(r.Funcs) != 0 {
		t.Errorf("chan-param func not skipped")
	}
	if len(r.Skipped) != 1 || !strings.Contains(r.Skipped[0].Reason, "phase 6.x") {
		t.Errorf("skip = %+v", r.Skipped)
	}
}

func TestEmitNoFuncsStillProducesValidSource(t *testing.T) {
	e := buildEmitter(t, "example.com/m", "m", nil)
	r, err := e.Emit()
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	if err := ParseGenerated("wrap.go", r.Files["wrap.go"]); err != nil {
		t.Errorf("wrap.go: %v", err)
	}
	if err := ParseGenerated("wrap_handles.go", r.Files["wrap_handles.go"]); err != nil {
		t.Errorf("wrap_handles.go: %v", err)
	}
}

func TestEmitDeterministicOrdering(t *testing.T) {
	funcs := []apisurface.Func{
		{Name: "Zeta", Results: []apisurface.Param{{Type: "int"}}},
		{Name: "Alpha", Results: []apisurface.Param{{Type: "int"}}},
		{Name: "Mu", Results: []apisurface.Param{{Type: "int"}}},
	}
	e := buildEmitter(t, "example.com/m", "m", funcs)
	r, _ := e.Emit()
	a, b, c := strings.Index(r.Files["wrap.go"], "_Alpha"),
		strings.Index(r.Files["wrap.go"], "_Mu"),
		strings.Index(r.Files["wrap.go"], "_Zeta")
	if !(a < b && b < c) {
		t.Errorf("ordering not stable: a=%d b=%d c=%d", a, b, c)
	}
}

func TestEmitMainPackageSkipped(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Module:        "example.com/m",
		Packages: []apisurface.Package{
			{ImportPath: "example.com/m", Name: "main", IsMain: true},
		},
	}
	s, _ := apisurface.Load(file, apisurface.LoadOptions{})
	m := typemap.NewMapper(s)
	e, _ := NewEmitter(s, m, "example.com/m", "v0.0.1")
	r, _ := e.Emit()
	if len(r.Funcs) != 0 || len(r.Skipped) != 1 || r.Skipped[0].Kind != "package" {
		t.Errorf("main skip not recorded: %+v / skipped: %+v", r.Funcs, r.Skipped)
	}
}

func TestNewEmitterValidation(t *testing.T) {
	if _, err := NewEmitter(nil, nil, "m", ""); err == nil {
		t.Errorf("nil surface accepted")
	}
	file := &apisurface.File{SchemaVersion: apisurface.SchemaVersion, Module: "m"}
	s, _ := apisurface.Load(file, apisurface.LoadOptions{})
	if _, err := NewEmitter(s, nil, "m", ""); err == nil {
		t.Errorf("nil mapper accepted")
	}
	m := typemap.NewMapper(s)
	if _, err := NewEmitter(s, m, "", ""); err == nil {
		t.Errorf("empty module accepted")
	}
}
