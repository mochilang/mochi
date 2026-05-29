package emit

import (
	"strings"
	"testing"

	"mochi/package3/go/typemap"
	"mochi/package3/go/wrapper"
)

func TestIsIdent(t *testing.T) {
	cases := map[string]bool{
		"cobra":     true,
		"go_pkg":    true,
		"yaml":      true,
		"_under":    true,
		"yaml.v3":   false,
		"123pkg":    false,
		"":          false,
		"pkg-name":  false,
		"a1":        true,
	}
	for in, want := range cases {
		if got := isIdent(in); got != want {
			t.Errorf("isIdent(%q) = %v; want %v", in, got, want)
		}
	}
}

func TestNewEmitterValidation(t *testing.T) {
	if _, err := NewEmitter(nil, "cobra"); err == nil {
		t.Errorf("nil wrap accepted")
	}
	w := &wrapper.Result{}
	if _, err := NewEmitter(w, ""); err == nil {
		t.Errorf("empty alias accepted")
	}
	if _, err := NewEmitter(w, "yaml.v3"); err == nil {
		t.Errorf("non-ident alias accepted")
	}
	if _, err := NewEmitter(w, "cobra"); err != nil {
		t.Errorf("valid construction rejected: %v", err)
	}
}

func TestEmitEmptyWrapper(t *testing.T) {
	w := &wrapper.Result{}
	e, _ := NewEmitter(w, "cobra")
	r, err := e.Emit()
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	if len(r.Externs) != 0 || len(r.Skipped) != 0 {
		t.Errorf("expected empty result; got %+v", r)
	}
	if !strings.Contains(r.Source, "DO NOT EDIT") {
		t.Errorf("missing banner; src: %q", r.Source)
	}
	if !strings.Contains(r.Source, "Alias: cobra") {
		t.Errorf("missing alias banner")
	}
}

func TestEmitScalarPassthrough(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_example_com_m_m_Add",
				Package: "example.com/m",
				GoName:  "Add",
				Params: []wrapper.EmittedParam{
					{Name: "x", CType: "C.long", Mochi: typemap.ScalarType{Name: "int"}},
					{Name: "y", CType: "C.long", Mochi: typemap.ScalarType{Name: "int"}},
				},
				Results: []wrapper.EmittedParam{
					{Name: "ret0", CType: "C.long", Mochi: typemap.ScalarType{Name: "int"}},
				},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if len(r.Externs) != 1 {
		t.Fatalf("externs = %+v", r.Externs)
	}
	want := "extern fun m.Add(x: int, y: int) : int"
	if !strings.Contains(r.Source, want) {
		t.Errorf("missing %q; src: %s", want, r.Source)
	}
	if !strings.Contains(r.Source, "mochi_go_example_com_m_m_Add") {
		t.Errorf("missing C symbol comment")
	}
}

func TestEmitStringInOut(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_example_com_m_m_Greet",
				Package: "example.com/m",
				GoName:  "Greet",
				Params: []wrapper.EmittedParam{
					{Name: "who", CType: "*C.char", Mochi: typemap.ScalarType{Name: "string"}},
				},
				Results: []wrapper.EmittedParam{
					{Name: "ret0", CType: "*C.char", Mochi: typemap.ScalarType{Name: "string"}},
				},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if !strings.Contains(r.Source, "extern fun m.Greet(who: string) : string") {
		t.Errorf("src: %s", r.Source)
	}
}

func TestEmitUnitReturn(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_example_com_m_m_Log",
				Package: "example.com/m",
				GoName:  "Log",
				Params: []wrapper.EmittedParam{
					{Name: "msg", CType: "*C.char", Mochi: typemap.ScalarType{Name: "string"}},
				},
				Results: []wrapper.EmittedParam{},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if !strings.Contains(r.Source, "extern fun m.Log(msg: string)  //") {
		t.Errorf("unit return rendering broken; src: %s", r.Source)
	}
	if strings.Contains(r.Source, "Log(msg: string) :") {
		t.Errorf("unit return should not have `:` clause; src: %s", r.Source)
	}
}

func TestEmitErrorOnlyReturn(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:   "mochi_go_example_com_m_m_Validate",
				Package:  "example.com/m",
				GoName:   "Validate",
				Params:   []wrapper.EmittedParam{{Name: "s", CType: "*C.char", Mochi: typemap.ScalarType{Name: "string"}}},
				Results:  []wrapper.EmittedParam{},
				HasError: true,
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if !strings.Contains(r.Source, "Result<unit, string>") {
		t.Errorf("error-only return should produce Result<unit, string>; src: %s", r.Source)
	}
}

func TestEmitValuePlusError(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_example_com_m_m_Lookup",
				Package: "example.com/m",
				GoName:  "Lookup",
				Params: []wrapper.EmittedParam{
					{Name: "k", CType: "*C.char", Mochi: typemap.ScalarType{Name: "string"}},
				},
				Results: []wrapper.EmittedParam{
					{Name: "ret0", CType: "C.long", Mochi: typemap.ScalarType{Name: "int"}},
				},
				HasError: true,
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if !strings.Contains(r.Source, "Result<int, string>") {
		t.Errorf("value+error should produce Result<int, string>; src: %s", r.Source)
	}
}

func TestEmitBytesReturn(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_example_com_m_m_Encode",
				Package: "example.com/m",
				GoName:  "Encode",
				Params: []wrapper.EmittedParam{
					{Name: "s", CType: "*C.char", Mochi: typemap.ScalarType{Name: "string"}},
				},
				Results: []wrapper.EmittedParam{
					{Name: "ret0", CType: "MochiSlice", Mochi: typemap.ScalarType{Name: "bytes"}},
				},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if !strings.Contains(r.Source, "extern fun m.Encode(s: string) : bytes") {
		t.Errorf("bytes return shape: %s", r.Source)
	}
}

func TestEmitBoolFloat(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_example_com_m_m_Negate",
				GoName:  "Negate",
				Params:  []wrapper.EmittedParam{{Name: "b", CType: "C.int", Mochi: typemap.ScalarType{Name: "bool"}}},
				Results: []wrapper.EmittedParam{{Name: "ret0", CType: "C.int", Mochi: typemap.ScalarType{Name: "bool"}}},
			},
			{
				Symbol:  "mochi_go_example_com_m_m_Sqrt",
				GoName:  "Sqrt",
				Params:  []wrapper.EmittedParam{{Name: "x", CType: "C.double", Mochi: typemap.ScalarType{Name: "float"}}},
				Results: []wrapper.EmittedParam{{Name: "ret0", CType: "C.double", Mochi: typemap.ScalarType{Name: "float"}}},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if !strings.Contains(r.Source, "extern fun m.Negate(b: bool) : bool") {
		t.Errorf("bool: %s", r.Source)
	}
	if !strings.Contains(r.Source, "extern fun m.Sqrt(x: float) : float") {
		t.Errorf("float: %s", r.Source)
	}
}

func TestEmitDeterministicOrdering(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{Symbol: "mochi_go_m_m_Zeta", GoName: "Zeta", Results: []wrapper.EmittedParam{{Mochi: typemap.ScalarType{Name: "int"}}}},
			{Symbol: "mochi_go_m_m_Alpha", GoName: "Alpha", Results: []wrapper.EmittedParam{{Mochi: typemap.ScalarType{Name: "int"}}}},
			{Symbol: "mochi_go_m_m_Mu", GoName: "Mu", Results: []wrapper.EmittedParam{{Mochi: typemap.ScalarType{Name: "int"}}}},
		},
	}
	e, _ := NewEmitter(w, "m")
	r1, _ := e.Emit()
	r2, _ := e.Emit()
	if r1.Source != r2.Source {
		t.Errorf("non-deterministic emit")
	}
	a := strings.Index(r1.Source, ".Alpha")
	b := strings.Index(r1.Source, ".Mu")
	c := strings.Index(r1.Source, ".Zeta")
	if !(a < b && b < c) {
		t.Errorf("ordering not stable: a=%d b=%d c=%d", a, b, c)
	}
}

func TestEmitMultiResultSkipped(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol:  "mochi_go_m_m_Pair",
				GoName:  "Pair",
				Params:  []wrapper.EmittedParam{},
				Results: []wrapper.EmittedParam{
					{Mochi: typemap.ScalarType{Name: "int"}},
					{Mochi: typemap.ScalarType{Name: "string"}},
				},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if len(r.Externs) != 0 {
		t.Errorf("multi-result not skipped; got %+v", r.Externs)
	}
	if len(r.Skipped) != 1 || !strings.Contains(r.Skipped[0].Reason, "phase 7.1") {
		t.Errorf("skip reason: %+v", r.Skipped)
	}
}

func TestEmitParamMissingMochiSkipped(t *testing.T) {
	w := &wrapper.Result{
		Funcs: []wrapper.EmittedFunc{
			{
				Symbol: "mochi_go_m_m_Bad",
				GoName: "Bad",
				Params: []wrapper.EmittedParam{{Name: "x", CType: "C.long", Mochi: nil}},
			},
		},
	}
	e, _ := NewEmitter(w, "m")
	r, _ := e.Emit()
	if len(r.Externs) != 0 {
		t.Errorf("param missing Mochi should skip")
	}
	if len(r.Skipped) != 1 {
		t.Errorf("expected 1 skip; got %+v", r.Skipped)
	}
}

func TestRenderResultTypeMatrix(t *testing.T) {
	cases := []struct {
		x    EmittedExtern
		want string
	}{
		{EmittedExtern{Result: nil, HasError: false}, ""},
		{EmittedExtern{Result: nil, HasError: true}, "Result<unit, string>"},
		{EmittedExtern{Result: typemap.ScalarType{Name: "int"}, HasError: false}, "int"},
		{EmittedExtern{Result: typemap.ScalarType{Name: "int"}, HasError: true}, "Result<int, string>"},
		{EmittedExtern{Result: typemap.ListType{Elem: typemap.ScalarType{Name: "string"}}, HasError: false}, "list<string>"},
	}
	for i, tc := range cases {
		got := renderResultType(&tc.x)
		if got != tc.want {
			t.Errorf("case %d: got %q want %q", i, got, tc.want)
		}
	}
}
