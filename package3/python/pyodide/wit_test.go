package pyodide

import (
	"strings"
	"testing"
)

func TestWITTypePrimitives(t *testing.T) {
	cases := map[WITKind]string{
		WITBool:   "bool",
		WITS8:     "s8",
		WITS16:    "s16",
		WITS32:    "s32",
		WITS64:    "s64",
		WITU8:     "u8",
		WITU16:    "u16",
		WITU32:    "u32",
		WITU64:    "u64",
		WITF32:    "f32",
		WITF64:    "f64",
		WITChar:   "char",
		WITString: "string",
	}
	for k, want := range cases {
		got := WITType{Kind: k}.Render()
		if got != want {
			t.Errorf("WITKind %v render = %q, want %q", k, got, want)
		}
	}
}

func TestWITTypeListAndOption(t *testing.T) {
	listOfString := WITType{Kind: WITList, ListOf: &WITType{Kind: WITString}}
	if got := listOfString.Render(); got != "list<string>" {
		t.Errorf("list<string> = %q", got)
	}
	optS32 := WITType{Kind: WITOption, Optional: &WITType{Kind: WITS32}}
	if got := optS32.Render(); got != "option<s32>" {
		t.Errorf("option<s32> = %q", got)
	}
	listOfOptF64 := WITType{Kind: WITList, ListOf: &WITType{Kind: WITOption, Optional: &WITType{Kind: WITF64}}}
	if got := listOfOptF64.Render(); got != "list<option<f64>>" {
		t.Errorf("nested render = %q", got)
	}
}

func TestWITTypeRef(t *testing.T) {
	if got := (WITType{Kind: WITRef, Name: "user-record"}).Render(); got != "user-record" {
		t.Errorf("ref render = %q", got)
	}
}

func TestWITTypePanics(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("expected panic on unknown kind")
		}
	}()
	_ = WITType{}.Render()
}

func TestWITTypeListPanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("expected panic on list with no element")
		}
	}()
	_ = WITType{Kind: WITList}.Render()
}

func TestWITTypeOptionPanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("expected panic on option with no inner")
		}
	}()
	_ = WITType{Kind: WITOption}.Render()
}

func TestWITTypeRefPanic(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("expected panic on ref with no name")
		}
	}()
	_ = WITType{Kind: WITRef}.Render()
}

func TestWITFuncRender(t *testing.T) {
	f := WITFunc{
		Name: "compute",
		Params: []WITField{
			{Name: "value-in", Type: WITType{Kind: WITS32}},
			{Name: "label", Type: WITType{Kind: WITString}},
		},
		Return: WITType{Kind: WITF64},
	}
	got := f.Render()
	want := "compute: func(value-in: s32, label: string) -> f64"
	if got != want {
		t.Errorf("render = %q, want %q", got, want)
	}
}

func TestWITFuncRenderNoReturn(t *testing.T) {
	f := WITFunc{Name: "ping"}
	if got := f.Render(); got != "ping: func()" {
		t.Errorf("render = %q", got)
	}
}

func TestWITWorldValidate(t *testing.T) {
	good := WITWorld{
		Package: "mochi:py-bridge",
		Name:    "demo-world",
		Records: []WITRecordDecl{
			{Name: "user-rec", Fields: []WITField{{Name: "id", Type: WITType{Kind: WITU64}}}},
		},
		Exports: []WITFunc{
			{Name: "get-user", Return: WITType{Kind: WITRef, Name: "user-rec"}},
		},
	}
	if err := good.Validate(); err != nil {
		t.Errorf("good world: %v", err)
	}

	bad := []WITWorld{
		{Package: "", Name: "x"},                                                                                   // empty package
		{Package: "mochi", Name: "BadWorld"},                                                                       // non-kebab world
		{Package: "mochi", Name: "x", Records: []WITRecordDecl{{Name: "Rec"}}},                                     // non-kebab record
		{Package: "mochi", Name: "x", Records: []WITRecordDecl{{Name: "rec"}, {Name: "rec"}}},                      // dup record
		{Package: "mochi", Name: "x", Records: []WITRecordDecl{{Name: "rec", Fields: []WITField{{Name: "ID"}}}}},   // non-kebab field
		{Package: "mochi", Name: "x", Exports: []WITFunc{{Name: "Fn"}}},                                            // non-kebab fn
		{Package: "mochi", Name: "x", Exports: []WITFunc{{Name: "fn"}, {Name: "fn"}}},                              // dup fn
		{Package: "mochi", Name: "x", Exports: []WITFunc{{Name: "fn", Params: []WITField{{Name: "BadParam"}}}}},    // non-kebab param
	}
	for i, w := range bad {
		if err := w.Validate(); err == nil {
			t.Errorf("bad[%d] expected error", i)
		}
	}
}

func TestWITWorldRender(t *testing.T) {
	w := WITWorld{
		Package: "mochi:py-bridge",
		Name:    "numpy-demo",
		Records: []WITRecordDecl{
			{Name: "vector", Fields: []WITField{
				{Name: "x", Type: WITType{Kind: WITF64}},
				{Name: "y", Type: WITType{Kind: WITF64}},
			}},
		},
		Imports: []WITFunc{
			{Name: "log", Params: []WITField{{Name: "msg", Type: WITType{Kind: WITString}}}},
		},
		Exports: []WITFunc{
			{Name: "dot", Params: []WITField{
				{Name: "a", Type: WITType{Kind: WITRef, Name: "vector"}},
				{Name: "b", Type: WITType{Kind: WITRef, Name: "vector"}},
			}, Return: WITType{Kind: WITF64}},
		},
	}
	got, err := w.Render()
	if err != nil {
		t.Fatalf("Render: %v", err)
	}
	wantSnippets := []string{
		"package mochi:py-bridge;",
		"world numpy-demo {",
		"  record vector {",
		"    x: f64,",
		"    y: f64,",
		"  import log: func(msg: string);",
		"  export dot: func(a: vector, b: vector) -> f64;",
	}
	for _, s := range wantSnippets {
		if !strings.Contains(got, s) {
			t.Errorf("rendered world missing %q\n--- got ---\n%s", s, got)
		}
	}
}

func TestWITWorldRenderDeterministic(t *testing.T) {
	w := WITWorld{
		Package: "mochi:bridge",
		Name:    "demo",
		Exports: []WITFunc{
			{Name: "zeta"},
			{Name: "alpha"},
			{Name: "mu"},
		},
	}
	first, _ := w.Render()
	second, _ := w.Render()
	if first != second {
		t.Error("Render is not deterministic across calls")
	}
	idxAlpha := strings.Index(first, "alpha")
	idxMu := strings.Index(first, "mu")
	idxZeta := strings.Index(first, "zeta")
	if !(idxAlpha < idxMu && idxMu < idxZeta) {
		t.Errorf("exports not sorted: %v %v %v", idxAlpha, idxMu, idxZeta)
	}
}

func TestWITWorldRenderRejectsInvalid(t *testing.T) {
	w := WITWorld{Package: "", Name: "x"}
	if _, err := w.Render(); err == nil {
		t.Error("expected validate error")
	}
}

func TestIsKebab(t *testing.T) {
	yes := []string{"a", "abc", "abc-def", "a1", "abc-def-ghi"}
	no := []string{"", "-a", "a-", "1abc", "Abc", "abc_def", "abc--def" /* double-hyphen */, "abc--"}
	for _, s := range yes {
		if !isKebab(s) {
			t.Errorf("isKebab(%q) = false, want true", s)
		}
	}
	for _, s := range no {
		if isKebab(s) {
			t.Errorf("isKebab(%q) = true, want false", s)
		}
	}
}
