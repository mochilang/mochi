package library

import (
	"strings"
	"testing"
)

func TestRenderLibRSPlainFn(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemFn{
				Name:   "add",
				Params: []Param{{"a", "i64"}, {"b", "i64"}},
				Return: "i64",
				Body:   "a + b",
			},
		},
	}
	got := RenderLibRS(api)
	want := "pub fn add(a: i64, b: i64) -> i64 {\n    a + b\n}\n"
	if got != want {
		t.Errorf("got %q want %q", got, want)
	}
}

func TestRenderLibRSExternFn(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemFn{
				Name:   "demo_add",
				Params: []Param{{"a", "i64"}, {"b", "i64"}},
				Return: "i64",
				Body:   "a + b",
				Extern: true,
			},
		},
	}
	got := RenderLibRS(api)
	for _, want := range []string{
		"#[no_mangle]",
		"pub extern \"C\" fn demo_add(a: i64, b: i64) -> i64",
		"    a + b\n",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in:\n%s", want, got)
		}
	}
}

func TestRenderLibRSVoidReturn(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemFn{
				Name:   "hello",
				Params: nil,
				Return: "()",
				Body:   "println!(\"hi\");",
			},
		},
	}
	got := RenderLibRS(api)
	if strings.Contains(got, "-> ()") {
		t.Errorf("should omit unit return:\n%s", got)
	}
	if !strings.Contains(got, "pub fn hello() {") {
		t.Errorf("missing fn header:\n%s", got)
	}
}

func TestRenderLibRSStruct(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemStruct{
				Name: "Point",
				Fields: []Field{
					{Name: "x", Type: "f64", Pub: true},
					{Name: "y", Type: "f64", Pub: true},
				},
				ReprC:   true,
				Derives: []string{"Clone", "Copy"},
			},
		},
	}
	got := RenderLibRS(api)
	for _, want := range []string{
		"#[repr(C)]\n",
		"#[derive(Clone, Copy)]\n",
		"pub struct Point {\n",
		"    pub x: f64,\n",
		"    pub y: f64,\n",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in:\n%s", want, got)
		}
	}
}

func TestRenderLibRSEnumUnitOnly(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemEnum{
				Name: "Status",
				Variants: []Variant{
					{Name: "Ok"},
					{Name: "Err"},
				},
				ReprC: true,
			},
		},
	}
	got := RenderLibRS(api)
	for _, want := range []string{
		"#[repr(C)]\n",
		"pub enum Status {\n",
		"    Ok,\n",
		"    Err,\n",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in:\n%s", want, got)
		}
	}
}

func TestRenderLibRSEnumWithFields(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemEnum{
				Name: "Shape",
				Variants: []Variant{
					{Name: "Circle", Fields: []Field{{Name: "r", Type: "f64"}}},
					{Name: "Point"},
				},
			},
		},
	}
	got := RenderLibRS(api)
	for _, want := range []string{
		"pub enum Shape {\n",
		"    Circle {\n",
		"        r: f64,\n",
		"    },\n",
		"    Point,\n",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in:\n%s", want, got)
		}
	}
}

func TestRenderLibRSNoStd(t *testing.T) {
	api := PublicAPI{
		NoStd: true,
		Items: []Item{ItemFn{Name: "x", Return: "()", Body: "()"}},
	}
	got := RenderLibRS(api)
	if !strings.HasPrefix(got, "#![no_std]\n") {
		t.Errorf("missing no_std attribute:\n%s", got)
	}
	if !strings.Contains(got, "extern crate alloc;") {
		t.Errorf("missing alloc extern crate:\n%s", got)
	}
}

func TestRenderLibRSDocComments(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemFn{
				Name:   "x",
				Return: "()",
				Body:   "()",
				Doc:    "Doc one\nDoc two",
			},
		},
	}
	got := RenderLibRS(api)
	for _, want := range []string{
		"/// Doc one\n",
		"/// Doc two\n",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q in:\n%s", want, got)
		}
	}
}

func TestRenderLibRSMultipleItemsSeparatedByBlankLine(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemFn{Name: "a", Return: "()", Body: "()"},
			ItemFn{Name: "b", Return: "()", Body: "()"},
		},
	}
	got := RenderLibRS(api)
	if !strings.Contains(got, "}\n\npub fn b") {
		t.Errorf("expected blank line between items:\n%s", got)
	}
}

func TestRenderLibRSBodyTrailingNewlineNormalised(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemFn{Name: "x", Return: "i64", Body: "42\n\n"},
		},
	}
	got := RenderLibRS(api)
	if strings.Contains(got, "\n    \n}") {
		t.Errorf("trailing blank lines in body should be trimmed:\n%s", got)
	}
}

func TestRenderLibRSPrivateField(t *testing.T) {
	api := PublicAPI{
		Items: []Item{
			ItemStruct{
				Name: "Hidden",
				Fields: []Field{
					{Name: "secret", Type: "i64", Pub: false},
				},
			},
		},
	}
	got := RenderLibRS(api)
	if strings.Contains(got, "pub secret") {
		t.Errorf("non-pub field should not have pub prefix:\n%s", got)
	}
	if !strings.Contains(got, "    secret: i64,\n") {
		t.Errorf("missing private field:\n%s", got)
	}
}
