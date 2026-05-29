package library

import (
	"strings"
	"testing"
)

func TestRenderCHeaderIncludeGuard(t *testing.T) {
	api := PublicAPI{CrateName: "demo", Version: "1.0.0"}
	got := RenderCHeader(api)
	for _, want := range []string{
		"#ifndef DEMO_H",
		"#define DEMO_H",
		"#endif // DEMO_H",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\n%s", want, got)
		}
	}
}

func TestRenderCHeaderIncludesStdint(t *testing.T) {
	got := RenderCHeader(PublicAPI{CrateName: "demo", Version: "1.0.0"})
	for _, want := range []string{
		"#include <stdint.h>",
		"#include <stddef.h>",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\n%s", want, got)
		}
	}
}

func TestRenderCHeaderExternCWrap(t *testing.T) {
	got := RenderCHeader(PublicAPI{CrateName: "demo", Version: "1.0.0"})
	if !strings.Contains(got, "#ifdef __cplusplus\nextern \"C\" {\n#endif") {
		t.Errorf("missing extern C opening:\n%s", got)
	}
	if !strings.Contains(got, "} // extern \"C\"") {
		t.Errorf("missing extern C closing:\n%s", got)
	}
}

func TestRenderCHeaderEmitsReprCStruct(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemStruct{
				Name: "Point",
				Fields: []Field{
					{Name: "x", Type: "f64"},
					{Name: "y", Type: "f64"},
				},
				ReprC: true,
			},
		},
	}
	got := RenderCHeader(api)
	for _, want := range []string{
		"typedef struct Point {",
		"    double x;",
		"    double y;",
		"} Point;",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\n%s", want, got)
		}
	}
}

func TestRenderCHeaderSkipsNonReprCStruct(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemStruct{Name: "Hidden", Fields: []Field{{Name: "x", Type: "i64"}}},
		},
	}
	got := RenderCHeader(api)
	if strings.Contains(got, "typedef struct Hidden") {
		t.Errorf("non-repr(C) struct should be skipped:\n%s", got)
	}
}

func TestRenderCHeaderEmitsUnitEnum(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemEnum{
				Name:     "Status",
				Variants: []Variant{{Name: "Ok"}, {Name: "Err"}},
				ReprC:    true,
			},
		},
	}
	got := RenderCHeader(api)
	for _, want := range []string{
		"typedef enum Status {",
		"    Status_Ok,",
		"    Status_Err",
		"} Status;",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\n%s", want, got)
		}
	}
}

func TestRenderCHeaderDefersTaggedEnum(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemEnum{
				Name: "Shape",
				Variants: []Variant{
					{Name: "Circle", Fields: []Field{{Name: "r", Type: "f64"}}},
				},
				ReprC: true,
			},
		},
	}
	got := RenderCHeader(api)
	if !strings.Contains(got, "render deferred to phase 12") {
		t.Errorf("tagged enum should be deferred with a note:\n%s", got)
	}
}

func TestRenderCHeaderEmitsExternFnDecl(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemFn{
				Name:   "demo_add",
				Params: []Param{{"a", "i64"}, {"b", "i64"}},
				Return: "i64",
				Extern: true,
			},
		},
	}
	got := RenderCHeader(api)
	want := "int64_t demo_add(int64_t a, int64_t b);"
	if !strings.Contains(got, want) {
		t.Errorf("missing %q\n%s", want, got)
	}
}

func TestRenderCHeaderSkipsNonExternFn(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemFn{Name: "private_thing", Return: "i64", Body: "0"},
		},
	}
	got := RenderCHeader(api)
	if strings.Contains(got, "private_thing") {
		t.Errorf("non-extern fn should be skipped:\n%s", got)
	}
}

func TestRenderCHeaderVoidParams(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo", Version: "1.0.0",
		Items: []Item{
			ItemFn{Name: "ping", Extern: true, Return: "()"},
		},
	}
	got := RenderCHeader(api)
	if !strings.Contains(got, "void ping(void);") {
		t.Errorf("expected `void ping(void)`:\n%s", got)
	}
}

func TestRustTypeToC(t *testing.T) {
	cases := map[string]string{
		"i8":             "int8_t",
		"i16":            "int16_t",
		"i32":            "int32_t",
		"i64":            "int64_t",
		"u8":             "uint8_t",
		"u16":            "uint16_t",
		"u32":            "uint32_t",
		"u64":            "uint64_t",
		"isize":          "intptr_t",
		"usize":          "size_t",
		"f32":            "float",
		"f64":            "double",
		"bool":           "bool",
		"c_char":         "char",
		"*const c_char":  "const char*",
		"*mut c_char":    "char*",
		"*const i32":     "const int32_t*",
		"*mut u64":       "uint64_t*",
		"":               "",
		"()":             "",
		"MyOpaqueStruct": "MyOpaqueStruct",
	}
	for in, want := range cases {
		if got := rustTypeToC(in); got != want {
			t.Errorf("rustTypeToC(%q)=%q want %q", in, got, want)
		}
	}
}

func TestRenderCHeaderHyphenatedCrateName(t *testing.T) {
	api := PublicAPI{CrateName: "rust-num", Version: "1.0.0"}
	got := RenderCHeader(api)
	if !strings.Contains(got, "#ifndef RUST_NUM_H") {
		t.Errorf("hyphen should become underscore in guard:\n%s", got)
	}
}
