package library

import (
	"strings"
	"testing"
)

// TestPhase09TargetRustLibrary is the MEP-73 Phase 9 sentinel. It
// pins the end-to-end contract that a PublicAPI surface makes with
// the publish-direction emit pipeline: Cargo.toml shape, src/lib.rs
// layout, and (when CHeader is true) a cbindgen-compatible header.
func TestPhase09TargetRustLibrary(t *testing.T) {
	t.Run("publishable_crate_shape", func(t *testing.T) {
		api := PublicAPI{
			CrateName: "mochi_demo",
			Version:   "0.1.0",
			Package: PackageMeta{
				Description: "A demo crate published from Mochi",
				License:     "MIT OR Apache-2.0",
				Repository:  "https://github.com/mochilang/demo",
				Keywords:    []string{"demo", "mochi"},
				Categories:  []string{"development-tools"},
				Authors:     []string{"Mochi <team@mochi-lang.org>"},
				Readme:      "README.md",
			},
			Items: []Item{
				ItemStruct{
					Name:    "Point",
					Fields:  []Field{{Name: "x", Type: "f64", Pub: true}, {Name: "y", Type: "f64", Pub: true}},
					ReprC:   true,
					Derives: []string{"Clone", "Copy", "Debug"},
				},
				ItemFn{
					Name:   "distance",
					Params: []Param{{"a", "&Point"}, {"b", "&Point"}},
					Return: "f64",
					Body:   "let dx = a.x - b.x;\nlet dy = a.y - b.y;\n(dx * dx + dy * dy).sqrt()",
				},
				ItemFn{
					Name:   "demo_point_new",
					Params: []Param{{"x", "f64"}, {"y", "f64"}},
					Return: "Point",
					Body:   "Point { x, y }",
					Extern: true,
				},
			},
			CHeader: true,
			Dependencies: map[string]string{
				"libm": "0.2",
			},
		}

		files, err := Render(api)
		if err != nil {
			t.Fatalf("render: %v", err)
		}

		// Exactly 3 files: Cargo.toml, src/lib.rs, include/<crate>.h.
		want := []string{"Cargo.toml", "include/mochi_demo.h", "src/lib.rs"}
		got := files.Sorted()
		if len(got) != len(want) {
			t.Fatalf("expected %d files, got %d: %v", len(want), len(got), got)
		}
		for i := range want {
			if got[i] != want[i] {
				t.Fatalf("file[%d]=%q want %q", i, got[i], want[i])
			}
		}

		manifest := files["Cargo.toml"]
		for _, expect := range []string{
			"name = \"mochi_demo\"",
			"version = \"0.1.0\"",
			"edition = \"2021\"",
			"license = \"MIT OR Apache-2.0\"",
			"repository = \"https://github.com/mochilang/demo\"",
			"readme = \"README.md\"",
			"keywords = [\"demo\", \"mochi\"]",
			"categories = [\"development-tools\"]",
			"crate-type = [\"rlib\", \"cdylib\"]",
			"[dependencies]",
			"libm = \"0.2\"",
		} {
			if !strings.Contains(manifest, expect) {
				t.Errorf("manifest missing %q\n--- manifest ---\n%s", expect, manifest)
			}
		}

		lib := files["src/lib.rs"]
		for _, expect := range []string{
			"#[repr(C)]\n",
			"#[derive(Clone, Copy, Debug)]\n",
			"pub struct Point {\n",
			"    pub x: f64,\n",
			"    pub y: f64,\n",
			"pub fn distance(a: &Point, b: &Point) -> f64 {\n",
			"#[no_mangle]\npub extern \"C\" fn demo_point_new(x: f64, y: f64) -> Point",
		} {
			if !strings.Contains(lib, expect) {
				t.Errorf("lib.rs missing %q\n--- lib.rs ---\n%s", expect, lib)
			}
		}

		hdr := files["include/mochi_demo.h"]
		for _, expect := range []string{
			"#ifndef MOCHI_DEMO_H",
			"#include <stdint.h>",
			"extern \"C\" {",
			"typedef struct Point {",
			"    double x;",
			"    double y;",
			"} Point;",
			"Point demo_point_new(double x, double y);",
		} {
			if !strings.Contains(hdr, expect) {
				t.Errorf("header missing %q\n--- header ---\n%s", expect, hdr)
			}
		}
		// The non-extern fn must NOT appear in the header.
		if strings.Contains(hdr, "distance(") {
			t.Errorf("header should not declare non-extern `distance`:\n%s", hdr)
		}
	})

	t.Run("determinism", func(t *testing.T) {
		api := PublicAPI{
			CrateName: "demo",
			Version:   "1.0.0",
			Items: []Item{
				ItemFn{Name: "f", Return: "i64", Body: "42"},
			},
			Dependencies: map[string]string{
				"serde":     "1.0.0",
				"anyhow":    "1.0.86",
				"thiserror": "1.0.61",
			},
		}
		a, _ := Render(api)
		b, _ := Render(api)
		for k, v := range a {
			if b[k] != v {
				t.Errorf("file %q changed between runs", k)
			}
		}
	})

	t.Run("nostd_subset", func(t *testing.T) {
		api := PublicAPI{
			CrateName: "demo",
			Version:   "0.1.0",
			NoStd:     true,
			Items: []Item{
				ItemFn{Name: "add", Params: []Param{{"a", "i64"}, {"b", "i64"}}, Return: "i64", Body: "a + b"},
			},
		}
		files, err := Render(api)
		if err != nil {
			t.Fatalf("render: %v", err)
		}
		lib := files["src/lib.rs"]
		if !strings.HasPrefix(lib, "#![no_std]\n") {
			t.Errorf("expected no_std preamble:\n%s", lib)
		}
		if !strings.Contains(lib, "extern crate alloc;") {
			t.Errorf("expected extern crate alloc:\n%s", lib)
		}
	})

	t.Run("c_header_omitted_by_default", func(t *testing.T) {
		api := PublicAPI{
			CrateName: "demo", Version: "0.1.0",
			Items: []Item{ItemFn{Name: "f", Return: "i64", Body: "0", Extern: true}},
		}
		files, err := Render(api)
		if err != nil {
			t.Fatalf("render: %v", err)
		}
		for path := range files {
			if strings.HasPrefix(path, "include/") {
				t.Errorf("unexpected header file when CHeader=false: %s", path)
			}
		}
	})
}
