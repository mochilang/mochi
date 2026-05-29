package pypackage

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/python/typemap"
)

// TestPhase10TargetPythonPackage is the Phase 10 umbrella sentinel. It builds
// a representative Package with a frozen dataclass, an async function, and a
// scalar constant, then renders both the sdist and wheel layouts, writes them
// to a temp dir, and asserts every expected artifact exists with non-empty
// content. The test exercises the publish direction end-to-end without
// running any external Python toolchain.
func TestPhase10TargetPythonPackage(t *testing.T) {
	p := Package{
		Distribution:   "mochi-phase10",
		Version:        "0.0.7",
		Summary:        "MEP-71 phase 10 sentinel package",
		License:        "Apache-2.0",
		Author:         "Mochi Team",
		HomePage:       "https://mochi-lang.org",
		RequiresPython: ">=3.12,<3.15",
		Dependencies:   []string{"httpx>=0.27,<0.28"},
		Exports: []Export{
			{Name: "Point", Kind: ExportRecord, Type: typemap.MochiType{
				Kind: typemap.KindRecord,
				Fields: []typemap.MochiField{
					{Name: "x", Type: typemap.MochiType{Kind: typemap.KindScalar, Name: "int"}},
					{Name: "y", Type: typemap.MochiType{Kind: typemap.KindScalar, Name: "int"}},
				},
			}},
			{Name: "fetch", Kind: ExportFunc, Type: typemap.MochiType{
				Kind: typemap.KindFun,
				Params: []typemap.MochiType{
					{Kind: typemap.KindScalar, Name: "string"},
					{Kind: typemap.KindAsync, Params: []typemap.MochiType{
						{Kind: typemap.KindScalar, Name: "string"},
					}},
				},
			}},
			{Name: "VERSION", Kind: ExportConstant, Type: typemap.MochiType{Kind: typemap.KindScalar, Name: "string"}},
		},
	}

	if err := p.Validate(); err != nil {
		t.Fatalf("Validate: %v", err)
	}

	sdist, err := RenderSdist(p)
	if err != nil {
		t.Fatalf("RenderSdist: %v", err)
	}
	wheel, err := RenderWheel(p)
	if err != nil {
		t.Fatalf("RenderWheel: %v", err)
	}

	root := t.TempDir()
	if _, err := sdist.Write(filepath.Join(root, "sdist")); err != nil {
		t.Fatalf("sdist write: %v", err)
	}
	if _, err := wheel.Write(filepath.Join(root, "wheel")); err != nil {
		t.Fatalf("wheel write: %v", err)
	}

	sdistFiles := []string{
		"mochi-phase10-0.0.7/pyproject.toml",
		"mochi-phase10-0.0.7/PKG-INFO",
		"mochi-phase10-0.0.7/README.md",
		"mochi-phase10-0.0.7/mochi_phase10/__init__.py",
		"mochi-phase10-0.0.7/mochi_phase10/__init__.pyi",
		"mochi-phase10-0.0.7/mochi_build/__init__.py",
	}
	for _, f := range sdistFiles {
		assertNonEmpty(t, filepath.Join(root, "sdist", f))
	}
	wheelFiles := []string{
		"mochi_phase10/__init__.py",
		"mochi_phase10/__init__.pyi",
		"mochi-phase10-0.0.7.dist-info/METADATA",
		"mochi-phase10-0.0.7.dist-info/WHEEL",
		"mochi-phase10-0.0.7.dist-info/RECORD",
	}
	for _, f := range wheelFiles {
		assertNonEmpty(t, filepath.Join(root, "wheel", f))
	}

	pyi, err := os.ReadFile(filepath.Join(root, "wheel", "mochi_phase10/__init__.pyi"))
	if err != nil {
		t.Fatalf("read pyi: %v", err)
	}
	body := string(pyi)
	for _, want := range []string{
		"class Point:",
		"x: int",
		"y: int",
		"def fetch(arg0: str) -> Awaitable[str]: ...",
		"VERSION: str",
		"from typing import",
		"Awaitable",
	} {
		if !strings.Contains(body, want) {
			t.Fatalf(".pyi missing %q\n%s", want, body)
		}
	}

	record, err := os.ReadFile(filepath.Join(root, "wheel", "mochi-phase10-0.0.7.dist-info/RECORD"))
	if err != nil {
		t.Fatalf("read RECORD: %v", err)
	}
	if !strings.Contains(string(record), "sha256=") {
		t.Fatalf("RECORD missing sha256 lines:\n%s", record)
	}

	backend, err := os.ReadFile(filepath.Join(root, "sdist", "mochi-phase10-0.0.7/mochi_build/__init__.py"))
	if err != nil {
		t.Fatalf("read backend: %v", err)
	}
	for _, want := range []string{"build_wheel", "build_sdist", "mochi pkg build"} {
		if !strings.Contains(string(backend), want) {
			t.Fatalf("backend missing %q", want)
		}
	}
}

func assertNonEmpty(t *testing.T, path string) {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	if info.Size() == 0 {
		t.Fatalf("file %s is empty", path)
	}
}
