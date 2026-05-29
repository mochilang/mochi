package library

import (
	"strings"
	"testing"
)

func TestRenderManifestBasic(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo",
		Version:   "1.0.0",
		Package:   PackageMeta{Description: "demo"},
	}
	got := RenderManifest(api)
	for _, want := range []string{
		"[package]",
		"name = \"demo\"",
		"version = \"1.0.0\"",
		"edition = \"2021\"",
		"description = \"demo\"",
		"[lib]",
		"crate-type = [\"rlib\", \"cdylib\"]",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("manifest missing %q\n--- output ---\n%s", want, got)
		}
	}
}

func TestRenderManifestCustomEdition(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo",
		Version:   "1.0.0",
		Package:   PackageMeta{Edition: "2024"},
	}
	got := RenderManifest(api)
	if !strings.Contains(got, "edition = \"2024\"") {
		t.Errorf("expected 2024 edition, got\n%s", got)
	}
}

func TestRenderManifestOmitsEmptyMetadata(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo",
		Version:   "1.0.0",
	}
	got := RenderManifest(api)
	for _, banned := range []string{
		"description = ",
		"license = ",
		"repository = ",
		"documentation = ",
		"homepage = ",
		"readme = ",
		"rust-version = ",
		"authors = ",
		"keywords = ",
		"categories = ",
	} {
		if strings.Contains(got, banned) {
			t.Errorf("manifest should not contain %q\n--- output ---\n%s", banned, got)
		}
	}
}

func TestRenderManifestFullMetadata(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo",
		Version:   "1.0.0",
		Package: PackageMeta{
			Description:   "a demo",
			License:       "MIT",
			Repository:    "https://github.com/mochilang/demo",
			Documentation: "https://docs.rs/demo",
			Homepage:      "https://mochi-lang.org/demo",
			Authors:       []string{"Mochi <team@mochi-lang.org>"},
			Keywords:      []string{"demo", "test"},
			Categories:    []string{"development-tools"},
			Readme:        "README.md",
			RustVersion:   "1.75",
		},
	}
	got := RenderManifest(api)
	for _, want := range []string{
		"description = \"a demo\"",
		"license = \"MIT\"",
		"repository = \"https://github.com/mochilang/demo\"",
		"documentation = \"https://docs.rs/demo\"",
		"homepage = \"https://mochi-lang.org/demo\"",
		"readme = \"README.md\"",
		"rust-version = \"1.75\"",
		"authors = [\"Mochi <team@mochi-lang.org>\"]",
		"keywords = [\"demo\", \"test\"]",
		"categories = [\"development-tools\"]",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("missing %q\n%s", want, got)
		}
	}
}

func TestRenderManifestDependenciesSorted(t *testing.T) {
	api := PublicAPI{
		CrateName: "demo",
		Version:   "1.0.0",
		Dependencies: map[string]string{
			"serde":   "1.0.0",
			"anyhow":  "1.0.86",
			"thiserror": "1.0.61",
		},
	}
	got := RenderManifest(api)
	if !strings.Contains(got, "[dependencies]") {
		t.Fatal("missing [dependencies] section")
	}
	idxA := strings.Index(got, "anyhow =")
	idxS := strings.Index(got, "serde =")
	idxT := strings.Index(got, "thiserror =")
	if idxA < 0 || idxS < 0 || idxT < 0 {
		t.Fatalf("missing dep rows: %s", got)
	}
	if !(idxA < idxS && idxS < idxT) {
		t.Errorf("deps not sorted alphabetically: %s", got)
	}
}

func TestRenderManifestOmitsDependenciesSectionWhenEmpty(t *testing.T) {
	api := PublicAPI{CrateName: "demo", Version: "1.0.0"}
	got := RenderManifest(api)
	if strings.Contains(got, "[dependencies]") {
		t.Errorf("should not emit [dependencies] when empty: %s", got)
	}
}

func TestRenderManifestLibSection(t *testing.T) {
	got := RenderManifest(PublicAPI{CrateName: "demo", Version: "1.0.0"})
	if !strings.Contains(got, "[lib]\ncrate-type = [\"rlib\", \"cdylib\"]") {
		t.Errorf("[lib] section malformed:\n%s", got)
	}
}
