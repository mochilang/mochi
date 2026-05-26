package build

import (
	"os"
	"path/filepath"
	"testing"
)

// TestPhase15_1Rebar3Project verifies that TargetRebar3Project emits a
// directory containing rebar.config, src/mochi_app.app.src, at least one
// .erl file in src/, and at least one .beam file in ebin/.
// Phase 15.1.
func TestPhase15_1Rebar3Project(t *testing.T) {
	src := writeTemp(t, "print(42)\n", "fixture.mochi")
	outDir := filepath.Join(t.TempDir(), "rebar3proj")

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outDir, TargetRebar3Project); err != nil {
		t.Fatalf("Build TargetRebar3Project: %v", err)
	}

	must := []string{
		filepath.Join(outDir, "rebar.config"),
		filepath.Join(outDir, "src", "mochi_app.app.src"),
	}
	for _, p := range must {
		if _, err := os.Stat(p); err != nil {
			t.Errorf("missing expected file %s: %v", p, err)
		}
	}

	erlFiles, err := filepath.Glob(filepath.Join(outDir, "src", "*.erl"))
	if err != nil || len(erlFiles) == 0 {
		t.Errorf("expected at least one .erl in src/, got: %v (err: %v)", erlFiles, err)
	}

	beamFiles, err := filepath.Glob(filepath.Join(outDir, "ebin", "*.beam"))
	if err != nil || len(beamFiles) == 0 {
		t.Errorf("expected at least one .beam in ebin/, got: %v (err: %v)", beamFiles, err)
	}
}

// TestPhase15_1Rebar3ProjectWithDeps verifies that mochi.toml [dependencies]
// entries appear in the emitted rebar.config.
// Phase 15.1.
func TestPhase15_1Rebar3ProjectWithDeps(t *testing.T) {
	dir := t.TempDir()
	mochiPath := filepath.Join(dir, "app.mochi")
	if err := os.WriteFile(mochiPath, []byte("print(1)\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	tomlContent := "[dependencies]\ncowboy = \"2.10.0\"\n"
	if err := os.WriteFile(filepath.Join(dir, "mochi.toml"), []byte(tomlContent), 0o644); err != nil {
		t.Fatal(err)
	}

	outDir := filepath.Join(t.TempDir(), "rebar3proj")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outDir, TargetRebar3Project); err != nil {
		t.Fatalf("Build TargetRebar3Project: %v", err)
	}

	rc, err := os.ReadFile(filepath.Join(outDir, "rebar.config"))
	if err != nil {
		t.Fatalf("read rebar.config: %v", err)
	}
	if !contains(string(rc), "cowboy") {
		t.Errorf("rebar.config missing cowboy dep:\n%s", rc)
	}
}

// TestPhase15_2MixProject verifies that TargetMixProject emits a directory
// containing mix.exs and at least one .beam in ebin/.
// Phase 15.2.
func TestPhase15_2MixProject(t *testing.T) {
	src := writeTemp(t, "print(99)\n", "fixture.mochi")
	outDir := filepath.Join(t.TempDir(), "mixproj")

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, outDir, TargetMixProject); err != nil {
		t.Fatalf("Build TargetMixProject: %v", err)
	}

	mixExs := filepath.Join(outDir, "mix.exs")
	if _, err := os.Stat(mixExs); err != nil {
		t.Fatalf("mix.exs not emitted: %v", err)
	}

	content, err := os.ReadFile(mixExs)
	if err != nil {
		t.Fatalf("read mix.exs: %v", err)
	}
	if !contains(string(content), "defmodule") || !contains(string(content), "mochi_app") {
		t.Errorf("mix.exs missing expected content:\n%s", content)
	}

	beamFiles, err := filepath.Glob(filepath.Join(outDir, "ebin", "*.beam"))
	if err != nil || len(beamFiles) == 0 {
		t.Errorf("expected at least one .beam in ebin/, got: %v (err: %v)", beamFiles, err)
	}
}

// TestPhase15_2MixProjectWithDeps verifies mochi.toml deps appear in mix.exs.
// Phase 15.2.
func TestPhase15_2MixProjectWithDeps(t *testing.T) {
	dir := t.TempDir()
	mochiPath := filepath.Join(dir, "app.mochi")
	if err := os.WriteFile(mochiPath, []byte("print(2)\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	tomlContent := "[dependencies]\njason = \"1.4.0\"\n"
	if err := os.WriteFile(filepath.Join(dir, "mochi.toml"), []byte(tomlContent), 0o644); err != nil {
		t.Fatal(err)
	}

	outDir := filepath.Join(t.TempDir(), "mixproj")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outDir, TargetMixProject); err != nil {
		t.Fatalf("Build TargetMixProject: %v", err)
	}

	content, err := os.ReadFile(filepath.Join(outDir, "mix.exs"))
	if err != nil {
		t.Fatalf("read mix.exs: %v", err)
	}
	if !contains(string(content), "jason") {
		t.Errorf("mix.exs missing jason dep:\n%s", content)
	}
}

func writeTemp(t *testing.T, code, name string) string {
	t.Helper()
	dir := t.TempDir()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte(code), 0o644); err != nil {
		t.Fatal(err)
	}
	return p
}

func contains(s, sub string) bool {
	return len(s) >= len(sub) && (s == sub || len(sub) == 0 ||
		func() bool {
			for i := 0; i <= len(s)-len(sub); i++ {
				if s[i:i+len(sub)] == sub {
					return true
				}
			}
			return false
		}())
}
