package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase12_2MochiToml verifies that when a mochi.toml with a [dependencies]
// section exists next to the source file, Driver.Build emits a rebar.config
// alongside the output artifact.
//
// Gate: rebar.config is written with correct {deps, [...]} entries.
func TestPhase12_2MochiToml(t *testing.T) {
	// Create a temporary directory with a minimal .mochi file and a mochi.toml.
	dir := t.TempDir()

	mochiSrc := filepath.Join(dir, "main.mochi")
	if err := os.WriteFile(mochiSrc, []byte(`print("hello")`+"\n"), 0o644); err != nil {
		t.Fatalf("write main.mochi: %v", err)
	}

	tomlContent := `[package]
name = "myapp"
version = "0.1.0"

[dependencies]
jsx = "3.1.0"
cowboy = "2.10.0"
`
	tomlPath := filepath.Join(dir, "mochi.toml")
	if err := os.WriteFile(tomlPath, []byte(tomlContent), 0o644); err != nil {
		t.Fatalf("write mochi.toml: %v", err)
	}

	outDir := t.TempDir()
	escriptOut := filepath.Join(outDir, "main.escript")

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiSrc, escriptOut, TargetEscript); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}

	// rebar.config must exist next to the output file.
	rebarPath := filepath.Join(outDir, "rebar.config")
	data, err := os.ReadFile(rebarPath)
	if err != nil {
		t.Fatalf("rebar.config not created: %v", err)
	}

	got := string(data)

	// Must contain erl_opts.
	if !strings.Contains(got, "{erl_opts, [debug_info]}.") {
		t.Errorf("rebar.config missing erl_opts line\ngot:\n%s", got)
	}
	// Must contain both deps.
	if !strings.Contains(got, `{jsx, "3.1.0"}`) {
		t.Errorf("rebar.config missing jsx dep\ngot:\n%s", got)
	}
	if !strings.Contains(got, `{cowboy, "2.10.0"}`) {
		t.Errorf("rebar.config missing cowboy dep\ngot:\n%s", got)
	}
	// Must have a {deps, [...]} block.
	if !strings.Contains(got, "{deps, [") {
		t.Errorf("rebar.config missing deps block\ngot:\n%s", got)
	}
}

// TestPhase12_2NoToml verifies that when no mochi.toml exists, no rebar.config
// is written and the build still succeeds.
func TestPhase12_2NoToml(t *testing.T) {
	dir := t.TempDir()

	mochiSrc := filepath.Join(dir, "main.mochi")
	if err := os.WriteFile(mochiSrc, []byte(`print("hello")`+"\n"), 0o644); err != nil {
		t.Fatalf("write main.mochi: %v", err)
	}

	outDir := t.TempDir()
	escriptOut := filepath.Join(outDir, "main.escript")

	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiSrc, escriptOut, TargetEscript); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}

	rebarPath := filepath.Join(outDir, "rebar.config")
	if _, err := os.Stat(rebarPath); err == nil {
		t.Errorf("rebar.config should not exist when mochi.toml is absent")
	}
}
