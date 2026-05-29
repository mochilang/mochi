package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase1Hello iterates the Phase 1 fixture directory and runs every
// .mochi file through the PHP transpiler, then diffs the emitted main.php
// output against the matching .out file under `php`.
//
// Tests skip cleanly when `php` is not on PATH; CI installs PHP 8.4 via
// shivammathur/setup-php@v2 so they always exercise the full pipeline in
// the upstream check.
func TestPhase1Hello(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase01-hello")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runPhpFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

// TestPhase1EmitWithoutPhp confirms the lowerer + emit pass can produce
// a syntactically plausible `mochi_print_str` PHP body for a hello-world
// fixture even when the host has no `php` installed.
func TestPhase1EmitWithoutPhp(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase01-hello")
	mochiPath := filepath.Join(fixtureDir, "hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("hello.mochi missing: %v", err)
	}
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	p, err := d.Build(mochiPath, outDir, TargetPhpSource)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	data, err := os.ReadFile(p)
	if err != nil {
		t.Fatalf("read emitted file: %v", err)
	}
	src := string(data)
	for _, want := range []string{
		"<?php",
		"function mochi_print_str(string $value): void",
		"function mochi_main(): void",
		`mochi_print_str("hello, world");`,
		"mochi_main();",
	} {
		if !strings.Contains(src, want) {
			t.Errorf("emitted source missing %q\n---\n%s", want, src)
		}
	}
}
