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

// TestPhase1EmitFragments locks the lowered PHP shape for each of the
// four print families (str / i64 / f64 / bool) plus the embedded-newline
// hello fixture. Phase 2 separately exercises these helpers as part of
// the scalar feature surface; this test pins them against the Phase 1
// "hello world" deliverable specifically, so a regression that breaks
// the i64 / f64 / bool entry points fails at Phase 1 (where the
// promise lives) instead of leaking into Phase 2's scalar coverage.
func TestPhase1EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "hello_int.mochi",
			wants: []string{
				"function mochi_print_i64(int $value): void",
				`mochi_print_i64(42);`,
			},
		},
		{
			fixture: "hello_float.mochi",
			wants: []string{
				"function mochi_print_f64(float $value): void",
				`mochi_print_f64(3.14);`,
			},
		},
		{
			fixture: "hello_bool.mochi",
			wants: []string{
				"function mochi_print_bool(bool $value): void",
				`echo $value ? "true\n" : "false\n";`,
				`mochi_print_bool(true);`,
			},
		},
		{
			// Embedded newlines inside the source string lower to a
			// PHP double-quoted literal containing the escape \n.
			// Pinning this keeps the lexer from accidentally
			// dropping the embedded line break or emitting it raw,
			// which would break the .out diff under php main.php.
			fixture: "hello_newline.mochi",
			wants: []string{
				`mochi_print_str("line1\nline2");`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase01-hello", c.fixture)
			if _, err := os.Stat(mochiPath); err != nil {
				t.Skipf("fixture missing: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(mochiPath, outDir, TargetPhpSource)
			if err != nil {
				t.Fatalf("Build(%s): %v", c.fixture, err)
			}
			data, err := os.ReadFile(p)
			if err != nil {
				t.Fatalf("read %s: %v", p, err)
			}
			src := string(data)
			for _, want := range c.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s: emitted source missing %q\n---\n%s", c.fixture, want, src)
				}
			}
		})
	}
}
