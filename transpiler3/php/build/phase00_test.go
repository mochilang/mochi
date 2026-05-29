package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase0Skeleton checks that the PHP transpiler can lower and emit
// an empty Mochi program: no top-level statements, just an implicit
// mochi_main() entry. The emitted main.php must produce no stdout.
func TestPhase0Skeleton(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase00-skeleton")
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

// TestPhase0EmitWithoutPhp covers the path where the host has no `php`
// binary: we still want the lowerer + emit step to succeed so MEP-55
// can be developed on a host without PHP installed.
func TestPhase0EmitWithoutPhp(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase00-skeleton")
	mochiPath := filepath.Join(fixtureDir, "empty.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("empty.mochi fixture missing: %v", err)
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
		"declare(strict_types=1);",
		"function mochi_main()",
		"mochi_main();",
	} {
		if !strings.Contains(src, want) {
			t.Errorf("emitted source missing %q\n---\n%s", want, src)
		}
	}
}
