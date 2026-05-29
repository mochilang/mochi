package build

import (
	"crypto/sha256"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase16Reproducible verifies that Mochi-to-Kotlin-JVM compilation is
// byte-reproducible: the same .mochi source produces a bit-identical .jar
// when built twice in separate directories.
//
// Strategy: build once via cachedBuild (warms the SHA-256 jar cache), then
// build again fresh via BuildFromKt (bypasses cache). If kotlinc is
// deterministic, both jars have the same SHA-256.
//
// Kotlin 2.0+ uses ZipOutputStream with all entry timestamps pinned to
// the ZIP epoch (1980-01-01 00:00:00 UTC) and a stable class ordering, so
// kotlinc produces bit-identical jars for identical Kotlin source.
func TestPhase16Reproducible(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase16-repro")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		e := e
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			srcPath := filepath.Join(fixtureDir, e.Name())

			d := &Driver{}
			ktSrc, err := d.GenerateKt(srcPath)
			if err != nil {
				t.Fatalf("GenerateKt(%s): %v", name, err)
			}

			// Verify GenerateKt is deterministic (pure Go lowering).
			ktSrc2, err := d.GenerateKt(srcPath)
			if err != nil {
				t.Fatalf("GenerateKt 2(%s): %v", name, err)
			}
			if ktSrc != ktSrc2 {
				t.Errorf("GenerateKt non-deterministic for %s", name)
				return
			}

			// Build 1: use cached build (may compile or load from cache).
			dir1 := t.TempDir()
			jar1 := cachedBuild(t, srcPath, dir1)
			data1, err := os.ReadFile(jar1)
			if err != nil {
				t.Fatalf("read jar 1: %v", err)
			}

			// Build 2: fresh compile in a separate temp dir (bypasses cache).
			dir2 := t.TempDir()
			jar2, err := d.BuildFromKt(ktSrc, dir2)
			if err != nil {
				t.Fatalf("build 2 (fresh): %v", err)
			}
			data2, err := os.ReadFile(jar2)
			if err != nil {
				t.Fatalf("read jar 2: %v", err)
			}

			h1 := sha256.Sum256(data1)
			h2 := sha256.Sum256(data2)
			if h1 != h2 {
				t.Errorf("non-reproducible build for %s:\n  cached sha256:  %x\n  fresh sha256:   %x",
					name, h1, h2)
			} else {
				t.Logf("reproducible: sha256=%x (%d bytes)", h1, len(data1))
			}
		})
	}
}
