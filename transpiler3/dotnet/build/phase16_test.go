package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/dotnet/lower"
)

// TestPhase16Reproducible is the gate test for MEP-48 Phase 16: deterministic builds.
// It builds the same Mochi source twice with cache disabled and asserts the resulting
// .dll files are bit-identical (same SHA-256). The <Deterministic>true</Deterministic>
// property in the generated .csproj makes Roslyn produce byte-identical assemblies.
func TestPhase16Reproducible(t *testing.T) {
	// Use hello-world fixtures from Phase 1 as the reproducibility corpus.
	fixtureBase := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "dotnet", "fixtures", "phase01-hello")
	entries, err := os.ReadDir(fixtureBase)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureBase, err)
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureBase, e.Name())

		t.Run(name, func(t *testing.T) {
			// Build 1
			out1 := t.TempDir()
			d1 := &Driver{CacheDir: t.TempDir(), NoCache: true}
			if err := d1.Build(mochiPath, out1, TargetFxDependent); err != nil {
				t.Fatalf("Build1(%s): %v", name, err)
			}

			// Build 2
			out2 := t.TempDir()
			d2 := &Driver{CacheDir: t.TempDir(), NoCache: true}
			if err := d2.Build(mochiPath, out2, TargetFxDependent); err != nil {
				t.Fatalf("Build2(%s): %v", name, err)
			}

			className := lower.ClassName(mochiPath)
			dll := className + ".dll"

			hash1, err := sha256File(filepath.Join(out1, dll))
			if err != nil {
				t.Fatalf("sha256 build1 %s: %v", dll, err)
			}
			hash2, err := sha256File(filepath.Join(out2, dll))
			if err != nil {
				t.Fatalf("sha256 build2 %s: %v", dll, err)
			}

			if hash1 != hash2 {
				t.Errorf("non-deterministic build: SHA-256 mismatch for %s\nbuild1: %s\nbuild2: %s", dll, hash1, hash2)
			}
		})
	}
}

func sha256File(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return fmt.Sprintf("%x", h.Sum(nil)), nil
}
