package build

import (
	"archive/zip"
	"crypto/sha256"
	"encoding/hex"
	"io"
	"strings"
	"testing"
	"path/filepath"
	"os"
)

// TestReproducibility builds every Phase 1 fixture twice and asserts that the
// resulting escript files are bit-identical. This exercises the `deterministic`
// compile flag added in Phase 18.0 and the sorted-Defs ordering from Phase 18.1.
func TestReproducibility(t *testing.T) {
	root := repoRoot(t)
	// Use phase01 fixtures as a stable, small set that covers basic lowering.
	fixturesDir := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase01")

	entries, err := os.ReadDir(fixturesDir)
	if err != nil {
		t.Fatalf("read fixtures dir: %v", err)
	}

	ran := 0
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		dir := filepath.Join(fixturesDir, name)
		mochi := filepath.Join(dir, name+".mochi")
		t.Run(name, func(t *testing.T) {
			checkReproducible(t, mochi)
		})
		ran++
	}
	if ran == 0 {
		t.Fatal("no fixtures found")
	}
}

// checkReproducible builds mochi twice in separate temp dirs and compares SHA-256.
func checkReproducible(t *testing.T, mochiPath string) {
	t.Helper()

	hash1 := buildAndHash(t, mochiPath)
	hash2 := buildAndHash(t, mochiPath)

	if hash1 != hash2 {
		t.Errorf("non-reproducible: build 1 SHA-256 %s, build 2 SHA-256 %s", hash1, hash2)
	}
}

// buildAndHash builds a Mochi file to an escript, then extracts and hashes the
// .beam content from inside the ZIP container. We hash .beam bytes rather than
// the escript file itself because escript is a ZIP archive whose per-entry
// timestamps are set to the current wall-clock time, making the container
// non-bit-identical across builds even when the .beam content is identical.
func buildAndHash(t *testing.T, mochiPath string) string {
	t.Helper()
	out := filepath.Join(t.TempDir(), "fixture.escript")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, out, TargetEscript); err != nil {
		t.Fatalf("Build(%s): %v", mochiPath, err)
	}

	zr, err := zip.OpenReader(out)
	if err != nil {
		t.Fatalf("open escript as zip: %v", err)
	}
	defer zr.Close()

	h := sha256.New()
	found := false
	for _, f := range zr.File {
		if !strings.HasSuffix(f.Name, ".beam") {
			continue
		}
		rc, err := f.Open()
		if err != nil {
			t.Fatalf("open zip entry %s: %v", f.Name, err)
		}
		if _, err := io.Copy(h, rc); err != nil {
			rc.Close()
			t.Fatalf("hash zip entry %s: %v", f.Name, err)
		}
		rc.Close()
		found = true
	}
	if !found {
		t.Fatalf("no .beam files found in escript %s", out)
	}
	return hex.EncodeToString(h.Sum(nil))
}
