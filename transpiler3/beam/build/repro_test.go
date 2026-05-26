package build

import (
	"crypto/sha256"
	"encoding/hex"
	"io"
	"os"
	"path/filepath"
	"testing"
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

func buildAndHash(t *testing.T, mochiPath string) string {
	t.Helper()
	out := filepath.Join(t.TempDir(), "fixture.escript")
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, out, TargetEscript); err != nil {
		t.Fatalf("Build(%s): %v", mochiPath, err)
	}
	f, err := os.Open(out)
	if err != nil {
		t.Fatalf("open escript: %v", err)
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		t.Fatalf("hash escript: %v", err)
	}
	return hex.EncodeToString(h.Sum(nil))
}
