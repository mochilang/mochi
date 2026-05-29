package build

import (
	"bytes"
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase16Repro is the Phase 16 gate: two independent builds of the
// same Mochi source produce byte-identical PHP source when the driver
// is run with Deterministic = true.
//
// Unlike the Swift target (which produces a Mach-O binary whose UUID is
// unreliable on macOS), the PHP target emits plain text source. There
// is no embedded build timestamp or random padding, so reproducibility
// must hold on every host. The gate runs on darwin and linux alike.
//
// Each sub-test:
//   1. Builds the fixture with Deterministic=true into temp dir A.
//   2. Builds it again with Deterministic=true into temp dir B.
//   3. Compares the SHA-256 of the two emitted main.php files.
//
// A divergent hash means some non-deterministic input (map iteration
// order, time.Now, random ids, absolute temp-dir paths leaking into
// source) has crept into the lowerer.
func TestPhase16Repro(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase16-repro")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	hasFixtures := false
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		hasFixtures = true
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		t.Run(name, func(t *testing.T) {
			testPhpReproducibleBuild(t, mochiPath)
		})
	}
	if !hasFixtures {
		t.Fatal("no .mochi fixtures found")
	}
}

// TestPhase16ReproEndToEnd runs each repro fixture under PHP and asserts
// stdout matches the recorded .out. Skips when PHP is not on PATH.
// This is the runtime correctness side-channel: byte-equal source that
// produces wrong output is still a Phase 16 failure.
func TestPhase16ReproEndToEnd(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase16-repro")
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

// TestPhase16NonDeterministicBuildsAlsoMatch is a defensive cross-check:
// even with Deterministic = false, two builds of the same source from
// the same revision should still produce byte-identical PHP source. The
// PHP lowerer has no time-, random-, or path-derived sources of
// non-determinism, so a divergence here means we accidentally introduced
// one. Phase 16's promise is "deterministic mode is reproducible";
// keeping the default path stable too prevents a Phase-N regression
// from silently breaking caching downstream.
func TestPhase16NonDeterministicBuildsAlsoMatch(t *testing.T) {
	mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase16-repro", "repro_hello.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		t.Skipf("repro_hello fixture missing: %v", err)
	}

	build := func() string {
		t.Helper()
		d := &Driver{CacheDir: t.TempDir(), NoCache: true}
		out := t.TempDir()
		p, err := d.Build(mochiPath, out, TargetPhpSource)
		if err != nil {
			t.Fatalf("Build: %v", err)
		}
		h, err := sha256File(p)
		if err != nil {
			t.Fatalf("sha256: %v", err)
		}
		return h
	}

	h1 := build()
	h2 := build()
	if h1 != h2 {
		t.Errorf("non-deterministic default-mode build:\n  build1: %s\n  build2: %s", h1, h2)
	}
}

func testPhpReproducibleBuild(t *testing.T, mochiPath string) {
	t.Helper()

	d1 := &Driver{CacheDir: t.TempDir(), NoCache: true, Deterministic: true}
	out1 := t.TempDir()
	p1, err := d1.Build(mochiPath, out1, TargetPhpSource)
	if err != nil {
		t.Fatalf("build 1: %v", err)
	}

	d2 := &Driver{CacheDir: t.TempDir(), NoCache: true, Deterministic: true}
	out2 := t.TempDir()
	p2, err := d2.Build(mochiPath, out2, TargetPhpSource)
	if err != nil {
		t.Fatalf("build 2: %v", err)
	}

	b1, err := os.ReadFile(p1)
	if err != nil {
		t.Fatalf("read build 1 output: %v", err)
	}
	b2, err := os.ReadFile(p2)
	if err != nil {
		t.Fatalf("read build 2 output: %v", err)
	}

	if !bytes.Equal(b1, b2) {
		h1 := sha256Bytes(b1)
		h2 := sha256Bytes(b2)
		t.Errorf("non-reproducible PHP source:\n  build1 SHA-256: %s\n  build2 SHA-256: %s\n--- build1 (%d bytes) ---\n%s\n--- build2 (%d bytes) ---\n%s",
			h1, h2, len(b1), b1, len(b2), b2)
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

func sha256Bytes(b []byte) string {
	h := sha256.Sum256(b)
	return fmt.Sprintf("%x", h[:])
}
