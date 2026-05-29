package build

import (
	"crypto/sha256"
	"encoding/hex"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase16Repro builds the same fixture twice with Deterministic=true and
// compares the resulting binaries SHA-256-byte-for-byte. On macOS, the
// Mach-O LC_UUID load command is randomised per link, so the gate is
// platform-skipped there (see also swift phase 16).
func TestPhase16Repro(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping reproducibility test in short mode")
	}
	if runtime.GOOS == "darwin" {
		t.Skip("reproducibility gate runs on linux only; macOS Mach-O LC_UUID is non-deterministic by default")
	}
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase16-repro")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		t.Run(name, func(t *testing.T) {
			d1 := &Driver{CacheDir: t.TempDir(), Deterministic: true, NoCache: true}
			d2 := &Driver{CacheDir: t.TempDir(), Deterministic: true, NoCache: true}
			out1 := t.TempDir()
			out2 := t.TempDir()
			bin1, err := d1.Build(mochiPath, out1, TargetNativeExecutable)
			if err != nil {
				t.Fatalf("build 1: %v", err)
			}
			bin2, err := d2.Build(mochiPath, out2, TargetNativeExecutable)
			if err != nil {
				t.Fatalf("build 2: %v", err)
			}
			h1, err := sha256Hex(bin1)
			if err != nil {
				t.Fatalf("sha256 bin1: %v", err)
			}
			h2, err := sha256Hex(bin2)
			if err != nil {
				t.Fatalf("sha256 bin2: %v", err)
			}
			if h1 != h2 {
				t.Errorf("non-reproducible build:\n  build1 SHA-256: %s\n  build2 SHA-256: %s", h1, h2)
			}
		})
	}
}

func sha256Hex(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}
