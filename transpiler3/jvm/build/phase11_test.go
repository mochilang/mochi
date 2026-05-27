package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase11Async is the gate test for MEP-47 Phase 11: async/await with Loom virtual threads.
// It runs every .mochi fixture under tests/transpiler3/jvm/phase11-async/ and
// compares stdout to the paired .out file.
func TestPhase11Async(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForTest(t), "tests", "transpiler3", "jvm", "phase11-async")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")

		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Errorf("missing .out for %s: %v", name, err)
			continue
		}

		t.Run(name, func(t *testing.T) {
			outJar := filepath.Join(t.TempDir(), name+".jar")
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
				t.Fatalf("Build: %v", err)
			}

			tc, _ := resolveToolchain()
			cmd := exec.Command(tc.Java, "-jar", outJar)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("java -jar: %v", err)
			}

			got := stdout.String()
			if got != string(want) {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, string(want))
			}
		})
	}
}
