package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"mochi/transpiler3/dotnet/lower"
)

// TestPhase15NativeAot is the gate test for MEP-48 Phase 15: NativeAOT packaging.
// Skipped if MOCHI_TEST_AOT is not set, because NativeAOT compilation takes 30+ seconds per fixture.
// Set MOCHI_TEST_AOT=1 to run.
func TestPhase15NativeAot(t *testing.T) {
	if os.Getenv("MOCHI_TEST_AOT") == "" {
		t.Skip("set MOCHI_TEST_AOT=1 to run NativeAOT tests (slow, ~1 min per fixture)")
	}

	fixtureBase := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "dotnet", "fixtures", "phase15-nativeaot")
	entries, err := os.ReadDir(fixtureBase)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureBase, err)
	}

	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		fixtureDir := filepath.Join(fixtureBase, name)
		mochiPath := filepath.Join(fixtureDir, name+".mochi")
		wantPath := filepath.Join(fixtureDir, name+".out")

		if _, err := os.Stat(mochiPath); err != nil {
			continue
		}
		want, err := os.ReadFile(wantPath)
		if err != nil {
			t.Errorf("missing .out for %s: %v", name, err)
			continue
		}

		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(mochiPath, outDir, TargetAot); err != nil {
				t.Fatalf("Build(%s) AOT: %v", name, err)
			}

			className := lower.ClassName(mochiPath)
			binaryPath := filepath.Join(outDir, className)
			if runtime.GOOS == "windows" {
				binaryPath += ".exe"
			}

			if _, err := os.Stat(binaryPath); err != nil {
				t.Fatalf("expected NativeAOT binary %s to exist: %v", binaryPath, err)
			}

			cmd := exec.Command(binaryPath)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v", binaryPath, err)
			}

			got := strings.TrimRight(stdout.String(), "\r\n")
			wantStr := strings.TrimRight(string(want), "\r\n")
			if got != wantStr {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, wantStr)
			}
		})
	}
}
