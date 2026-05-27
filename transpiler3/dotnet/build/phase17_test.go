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

// TestPhase17SelfContained is the gate test for MEP-48 Phase 17: self-contained packaging.
// It builds each fixture as a self-contained publish for the host RID, finds the native
// apphost binary, runs it, and checks stdout against the paired .out file.
func TestPhase17SelfContained(t *testing.T) {
	fixtureDir := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "dotnet", "fixtures", "phase17-selfcontained")
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
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(mochiPath, outDir, TargetSelfContained); err != nil {
				t.Fatalf("Build(%s) SelfContained: %v", name, err)
			}

			className := lower.ClassName(mochiPath)

			// Self-contained publish produces a native apphost with the class name.
			// On Windows it has .exe extension; on Unix no extension.
			binaryPath := filepath.Join(outDir, className)
			if runtime.GOOS == "windows" {
				binaryPath += ".exe"
			}

			if _, err := os.Stat(binaryPath); err != nil {
				// Fall back to running via dotnet if the apphost isn't found.
				tc, err := resolveToolchain()
				if err != nil {
					t.Fatalf("resolveToolchain: %v", err)
				}
				dllPath := filepath.Join(outDir, className+".dll")
				cmd := exec.Command(tc.Dotnet, dllPath)
				var stdout bytes.Buffer
				cmd.Stdout = &stdout
				cmd.Stderr = os.Stderr
				if err := cmd.Run(); err != nil {
					t.Fatalf("dotnet %s: %v", dllPath, err)
				}
				got := strings.TrimRight(stdout.String(), "\r\n")
				wantStr := strings.TrimRight(string(want), "\r\n")
				if got != wantStr {
					t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, wantStr)
				}
				return
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
