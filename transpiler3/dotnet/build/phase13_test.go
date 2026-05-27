package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/dotnet/lower"
)

// TestPhase13LLM is the gate test for MEP-48 Phase 13: generate expressions with cassette replay.
// Each fixture lives in its own subdirectory. If a cassette/ subdirectory exists
// it is passed via MOCHI_LLM_CASSETTE_DIR so Ai.Call replays recorded responses.
func TestPhase13LLM(t *testing.T) {
	fixtureBase := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "dotnet", "fixtures", "phase13-llm")
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
			if err := d.Build(mochiPath, outDir, TargetFxDependent); err != nil {
				t.Fatalf("Build(%s): %v", name, err)
			}

			tc, err := resolveToolchain()
			if err != nil {
				t.Fatalf("resolveToolchain: %v", err)
			}

			className := lower.ClassName(mochiPath)
			dllPath := filepath.Join(outDir, className+".dll")
			cmd := exec.Command(tc.Dotnet, dllPath)

			cassetteDir := filepath.Join(fixtureDir, "cassette")
			if _, err := os.Stat(cassetteDir); err == nil {
				cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
			}

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
		})
	}
}
