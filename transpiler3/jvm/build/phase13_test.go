package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase13LLM is the gate test for MEP-47 Phase 13: generate expressions with cassette replay.
// Each fixture lives in its own subdirectory under tests/transpiler3/jvm/phase13-llm/.
// If a cassette/ subdirectory exists next to the .mochi file it is passed via
// MOCHI_LLM_CASSETTE_DIR so AI.call replays recorded responses without live API access.
func TestPhase13LLM(t *testing.T) {
	root := repoRootForTest(t)
	fixtureBase := filepath.Join(root, "tests", "transpiler3", "jvm", "phase13-llm")
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
			outJar := filepath.Join(t.TempDir(), name+".jar")
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(mochiPath, outJar, TargetUberJar); err != nil {
				t.Fatalf("Build: %v", err)
			}

			tc, _ := resolveToolchain()
			cmd := exec.Command(tc.Java, "-jar", outJar)

			cassetteDir := filepath.Join(fixtureDir, "cassette")
			if _, err := os.Stat(cassetteDir); err == nil {
				cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
			}

			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("java -jar: %v", err)
			}

			got := strings.TrimRight(stdout.String(), "\n")
			wantStr := strings.TrimRight(string(want), "\n")
			if got != wantStr {
				t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, wantStr)
			}
		})
	}
}
