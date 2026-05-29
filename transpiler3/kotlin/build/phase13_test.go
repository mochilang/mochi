package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// runLLMFixtureDir runs all LLM fixtures in a directory.
// Each fixture is a subdirectory with <name>.mochi, <name>.out, and cassette/.
// The cassette directory is set via MOCHI_LLM_CASSETTE when running java.
func runLLMFixtureDir(t *testing.T, fixtureDir string) {
	t.Helper()
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		subDir := filepath.Join(fixtureDir, name)
		mochiFile := filepath.Join(subDir, name+".mochi")
		outFile := filepath.Join(subDir, name+".out")
		cassetteDir := filepath.Join(subDir, "cassette")
		if _, err := os.Stat(mochiFile); err != nil {
			continue // not a fixture dir
		}
		e := e
		t.Run(e.Name(), func(t *testing.T) {
			t.Parallel()
			want, err := os.ReadFile(outFile)
			if err != nil {
				t.Fatalf("read want: %v", err)
			}
			outDir := t.TempDir()
			jarPath := cachedBuild(t, mochiFile, outDir)
			javaPath := resolveJava(t)
			cmd := exec.Command(javaPath, "-jar", jarPath)
			if _, err := os.Stat(cassetteDir); err == nil {
				cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE="+cassetteDir)
			}
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v\nstderr: %s", jarPath, err, stderr.String())
			}
			got := stdout.Bytes()
			if !bytes.Equal(got, want) {
				d := &Driver{}
				ktSrc, _ := d.GenerateKt(mochiFile)
				t.Errorf("stdout mismatch for %s\ngot:  %q\nwant: %q\n\nGenerated Kotlin:\n%s",
					name, strings.TrimRight(string(got), "\n"), strings.TrimRight(string(want), "\n"), ktSrc)
			}
		})
	}
}

func TestPhase13LLM(t *testing.T) {
	runLLMFixtureDir(t, filepath.Join(repoRoot(t), "tests", "transpiler3", "kotlin", "fixtures", "phase13-llm"))
}
