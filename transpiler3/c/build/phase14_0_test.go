package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"testing"
)

// TestPhase14LLM is the MEP-45 Phase 14.0 gate. It compiles every fixture
// under tests/transpiler3/c/fixtures/llm and runs each binary with
// MOCHI_LLM_CASSETTE_DIR pointing to the fixture's cassette/ subdirectory,
// asserting byte-equal output vs expect.txt.
//
// Cassette replay mode (no HTTP, no API keys required):
//   The C runtime reads MOCHI_LLM_CASSETTE_DIR and looks up a pre-recorded
//   response file named by the DJB2 hash of ("<provider>\0<model>\0<prompt>").
//   Cassette files are stored under <fixture>/cassette/<hash>.txt.
//
// Fixtures:
//   generate_text          -- generate openai { prompt: "..." } -> print
//   generate_with_model    -- explicit model field
//   generate_anthropic     -- anthropic provider
//   generate_google        -- google provider
//   generate_in_var        -- result stored in a variable, then concatenated
//   generate_concat        -- concatenate result inline
//   generate_multiple      -- two generate calls in one program
//   generate_in_fun        -- generate inside a user-defined function
//   generate_anthropic_model -- anthropic with explicit model
//   generate_openai_model  -- openai with gpt-4o
func TestPhase14LLM(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}
	runLLMFixtureSuite(t, "llm")
}

// runLLMFixtureSuite is like runFixtureSuite but sets MOCHI_LLM_CASSETTE_DIR
// to <fixture>/cassette/ before running each compiled binary. Each fixture
// subdirectory must contain a cassette/ directory with pre-recorded response
// files named by the DJB2 hash (see mochi/llm.h for the key format).
func runLLMFixtureSuite(t *testing.T, dir string) {
	t.Helper()
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", dir)
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir %s: %v", base, err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no fixtures under %s", base)
	}

	for _, name := range names {
		name := name
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			cassetteDir := filepath.Join(fixture, "cassette")
			if _, err := os.Stat(cassetteDir); err != nil {
				t.Fatalf("cassette dir missing for %s: %v", name, err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}

			cmd := exec.Command(outBin)
			cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v\nstdout: %q", name, err, stdout.String())
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}
