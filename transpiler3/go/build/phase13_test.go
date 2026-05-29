package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"testing"
)

// TestPhase13LLM is the MEP-54 Phase 13.0 gate for `generate`
// expressions. The Go lowerer routes LLMGenerateExpr through
// mochiLLMGenerate, which reads a recorded reply from
// $MOCHI_LLM_CASSETTE_DIR / <djb2(provider\0model\0prompt)>.txt
// so CI never makes a live API call.
//
// Each fixture lives flat under tests/transpiler3/go/fixtures
// next to a sibling `cassette/` directory holding the recorded
// reply files. Phase 1's walker already skips any fixture that
// has a cassette/ subdir; this test re-discovers and runs them
// with MOCHI_LLM_CASSETTE_DIR pointed at that subdir.
func TestPhase13LLM(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "go", "fixtures")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir: %v", err)
	}

	var names []string
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		if _, err := os.Stat(filepath.Join(base, e.Name(), "cassette")); err != nil {
			continue
		}
		names = append(names, e.Name())
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no LLM fixtures found under %s", base)
	}

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build: %v", err)
			}

			cmd := exec.Command(outBin)
			cmd.Env = append(os.Environ(),
				"MOCHI_LLM_CASSETTE_DIR="+filepath.Join(fixture, "cassette"),
			)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v", outBin, err)
			}
			if got := stdout.String(); got != string(want) {
				t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
			}
		})
	}
}

// TestPhase13LLMSource gates the lowering shape for a single
// fixture (generate_text) without executing the binary. It
// verifies the call goes through mochiLLMGenerate with the
// quoted provider, the helper is present, the DJB2 hasher is
// emitted, and the cassette-replay path is wired through
// MOCHI_LLM_CASSETTE_DIR + io.ReadFile.
func TestPhase13LLMSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "generate_text", "generate_text.mochi")
	outBin := filepath.Join(t.TempDir(), "generate_text")
	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	mainPath := filepath.Join(d.WorkDirPath, "main.go")
	srcBytes, err := os.ReadFile(mainPath)
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	got := string(srcBytes)

	for _, marker := range []string{
		`mochiLLMGenerate("openai"`,
		"func mochiLLMGenerate(provider, model, prompt string) string",
		"func mochiDJB2Key(provider, model, prompt string) uint64",
		`os.Getenv("MOCHI_LLM_CASSETTE_DIR")`,
		"mochiDJB2Key(provider, model, prompt)",
		`os.ReadFile(path)`,
		`strings.TrimRight(string(b), "\n")`,
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}
