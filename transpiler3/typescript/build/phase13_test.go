package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase13FileIONode/Deno/Bun gate file I/O lowering.
//
// Phase 13 lowering contract:
//
//   - `read_file(path)` → `mochi_read_file(path)` runtime helper.
//   - `write_file(path, content)` → `mochi_write_file(path, content)`.
//   - `append_file(path, content)` → `mochi_append_file(path, content)`.
//   - `lines(path)` → `mochi_lines(path)` (splits on "\n").
//
//   - A `__mochi_runtime` detector const is emitted before any file I/O
//     helper to select between Deno, Bun, and Node APIs at runtime.
//
// Tests use `/tmp/mochi_ts_test_*.txt` scratch files.
//
// Minimum fixture count: 10.
func TestPhase13FileIONode(t *testing.T) { runPhase13On(t, "node") }
func TestPhase13FileIODeno(t *testing.T) { runPhase13On(t, "deno") }
func TestPhase13FileIOBun(t *testing.T)  { runPhase13On(t, "bun") }

func runPhase13On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase13-fileio")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	mochiCount := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		mochiCount++
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantFile := filepath.Join(fixtureDir, name+".out")
		t.Run(name, func(t *testing.T) {
			runFileIOFixture(t, runtime, mochiPath, wantFile)
		})
	}
	if mochiCount < 10 {
		t.Fatalf("Phase 13 fixture corpus has %d .mochi files, expected at least 10", mochiCount)
	}
}

// runFileIOFixture is like runTsFixture but adds --allow-read --allow-write
// for Deno which requires explicit fs permission flags.
func runFileIOFixture(t *testing.T, runtime, mochiPath, wantFile string) {
	t.Helper()
	bin, ok := resolveRuntime(runtime)
	if !ok {
		t.Skipf("%s not on PATH", runtime)
	}
	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want %s: %v", wantFile, err)
	}
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	emittedPath, err := d.Build(mochiPath, outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}
	var args []string
	switch runtime {
	case "deno":
		args = []string{"run", "--allow-read", "--allow-write", emittedPath}
	default:
		args = []string{emittedPath}
	}
	cmd := exec.Command(bin, args...)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("%s %s: %v", runtime, emittedPath, err)
	}
	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

// TestPhase13EmitShape verifies that file I/O fixtures include the runtime
// detection block and the mochi_read_file / mochi_write_file helpers.
func TestPhase13EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase13-fileio")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "write_read.mochi",
			wants:   []string{"__mochi_runtime", "mochi_read_file", "mochi_write_file"},
		},
		{
			fixture: "lines_basic.mochi",
			wants:   []string{"mochi_lines", "mochi_read_file"},
		},
	}
	for _, tc := range cases {
		t.Run(strings.TrimSuffix(tc.fixture, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, tc.fixture), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build %s: %v", tc.fixture, err)
			}
			src := readTrim(t, p)
			for _, want := range tc.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s missing %q\n---\n%s", tc.fixture, want, src)
				}
			}
		})
	}
}
