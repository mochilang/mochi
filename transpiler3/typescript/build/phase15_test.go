package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase15HTTPNode/Deno/Bun gate HTTP fetch lowering.
//
// Phase 15 lowering contract:
//
//   - `http_get(url)` → `await mochi_http_get(url)` (async, propagates colour).
//
//   - `mochi_http_get` supports the `file://` scheme using fs read, which
//     allows deterministic fixture tests without a network.
//
//   - Deno requires `--allow-read --allow-net` flags.
//
// All fixtures use `file://` URLs pointing to temp files so tests are
// network-free and deterministic.
//
// Minimum fixture count: 10.
func TestPhase15HTTPNode(t *testing.T) { runPhase15On(t, "node") }
func TestPhase15HTTPDeno(t *testing.T) { runPhase15On(t, "deno") }
func TestPhase15HTTPBun(t *testing.T)  { runPhase15On(t, "bun") }

func runPhase15On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase15-http")
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
		t.Run(name, func(t *testing.T) {
			runHTTPFixture(t, runtime,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
	if mochiCount < 10 {
		t.Fatalf("Phase 15 fixture corpus has %d .mochi files, expected at least 10", mochiCount)
	}
}

// runHTTPFixture adds --allow-read --allow-net for Deno.
func runHTTPFixture(t *testing.T, runtime, mochiPath, wantFile string) {
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
		args = []string{"run", "--allow-read", "--allow-net", emittedPath}
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

// TestPhase15EmitShape verifies that HTTP fixtures include mochi_http_get.
func TestPhase15EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase15-http")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "http_file_scheme.mochi",
			wants:   []string{"mochi_http_get", "async function", "__mochi_runtime"},
		},
		{
			fixture: "http_with_transform.mochi",
			wants:   []string{"mochi_http_get", "async function"},
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
