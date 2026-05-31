package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase11AsyncNode/Deno/Bun gate async/await lowering with the full
// call-graph colour pass.
//
// Phase 11 lowering contract:
//
//   - The colour pass replaces the Phase 1-10 all-Blue stub with a
//     real call-graph fixed-point: any function containing AsyncExpr or
//     AwaitExpr is Red; any function calling a Red function is also Red
//     (transitively to fixpoint).
//
//   - Red functions get the `async` modifier and their return type is
//     wrapped in `Promise<T>`.
//
//   - `async BODY` lowers to `(async (): Promise<T> => { return BODY; })()`.
//
//   - `await FUTURE` lowers to `await FUTURE`.
//
//   - The runtime builtins `mochi_http_get` and `mochi_llm_generate` are
//     seeded Red; any function calling them propagates Red transitively.
//
// Minimum fixture count: 12.
func TestPhase11AsyncNode(t *testing.T) { runPhase11On(t, "node") }
func TestPhase11AsyncDeno(t *testing.T) { runPhase11On(t, "deno") }
func TestPhase11AsyncBun(t *testing.T)  { runPhase11On(t, "bun") }

func runPhase11On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase11-async")
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
			runTsFixture(t, runtime,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
	if mochiCount < 12 {
		t.Fatalf("Phase 11 fixture corpus has %d .mochi files, expected at least 12", mochiCount)
	}
}

// TestPhase11EmitShape verifies async function header and await emission.
func TestPhase11EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase11-async")
	cases := []struct {
		fixture  string
		wants    []string
		notWants []string
	}{
		{
			fixture: "async_simple.mochi",
			wants:   []string{"async function", "await ", "Promise<"},
			notWants: []string{"function make_async():  {"},
		},
		{
			fixture: "async_chain.mochi",
			wants:   []string{"async function", "await "},
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
			for _, notWant := range tc.notWants {
				if strings.Contains(src, notWant) {
					t.Errorf("%s has forbidden %q", tc.fixture, notWant)
				}
			}
		})
	}
}
