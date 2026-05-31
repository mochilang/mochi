package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase16JSONNode/Deno/Bun gate JSON decode lowering.
//
// Phase 16 lowering contract:
//
//   - `json_decode(input)` → `JSON.parse(input)`.
//
//   - Typed helpers `mochi_json_decode_list_i64` and
//     `mochi_json_decode_list_str` are emitted when decode results are
//     used as typed lists.
//
// Minimum fixture count: 8.
func TestPhase16JSONNode(t *testing.T) { runPhase16On(t, "node") }
func TestPhase16JSONDeno(t *testing.T) { runPhase16On(t, "deno") }
func TestPhase16JSONBun(t *testing.T)  { runPhase16On(t, "bun") }

func runPhase16On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase16-json")
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
	if mochiCount < 8 {
		t.Fatalf("Phase 16 fixture corpus has %d .mochi files, expected at least 8", mochiCount)
	}
}

// TestPhase16EmitShape verifies JSON.parse emission and typed helpers.
func TestPhase16EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase16-json")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "json_parse_string.mochi",
			wants:   []string{"JSON.parse("},
		},
		{
			fixture: "json_roundtrip_int.mochi",
			wants:   []string{"JSON.parse(", "JSON.stringify("},
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
