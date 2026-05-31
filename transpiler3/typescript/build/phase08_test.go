package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase8TransformsNode/Deno/Bun gate the string/list/math transform
// lowering on all three runtimes.
//
// Phase 8 lowering contract:
//
//   - map(xs, fn)    → xs.map(fn)
//   - filter(xs, fn) → xs.filter(fn)
//   - reduce(xs, fn, init) → xs.reduce(fn, init)
//   - split(s, sep)  → s.split(sep)
//   - join(xs, sep)  → xs.join(sep)
//   - upper(s)       → s.toUpperCase()
//   - lower(s)       → s.toLowerCase()
//   - reverse(s)     → mochi_str_reverse(s) (inline helper)
//   - int(f)         → Math.trunc(f) for float→int conversion
//   - abs / floor / ceil / sqrt → Math.abs / Math.floor / Math.ceil / Math.sqrt
//
// Minimum fixture count: 15.
func TestPhase8TransformsNode(t *testing.T) { runPhase8On(t, "node") }
func TestPhase8TransformsDeno(t *testing.T) { runPhase8On(t, "deno") }
func TestPhase8TransformsBun(t *testing.T)  { runPhase8On(t, "bun") }

func runPhase8On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase08-transforms")
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
	if mochiCount < 15 {
		t.Fatalf("Phase 8 fixture corpus has %d .mochi files, expected at least 15", mochiCount)
	}
}

// TestPhase8EmitShape verifies the lowering shape of key transforms.
func TestPhase8EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase08-transforms")
	cases := []struct {
		fixture  string
		wants    []string
		notWants []string
	}{
		{
			fixture:  "list_map.mochi",
			wants:    []string{".map("},
			notWants: []string{"mochi_list_map"},
		},
		{
			fixture:  "str_upper_lower.mochi",
			wants:    []string{".toUpperCase()", ".toLowerCase()"},
			notWants: []string{"mochi_str_upper"},
		},
		{
			fixture: "str_reverse.mochi",
			wants:   []string{"mochi_str_reverse"},
		},
		{
			fixture: "math_floor_ceil.mochi",
			wants:   []string{"Math.floor", "Math.ceil"},
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
