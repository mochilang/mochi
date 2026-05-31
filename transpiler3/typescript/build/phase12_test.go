package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase12ErrorsNode/Deno/Bun gate error handling lowering.
//
// Phase 12 lowering contract:
//
//   - `try { ... } catch err { ... }` lowers to:
//     `try { ... } catch (__e: unknown) {
//         const err = __e instanceof Error ? __e.message : String(__e);
//         ... }`
//
//   - `panic(code, msg)` lowers to `throw new Error(msg)`.
//
//   - Top-level panic from `mochi_main()` is caught by a wrapping
//     try/catch that writes to stderr and exits with code 1 (via
//     the needsPanicHandler flag).
//
// Minimum fixture count: 10.
func TestPhase12ErrorsNode(t *testing.T) { runPhase12On(t, "node") }
func TestPhase12ErrorsDeno(t *testing.T) { runPhase12On(t, "deno") }
func TestPhase12ErrorsBun(t *testing.T)  { runPhase12On(t, "bun") }

func runPhase12On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase12-errors")
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
	if mochiCount < 10 {
		t.Fatalf("Phase 12 fixture corpus has %d .mochi files, expected at least 10", mochiCount)
	}
}

// TestPhase12EmitShape verifies try/catch and throw emit shape.
func TestPhase12EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase12-errors")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "try_catch_panic.mochi",
			wants:   []string{"try {", "catch (__e: unknown)", "throw new Error("},
		},
		{
			fixture: "try_basic.mochi",
			wants:   []string{"try {", "catch (__e: unknown)"},
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
