package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase18ReproNode/Deno/Bun gate reproducible build output on all runtimes.
//
// Phase 18 lowering contract:
//
//   - Two independent builds of the same source produce byte-identical .ts output.
//
//   - All helper emission order is deterministic (sorted, not map-iteration order).
//
//   - The emitted file hash is stable across invocations (no timestamp, random, or
//     build-ID injection).
//
// Minimum fixture count: 7.
func TestPhase18ReproNode(t *testing.T) { runPhase18On(t, "node") }
func TestPhase18ReproDeno(t *testing.T) { runPhase18On(t, "deno") }
func TestPhase18ReproBun(t *testing.T)  { runPhase18On(t, "bun") }

func runPhase18On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase18-repro")
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
	if mochiCount < 7 {
		t.Fatalf("Phase 18 fixture corpus has %d .mochi files, expected at least 7", mochiCount)
	}
}

// TestPhase18ByteIdentical verifies that two independent builds of the same
// source file produce byte-identical .ts output.
func TestPhase18ByteIdentical(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase18-repro")
	fixtures := []string{
		"repro_hello.mochi",
		"repro_function.mochi",
		"repro_list.mochi",
		"repro_math.mochi",
		"repro_record.mochi",
	}
	for _, fix := range fixtures {
		fix := fix
		t.Run(strings.TrimSuffix(fix, ".mochi"), func(t *testing.T) {
			src := filepath.Join(fixtureDir, fix)

			out1 := t.TempDir()
			d1 := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p1, err := d1.Build(src, out1, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build1(%s): %v", fix, err)
			}

			out2 := t.TempDir()
			d2 := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p2, err := d2.Build(src, out2, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build2(%s): %v", fix, err)
			}

			b1, err := os.ReadFile(p1)
			if err != nil {
				t.Fatalf("read build1: %v", err)
			}
			b2, err := os.ReadFile(p2)
			if err != nil {
				t.Fatalf("read build2: %v", err)
			}
			if string(b1) != string(b2) {
				t.Errorf("non-deterministic output for %s\nbuild1:\n%s\nbuild2:\n%s", fix, b1, b2)
			}
		})
	}
}

// TestPhase18EmitShape verifies that repro fixtures compile without error
// and produce non-empty output.
func TestPhase18EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase18-repro")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		e := e
		t.Run(strings.TrimSuffix(e.Name(), ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build %s: %v", e.Name(), err)
			}
			src := readTrim(t, p)
			if len(src) == 0 {
				t.Errorf("%s produced empty output", e.Name())
			}
		})
	}
}
