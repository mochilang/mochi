package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase10StreamsNode/Deno/Bun gate stream and channel lowering.
//
// Phase 10 lowering contract:
//
//   - `stream<T>(cap)` → `new MochiStream(cap)` (synchronous broadcast).
//   - `emit(s, v)` → `s.emit(v)`.
//   - `subscribe(s)` → `s.subscribe()`.
//   - `subscribe_limited(s, n)` → `s.subscribeLimited(n)`.
//   - `recv(sub)` → `sub.recv()`.
//   - `chan<T>(cap)` → `new MochiChan()` (synchronous FIFO).
//   - `send(ch, v)` → `ch.send(v)`.
//   - `recv(ch)` → `ch.recv()`.
//
// The synchronous model matches Go/Swift/Ruby backends. No async/await
// is involved in Phase 10.
//
// Minimum fixture count: 12.
func TestPhase10StreamsNode(t *testing.T) { runPhase10On(t, "node") }
func TestPhase10StreamsDeno(t *testing.T) { runPhase10On(t, "deno") }
func TestPhase10StreamsBun(t *testing.T)  { runPhase10On(t, "bun") }

func runPhase10On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase10-streams")
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
		t.Fatalf("Phase 10 fixture corpus has %d .mochi files, expected at least 12", mochiCount)
	}
}

// TestPhase10EmitShape verifies stream/chan runtime class inclusion.
func TestPhase10EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase10-streams")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "stream_basic.mochi",
			wants:   []string{"class MochiStream", "class MochiSub", "new MochiStream("},
		},
		{
			fixture: "chan_basic.mochi",
			wants:   []string{"class MochiChan", "new MochiChan("},
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
