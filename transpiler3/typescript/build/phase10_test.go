package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase10Streams is the Phase 10 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase10-streams must
// lower via the TypeScript transpiler and produce byte-equal stdout
// on Node 22, Deno 2, and Bun 1.1 against the recorded .out.
//
// Phase 10 lands the Mochi chan<T> + stream<T> sub-languages on
// TypeScript by emitting three inline runtime classes (MochiChan,
// MochiStream, MochiSub) plus per-call-site lowering of make_chan
// / send / recv / make_stream / subscribe / emit / recv_sub.
//
// The MEP-52 §Phase 10 spec budget (~10 KB gzipped for an
// AsyncIterableQueue + AbortController + AggregateError runtime
// under `@mochi/runtime/stream`) is deferred for the same reason
// Phase 9 deferred its async-agent engine: every fixture in the
// Phase 10 corpus is a single-threaded synchronous use of the
// surface (producer runs to completion before consumer; buffer is
// always sized to hold every item that will ever be sent). The
// async runtime would force the async colour onto every operation
// (Phase 11) for zero behaviour benefit on the corpus. The Rust
// transpiler keeps a parking-lot runtime because Rust agents may
// move across OS threads; the TS path runs single-threaded by
// construction (one event loop per Node / Deno / Bun process). If
// a future fixture introduces fiber-style spawn that genuinely
// blocks on a full/empty buffer the runtime engine can be added
// without disturbing the synchronous path for closed programs.
//
// Phase 10 ships 31 fixtures (the full Rust Phase 10 corpus); the
// floor is 25 per MEP-52 §Phase 10.
func TestPhase10StreamsNode(t *testing.T) { runPhase10FixturesOn(t, "node") }
func TestPhase10StreamsDeno(t *testing.T) { runPhase10FixturesOn(t, "deno") }
func TestPhase10StreamsBun(t *testing.T)  { runPhase10FixturesOn(t, "bun") }

func runPhase10FixturesOn(t *testing.T, runtime string) {
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
	if mochiCount < 25 {
		t.Fatalf("Phase 10 fixture corpus has %d .mochi files, expected at least 25", mochiCount)
	}
}

// TestPhase10EmitShape asserts the load-bearing tokens of the
// chan/stream lowering are present in the emit. The invariants
// encode the chosen strategy (inline synchronous classes + plain
// method dispatch) and rule out two alternatives that would
// produce the same stdout but break later phases:
//
//   - Promise-coloured async runtime. Would surface as imports of
//     `@mochi/runtime/stream`, `AsyncIterableQueue`,
//     `AbortController`, or `await ch.recv()`. Forces the async
//     colour onto every recv (Phase 11), inflates packages by
//     10+ KB (Phase 15 budget), and breaks byte-equal repro under
//     runtime version skew.
//
//   - Bare array lowering with free `mochi_chan_send(...)` helpers.
//     Would surface as `mochi_chan_send(ch, v)`. That pattern works
//     for the C target (no methods, no `this`) but in TS forces the
//     wrong call shape and prevents typed generics from flowing.
func TestPhase10EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase10-streams")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "chan_basic.mochi",
			wants: []string{
				"class MochiChan<T> {",
				"const ch: MochiChan<number> = MochiChan.make<number>(1);",
				"ch.send(42);",
				"const x: number = ch.recv();",
			},
		},
		{
			fixture: "chan_string.mochi",
			wants: []string{
				"const ch: MochiChan<string> = MochiChan.make<string>(2);",
				"ch.send(\"hello\");",
				"ch.send(\"world\");",
			},
		},
		{
			fixture: "stream_basic.mochi",
			wants: []string{
				"class MochiStream<T> {",
				"class MochiSub<T> {",
				"const s: MochiStream<number> = MochiStream.make<number>(4);",
				"const sub: MochiSub<number> = s.subscribe();",
				"s.emit(10);",
				"const a: number = sub.recv();",
			},
		},
		{
			fixture: "stream_multi_sub.mochi",
			wants: []string{
				"const sub1: MochiSub<number> = s.subscribe();",
				"const sub2: MochiSub<number> = s.subscribe();",
			},
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
					t.Errorf("%s emit missing %q\n---\n%s", tc.fixture, want, src)
				}
			}
		})
	}
}

// TestPhase10NoAsyncRuntime asserts that no Promise/async runtime
// tokens leak into the TS emit. The synchronous-class path must
// fully consume the chan/stream surface; nothing of the Phase 10
// spec's planned AsyncIterableQueue + AbortController +
// AggregateError runtime should appear.
//
// If this test fails the most likely cause is that a spec-driven
// future change re-introduced the async runtime; the audit (which
// dropped the engine in favour of inline synchronous classes)
// should be re-read first.
func TestPhase10NoAsyncRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase10-streams")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"AsyncIterableQueue",
		"AbortController",
		"@mochi/runtime/stream",
		"mochi_chan_",
		"mochi_stream_",
		"AggregateError",
		" await ",
		"async ",
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		t.Run(strings.TrimSuffix(e.Name(), ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			for _, f := range forbidden {
				if strings.Contains(src, f) {
					t.Errorf("%s emit leaked async-runtime token %q\n---\n%s", e.Name(), f, src)
				}
			}
		})
	}
}
