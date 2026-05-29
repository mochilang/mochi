package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase11Errors is the Phase 11 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase11-errors must
// lower via the TypeScript transpiler and produce byte-equal stdout
// on Node 22, Deno 2, and Bun 1.1 against the recorded .out.
//
// Phase 11 lands Mochi's panic / try-catch sub-language on
// TypeScript by emitting a `MochiPanic extends Error` class
// carrying an integer code plus throwing div / mod / index helpers
// that raise it on the documented trap conditions. The user-visible
// `try { ... } catch e { ... }` lowers to JS `try { ... } catch
// (__panic_N) { const e: number = (__panic_N instanceof MochiPanic
// ? __panic_N.code : 0); ... }` so e is always a number.
//
// The MEP-52 §Phase 11 spec proposed an async-coloured runtime
// (`MochiResult<T, E>`, Promise rejection plumbing, AggregateError
// fan-in). The audit found every fixture in the 36-fixture corpus
// is a synchronous use of panic + try / catch: the catch variable
// is an integer code, no fixture awaits anything, no fixture
// aggregates multiple errors. JavaScript's native `throw` / `try`
// / `catch` covers the full surface at zero runtime cost.
//
// Phase 11 ships 36 fixtures (the full Rust Phase 11 corpus); the
// floor is 30 per MEP-52 §Phase 11.
func TestPhase11ErrorsNode(t *testing.T) { runPhase11FixturesOn(t, "node") }
func TestPhase11ErrorsDeno(t *testing.T) { runPhase11FixturesOn(t, "deno") }
func TestPhase11ErrorsBun(t *testing.T)  { runPhase11FixturesOn(t, "bun") }

func runPhase11FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase11-errors")
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
	if mochiCount < 30 {
		t.Fatalf("Phase 11 fixture corpus has %d .mochi files, expected at least 30", mochiCount)
	}
}

// TestPhase11EmitShape asserts the load-bearing tokens of the
// panic / try-catch lowering are present in the emit. The
// invariants encode the chosen strategy (native JS throw + class
// narrowing) and rule out two alternatives that would produce the
// same stdout but break later phases:
//
//   - MochiResult<T, E> sum-typed return values everywhere. Would
//     surface as `MochiResult.ok(...)`, `MochiResult.err(...)`, and
//     pattern matching on every call site. Inflates emit several-
//     fold for zero corpus-level behaviour benefit and forces the
//     async colour (Phase 11 spec's planned coupling to Phase 11
//     proper) onto every function.
//
//   - Bare integer return + global error register. Would surface
//     as `mochi_last_error = N; return 0;` after every panic site,
//     with the catch reading the register. That pattern matches
//     C's setjmp/longjmp emit but in TS forfeits stack-unwind for
//     free; the JS engine already has the unwind in native `throw`.
func TestPhase11EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase11-errors")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "user_panic_basic.mochi",
			wants: []string{
				"class MochiPanic extends Error {",
				"throw new MochiPanic(42, \"boom\");",
				"} catch (__mochi_err_0) {",
				"const e: number = (__mochi_err_0 instanceof MochiPanic ? __mochi_err_0.code : 0);",
			},
		},
		{
			fixture: "try_catch_div_zero.mochi",
			wants: []string{
				"function mochi_div_i64(",
				"throw new MochiPanic(5,",
				"const z: number = mochi_div_i64(x, y);",
			},
		},
		{
			fixture: "try_catch_mod_zero.mochi",
			wants: []string{
				"function mochi_mod_i64(",
				"mochi_mod_i64(",
			},
		},
		{
			fixture: "try_catch_index_oob.mochi",
			wants: []string{
				"function mochi_list_at",
				"throw new MochiPanic(4,",
				"mochi_list_at(",
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

// TestPhase11NoAsyncRuntime asserts that no async / Result runtime
// tokens leak into the TS emit. The synchronous-throw path must
// fully consume the panic / try-catch surface; nothing of the
// Phase 11 spec's originally planned `MochiResult<T, E>` +
// AggregateError + async-colour runtime should appear.
//
// If this test fails the most likely cause is that a spec-driven
// future change re-introduced the async runtime; the audit (which
// dropped the engine in favour of native throw + MochiPanic class)
// should be re-read first.
func TestPhase11NoAsyncRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase11-errors")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"MochiResult",
		"AggregateError",
		"@mochi/runtime/result",
		"AbortController",
		" await ",
		"async function",
		"Promise.",
		"Promise<",
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
