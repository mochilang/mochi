package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase12FFI is the Phase 12 primary gate. Every fixture under
// tests/transpiler3/typescript/fixtures/phase12-ffi must lower via
// the TypeScript transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the recorded .out.
//
// Phase 12 lands Mochi's `extern fun` surface on TypeScript by
// inline-translating the sidecar `<fixture>.c` file into TS at
// compile time. The cffi package walks a constrained C subset (the
// one the corpus uses: int64_t / uint64_t / double / bool scalars,
// arithmetic / comparison / bitwise / logical operators, ternary,
// if-else, for, while, return, compound assignments, casts) and
// emits one TS `function NAME(...): RET { ... }` per C function.
// The build driver injects these as RawDecl entries at the head of
// the SourceFile so user code can call them via the same identifier
// the .mochi `extern fun` declares.
//
// The MEP-52 §Phase 12 spec proposed three FFI backends (Node
// N-API, Deno FFI, Bun FFI) plus a per-runtime shared-library
// build. The audit found every fixture in the 24-fixture corpus is
// a pure computation (no syscalls, no mutable state, no memory
// ownership), so inline translation collapses the multi-runtime
// dispatch problem to a no-op: every tier-1 runtime executes the
// same TS function, byte-equal stdout drops out of the existing
// emit pipeline. The N-API / Deno-FFI / Bun-FFI backends remain
// available as future 12.1 / 12.2 / 12.3 sub-phases the day a
// fixture introduces a side-effectful extern.
//
// Phase 12 ships 24 fixtures (the full Rust Phase 12 corpus); the
// floor is 20 per MEP-52 §Phase 12.
func TestPhase12FFINode(t *testing.T) { runPhase12FixturesOn(t, "node") }
func TestPhase12FFIDeno(t *testing.T) { runPhase12FixturesOn(t, "deno") }
func TestPhase12FFIBun(t *testing.T)  { runPhase12FixturesOn(t, "bun") }

func runPhase12FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase12-ffi")
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
	if mochiCount < 20 {
		t.Fatalf("Phase 12 fixture corpus has %d .mochi files, expected at least 20", mochiCount)
	}
}

// TestPhase12EmitShape asserts the load-bearing tokens of the
// inline-translation lowering are present in the emit. The
// invariants encode the chosen strategy (compile-time C-to-TS
// translation via cffi) and rule out two alternatives that would
// produce the same stdout but break later phases:
//
//   - Runtime FFI dispatch via Node N-API + Deno FFI + Bun FFI.
//     Would surface as `Deno.dlopen(...)`, `require('node-api-headers')`,
//     `import { dlopen } from "bun:ffi"`, and a build step that
//     compiles each sidecar .c to a .dylib / .so per platform.
//     The 24-fixture corpus contains zero side-effectful externs;
//     the engineering cost is wasted.
//
//   - WebAssembly compile of the sidecar .c. Would surface as
//     `WebAssembly.instantiate(...)` and a wasm build step. Same
//     cost-benefit problem as runtime FFI; also rules out browser
//     parity because Node/Deno/Bun wasm loaders differ.
func TestPhase12EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase12-ffi")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "add_extern.mochi",
			wants: []string{
				"Phase 12: inline-translated from sidecar add_extern.c.",
				"function c_add(a: number, b: number): number",
				"c_add(",
			},
		},
		{
			fixture: "is_even_extern.mochi",
			wants: []string{
				"function c_is_even(x: number): boolean",
			},
		},
		{
			fixture: "two_externs.mochi",
			wants: []string{
				"function c_add(a: number, b: number): number",
				"function c_mul(a: number, b: number): number",
			},
		},
		{
			fixture: "extern_returns_zero.mochi",
			wants: []string{
				"function c_zero(): number",
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

// TestPhase12NoNativeFFI asserts that no native-FFI runtime tokens
// leak into the TS emit. The compile-time-inline path must fully
// consume the extern surface; nothing of the Phase 12 spec's
// originally planned N-API / Deno-FFI / Bun-FFI dispatch should
// appear.
//
// If this test fails the most likely cause is that a spec-driven
// future change re-introduced one of the runtime FFI backends; the
// audit (which dropped runtime dispatch in favour of compile-time
// inline translation) should be re-read first.
func TestPhase12NoNativeFFI(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase12-ffi")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"Deno.dlopen",
		"node-api-headers",
		"bun:ffi",
		"WebAssembly.instantiate",
		"WebAssembly.compile",
		"dlopen(",
		"require(\"ffi-",
		"import \"ffi-",
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
					t.Errorf("%s emit leaked native-FFI token %q\n---\n%s", e.Name(), f, src)
				}
			}
		})
	}
}
