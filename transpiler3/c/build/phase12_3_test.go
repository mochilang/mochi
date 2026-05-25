package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"testing"
)

// TestPhase12WasmCorpus is the MEP-45 Phase 12.3 gate. It compiles the full
// Phase 1-10 fixture corpus for wasm32-wasi using zig cc and runs each binary
// under wasmtime, asserting byte-equal output vs expect.txt.
//
// Gated on MOCHI_TEST_ZIG_DOWNLOAD=1 (zig download) and silently skipped if
// wasmtime is not on PATH (CI installs wasmtime; dev hosts may skip the run).
//
// Excluded suites:
//   - file_io: WASI file access requires preopened directories via
//     `wasmtime run --dir=.`; deferred until Phase 12.3+ wires --dir flags.
//   - csv_adapters: uses fopen/fgets; same blocker as file_io.
//   - ffi: neighbour .c compiled without wasm target; cross-TU boundary.
//   - divzero-trip: intentionally exits non-zero.
//   - hello: flat fixture (no subdirectory per fixture).
//   - arena_query, query, query_join: arena allocator returns byte-granular
//     pointers that may not satisfy 8-byte alignment for int64_t/double on
//     wasm32-wasi; wasmtime traps on misaligned stores. Pre-existing issue;
//     deferred to a separate sub-phase.
func TestPhase12WasmCorpus(t *testing.T) {
	if os.Getenv("MOCHI_TEST_ZIG_DOWNLOAD") != "1" {
		t.Skip("set MOCHI_TEST_ZIG_DOWNLOAD=1 to enable WASM corpus gate")
	}

	wasmtime, wasmtimeErr := exec.LookPath("wasmtime")
	if wasmtimeErr != nil {
		t.Skip("wasmtime not found on PATH; set up wasmtime to run the WASM corpus gate")
	}

	suites := []string{
		// Phase 1-8 corpus (27 suites; arena_query/query/query_join excluded due to
		// pre-existing WASM32 alignment trap in arena allocator; functions excluded
		// here because fun_early_return needs per-fixture exclusion via
		// runFixtureSuiteWasmExclude below).
		"capturing_closures",
		"closures",
		"collection_equality",
		"control-flow",
		"divzero",
		"for-range",
		"free_function_shim",
		"index_assign",
		"list_of_list",
		"list_of_map",
		"list_of_record",
		"list_slice",
		"lists",
		"map_of_list",
		"maps",
		"math_builtins",
		"nan-inf",
		"primitives",
		"records",
		"str_convert",
		"string_extra",
		"string_methods",
		"string_ops",
		"strings",
		"sum_types",
		"type_cast",
		"typed_empty_literal",
		// Phase 9 suites (Phase 12.2): chan, stream, agent work under the WASM
		// single-fibre synchronous stub because all fixtures use buffered channels
		// and pre-emitted streams (no blocking paths exercised).
		"chan",
		"stream",
		"agent",
	}

	for _, suite := range suites {
		suite := suite
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteWasm(t, suite, wasmtime)
		})
	}

	// Shutdown suite: exclude shutdown_sched (uses extern fun run_scheduler()
	// which is an FFI neighbour .c call, excluded from WASM corpus).
	t.Run("shutdown", func(t *testing.T) {
		runFixtureSuiteWasmExclude(t, "shutdown", wasmtime, []string{"shutdown_sched"})
	})

	// Functions suite: exclude fun_early_return which defines a Mochi function
	// named "abs". wasm-wasi-musl's __math.h declares extern int abs(int), which
	// conflicts with the emitter's static int64_t abs(...). Pre-existing issue;
	// the same fixture compiles fine on LP64 targets where __math.h is not pulled in.
	t.Run("functions", func(t *testing.T) {
		runFixtureSuiteWasmExclude(t, "functions", wasmtime, []string{"fun_early_return"})
	})
}

// runFixtureSuiteWasmExclude is like runFixtureSuiteWasm but skips fixtures
// whose names appear in the exclude list. Used for suites that contain a
// mix of WASM-compatible and WASM-incompatible fixtures (e.g. shutdown, which
// includes shutdown_sched that requires an FFI neighbour .c file).
func runFixtureSuiteWasmExclude(t *testing.T, dir, wasmtime string, exclude []string) {
	t.Helper()
	skip := make(map[string]bool, len(exclude))
	for _, name := range exclude {
		skip[name] = true
	}

	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", dir)
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir %s: %v", base, err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() && !skip[e.Name()] {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no eligible fixtures under %s (all excluded?)", base)
	}

	for _, name := range names {
		name := name
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name+".wasm")
			d := &Driver{
				CacheDir: t.TempDir(),
				NoCache:  true,
			}
			if err := d.Build(src, outBin, "wasm32-wasi", ""); err != nil {
				t.Fatalf("Driver.Build(wasm32-wasi) %s: %v", src, err)
			}

			cmd := exec.Command(wasmtime, "run", outBin)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("wasmtime run %s: %v\nstdout so far: %q", name, err, stdout.String())
			}
			if got := stdout.Bytes(); !bytes.Equal(got, expect) {
				t.Fatalf("output mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, expect, got)
			}
		})
	}
}

// runFixtureSuiteWasm compiles every fixture in dir for wasm32-wasi and runs
// each produced binary via wasmtime. Helper for Phase 12.3.
func runFixtureSuiteWasm(t *testing.T, dir, wasmtime string) {
	t.Helper()
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", dir)
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir %s: %v", base, err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no fixtures under %s", base)
	}

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name+".wasm")
			d := &Driver{
				CacheDir: t.TempDir(),
				NoCache:  true,
			}
			if err := d.Build(src, outBin, "wasm32-wasi", ""); err != nil {
				t.Fatalf("Driver.Build(wasm32-wasi) %s: %v", src, err)
			}

			cmd := exec.Command(wasmtime, "run", outBin)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("wasmtime run %s: %v\nstdout so far: %q", name, err, stdout.String())
			}
			if got := stdout.Bytes(); !bytes.Equal(got, expect) {
				t.Fatalf("output mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, expect, got)
			}
		})
	}
}
