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
func TestPhase12WasmCorpus(t *testing.T) {
	if os.Getenv("MOCHI_TEST_ZIG_DOWNLOAD") != "1" {
		t.Skip("set MOCHI_TEST_ZIG_DOWNLOAD=1 to enable WASM corpus gate")
	}

	wasmtime, wasmtimeErr := exec.LookPath("wasmtime")
	if wasmtimeErr != nil {
		t.Skip("wasmtime not found on PATH; set up wasmtime to run the WASM corpus gate")
	}

	suites := []string{
		"arena_query",
		"capturing_closures",
		"closures",
		"collection_equality",
		"control-flow",
		"divzero",
		"for-range",
		"free_function_shim",
		"functions",
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
		"query",
		"query_join",
		"records",
		"str_convert",
		"string_extra",
		"string_methods",
		"string_ops",
		"strings",
		"sum_types",
		"type_cast",
		"typed_empty_literal",
	}

	for _, suite := range suites {
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteWasm(t, suite, wasmtime)
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
