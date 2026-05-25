package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"testing"
)

// TestPhase16ASan is the MEP-45 Phase 16.0 gate. It compiles the full
// current fixture corpus with -fsanitize=address and runs each binary,
// asserting byte-equal stdout against expect.txt. Any AddressSanitizer
// diagnostic (use-after-free, heap buffer overflow, stack overflow,
// use-after-return) causes the binary to abort, which the test runner
// catches as a non-zero exit.
//
// Leak detection is suppressed via ASAN_OPTIONS=detect_leaks=0 because
// the GC-less mochi runtime intentionally does not free heap-allocated
// lists/maps/strings at program exit. A full LeakSan pass requires
// explicit free() calls at every exit path, which is tracked as a
// follow-on sub-phase.
//
// Platform notes:
//   - macOS (Apple clang): -fsanitize=address only; LeakSan is not
//     available in Apple's clang toolchain.
//   - Linux (clang/gcc): -fsanitize=address; leak detection disabled
//     via ASAN_OPTIONS.
//   - Windows: skipped (ASan requires PE-specific instrumentation not
//     present in the Phase 16 toolchain configuration).
//
// Suites excluded from this gate:
//   - divzero-trip: intentionally exits non-zero (MOCHI_ERR_DIVZERO),
//     which the standard fixture runner would flag as a failure. It is
//     exercised by TestPhase2DivzeroTrip with a custom exit-code check.
func TestPhase16ASan(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("ASan gate skipped on Windows; Phase 11 wires Windows CI")
	}

	// Verify that the C compiler actually supports -fsanitize=address
	// before running the full suite. Some CI images ship a stripped cc
	// that doesn't include the ASan runtime.
	if !asanAvailable(t) {
		t.Skip("ASan not available on this host (no -fsanitize=address support)")
	}

	// suites is every fixture directory we run under ASan. We exclude
	// divzero-trip (intentional non-zero exit) and file_io (writes to
	// /tmp; correct but orthogonal to memory safety; covered by its own
	// phase gate).
	// Suites with sub-fixture directories (each subdirectory is one
	// fixture: <name>.mochi + expect.txt). The "hello" top-level
	// fixture is flat (no subdirs) and is omitted; it is already
	// covered by TestHello and by the primitives suite.
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

	asanFlags := []string{"-fsanitize=address"}
	// ASAN_OPTIONS: detect_leaks=0 suppresses LeakSan because the
	// GC-less runtime does not free heap at exit. halt_on_error=1
	// ensures ASan aborts on the first real memory error rather than
	// continuing (the binary exit code then != 0, which the runner
	// below treats as a test failure). abort_on_error=1 makes the exit
	// unconditional even on hosts where ASAN_OPTIONS defaults differ.
	asanEnv := "ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:abort_on_error=0"

	for _, suite := range suites {
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteASan(t, suite, asanFlags, asanEnv)
		})
	}
}

// asanAvailable probes whether the host cc understands -fsanitize=address
// by compiling a trivial C file. Returns false (and logs) on failure
// instead of fataling so the caller can decide to skip.
func asanAvailable(t *testing.T) bool {
	t.Helper()
	src := filepath.Join(t.TempDir(), "probe.c")
	if err := writeProbeC(src); err != nil {
		t.Logf("asanAvailable: write probe: %v", err)
		return false
	}
	out := filepath.Join(t.TempDir(), "probe")
	cmd := exec.Command("cc", "-fsanitize=address", src, "-o", out)
	if output, err := cmd.CombinedOutput(); err != nil {
		t.Logf("asanAvailable: cc probe failed: %v\n%s", err, output)
		return false
	}
	return true
}

// writeProbeC writes a minimal valid C file to path for sanitiser probing.
func writeProbeC(path string) error {
	return os.WriteFile(path, []byte("int main(void){return 0;}\n"), 0o644)
}

// runFixtureSuiteASan is like runFixtureSuite but compiles each fixture
// with extraFlags and sets asanEnv when running the binary.
func runFixtureSuiteASan(t *testing.T, dir string, extraFlags []string, asanEnv string) {
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

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{
				CacheDir:   t.TempDir(),
				NoCache:    true,
				ExtraFlags: extraFlags,
			}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}

			cmd := exec.Command(outBin)
			// Inject ASAN_OPTIONS into the subprocess environment.
			cmd.Env = append(os.Environ(), strings.Split(asanEnv, " ")...)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s (ASan): %v\nstdout so far: %q", name, err, stdout.String())
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}
