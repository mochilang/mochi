package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase16MSan is the MEP-45 Phase 16.3 gate. It compiles the full
// current fixture corpus with clang's -fsanitize=memory and runs each
// binary, asserting byte-equal stdout against expect.txt. Any read from
// uninitialized memory causes the binary to abort, which the test runner
// catches as a non-zero exit.
//
// MSan requirements:
//   - Linux only: Apple-silicon MSan is unsupported upstream.
//   - clang only: gcc does not implement MemorySanitizer.
//   - All code must be compiled with MSan instrumentation; the mochi
//     runtime is compiled from source in each Driver.Build call, so the
//     instrumentation is complete. System libc functions are intercepted
//     by clang's MSan runtime automatically.
//
// Suites excluded from this gate (same rationale as Phase 16.0 ASan):
//   - divzero-trip: intentionally exits non-zero.
//   - hello: flat fixture (no subdirectory per fixture).
//   - file_io: deferred; fopen/fread MSan interception edge-cases are
//     tracked as a follow-on.
//   - csv_adapters: uses fopen/fgets; deferred alongside file_io.
//   - ffi: requires a neighbour .c file compiled without MSan flags;
//     the cross-TU boundary makes instrumentation incomplete.
func TestPhase16MSan(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skip("Phase 16.3 MSan gate runs on Linux only (Apple-silicon MSan unsupported upstream)")
	}

	clangPath, clangErr := exec.LookPath("clang")
	if clangErr != nil {
		t.Skip("Phase 16.3 MSan gate requires clang (not found on PATH)")
	}
	if !msanAvailable(t, clangPath) {
		t.Skip("Phase 16.3 MSan not available with this clang build (no -fsanitize=memory support)")
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

	msanFlags := []string{"-fsanitize=memory", "-fno-omit-frame-pointer"}
	// halt_on_error=1: abort on first MSan error (non-zero exit).
	// poison_in_dtor=0: suppress synthetic destructor poisoning noise.
	msanEnv := "MSAN_OPTIONS=halt_on_error=1:poison_in_dtor=0"

	for _, suite := range suites {
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteMSan(t, suite, clangPath, msanFlags, msanEnv)
		})
	}
}

// msanAvailable probes whether clang supports -fsanitize=memory by compiling
// a trivial C file. Returns false (and logs) on failure so the caller can skip.
func msanAvailable(t *testing.T, clangPath string) bool {
	t.Helper()
	src := filepath.Join(t.TempDir(), "msan_probe.c")
	if err := os.WriteFile(src, []byte("int main(void){return 0;}\n"), 0o644); err != nil {
		t.Logf("msanAvailable: write probe: %v", err)
		return false
	}
	out := filepath.Join(t.TempDir(), "msan_probe")
	cmd := exec.Command(clangPath, "-fsanitize=memory", src, "-o", out)
	if output, err := cmd.CombinedOutput(); err != nil {
		t.Logf("msanAvailable: clang probe failed: %v\n%s", err, output)
		return false
	}
	return true
}

// runFixtureSuiteMSan is like runFixtureSuiteASan but forces clang as the
// compiler (MSan is clang-only) via Driver.CC.
func runFixtureSuiteMSan(t *testing.T, dir, clangPath string, extraFlags []string, msanEnv string) {
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
				CC:         clangPath,
				CacheDir:   t.TempDir(),
				NoCache:    true,
				ExtraFlags: extraFlags,
			}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}

			cmd := exec.Command(outBin)
			cmd.Env = append(os.Environ(), msanEnv)
			var stdout, stderr strings.Builder
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s (MSan): %v\nstdout: %q\nstderr: %q",
					name, err, stdout.String(), stderr.String())
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}
