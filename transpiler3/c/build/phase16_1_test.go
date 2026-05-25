package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase16UBSan is the MEP-45 Phase 16.1 gate. It compiles the
// full current fixture corpus with -fsanitize=undefined and runs each
// binary, asserting byte-equal stdout against expect.txt. Any
// UndefinedBehaviorSanitizer diagnostic (signed integer overflow,
// misaligned pointer dereference, out-of-bounds shift, null pointer
// dereference) causes the binary to exit non-zero, which the test
// runner catches as a failure.
//
// UBSAN_OPTIONS=halt_on_error=1 ensures the first UB trap aborts the
// binary instead of printing and continuing. print_stacktrace=1 makes
// diagnostics readable in CI output.
//
// Suites excluded: same as Phase 16.0 (divzero-trip exits non-zero
// by design; file_io is orthogonal to UB).
func TestPhase16UBSan(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("UBSan gate skipped on Windows; Phase 11 wires Windows CI")
	}
	if !ubsanAvailable(t) {
		t.Skip("UBSan not available on this host (no -fsanitize=undefined support)")
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

	ubsanFlags := []string{
		"-fsanitize=undefined",
		"-fno-sanitize-recover=all",
	}
	ubsanEnv := "UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1"

	for _, suite := range suites {
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteASan(t, suite, ubsanFlags, ubsanEnv)
		})
	}
}

// ubsanAvailable probes whether the host cc understands
// -fsanitize=undefined by compiling a trivial C file.
func ubsanAvailable(t *testing.T) bool {
	t.Helper()
	src := filepath.Join(t.TempDir(), "probe.c")
	if err := os.WriteFile(src, []byte("int main(void){return 0;}\n"), 0o644); err != nil {
		t.Logf("ubsanAvailable: write probe: %v", err)
		return false
	}
	out := filepath.Join(t.TempDir(), "probe")
	cmd := exec.Command("cc", "-fsanitize=undefined", src, "-o", out)
	if output, err := cmd.CombinedOutput(); err != nil {
		t.Logf("ubsanAvailable: cc probe failed: %v\n%s", err, output)
		return false
	}
	return true
}
