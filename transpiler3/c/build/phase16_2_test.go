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

// TestPhase16TSan is the MEP-45 Phase 16.2 gate. It compiles the streams
// and agents fixture corpus with -fsanitize=thread (TSan) and asserts
// byte-equal stdout against expect.txt.
//
// All Phase 9 concurrency primitives (scheduler fibers, chan<T>, stream<T>,
// agents, method shims) run on a single OS thread via cooperative ucontext
// scheduling, so no actual data races exist. TSan clean on this corpus
// confirms that the runtime's stack-switching and ring-buffer operations do
// not trigger false positives under thread sanitisation.
//
// Platform notes:
//   - macOS (Apple clang): -fsanitize=thread is supported.
//   - Linux (clang/gcc): -fsanitize=thread is supported.
//   - Windows: skipped (TSan not available in the Phase 16 toolchain).
func TestPhase16TSan(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("TSan gate skipped on Windows")
	}
	if !tsanAvailable(t) {
		t.Skip("TSan not available on this host (no -fsanitize=thread support)")
	}

	suites := []string{
		"agent",
		"chan",
		"method_shim",
		"scheduler",
		"stream",
	}

	tsanFlags := []string{"-fsanitize=thread"}
	tsanEnv := "TSAN_OPTIONS=halt_on_error=1"

	for _, suite := range suites {
		suite := suite
		t.Run(suite, func(t *testing.T) {
			runFixtureSuiteTSan(t, suite, tsanFlags, tsanEnv)
		})
	}
}

// tsanAvailable probes whether the host cc understands -fsanitize=thread.
func tsanAvailable(t *testing.T) bool {
	t.Helper()
	src := filepath.Join(t.TempDir(), "probe.c")
	if err := writeProbeC(src); err != nil {
		t.Logf("tsanAvailable: write probe: %v", err)
		return false
	}
	out := filepath.Join(t.TempDir(), "probe")
	cmd := exec.Command("cc", "-fsanitize=thread", src, "-o", out)
	if output, err := cmd.CombinedOutput(); err != nil {
		t.Logf("tsanAvailable: cc probe failed: %v\n%s", err, output)
		return false
	}
	return true
}

// runFixtureSuiteTSan is like runFixtureSuiteASan but for TSan.
func runFixtureSuiteTSan(t *testing.T, dir string, extraFlags []string, tsanEnv string) {
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
		name := name
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
			cmd.Env = append(os.Environ(), strings.Split(tsanEnv, " ")...)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s (TSan): %v\nstdout so far: %q", name, err, stdout.String())
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}
