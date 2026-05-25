package build

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"testing"
	"time"
)

// TestPhase18BenchHarness is the MEP-45 Phase 18.0 gate. It builds
// each kernel under tests/transpiler3/c/bench/ via the AOT transpiler,
// runs it 5 times, and records wall-clock time (median) and binary size.
// The test does NOT enforce the 2x gate vs vm3 yet (Phase 18.1+);
// it asserts only that:
//   - each kernel compiles and runs successfully,
//   - the output matches the vm3 oracle, and
//   - the binary size and median wall-clock are logged for human review.
//
// BG kernel corpus:
//   - hello_world  baseline binary size + startup overhead
//   - sum_loop     tight integer arithmetic loop (sum 1..1_000_000)
//   - fib_iter     iterative Fibonacci(50)
//   - fib_rec      recursive Fibonacci(35)  - function-call overhead
//   - list_sum     list append + for-in iteration (10 000 elements)
func TestPhase18BenchHarness(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 18 bench harness skipped on Windows; Phase 11 wires Windows CI")
	}

	root := repoRoot(t)
	benchDir := filepath.Join(root, "tests", "transpiler3", "c", "bench")

	// kernel → expected stdout (obtained from vm3)
	kernels := []struct {
		name   string
		expect string
	}{
		{"hello_world", "Hello, World!\n"},
		{"sum_loop", "500000500000\n"},
		{"fib_iter", "12586269025\n"},
		{"fib_rec", "9227465\n"},
		{"list_sum", "49995000\n"},
	}

	const runs = 5
	t.Logf("%-16s  %8s  %10s  %10s  %10s", "kernel", "binsize", "min_ms", "med_ms", "max_ms")
	t.Logf("%-16s  %8s  %10s  %10s  %10s", "------", "-------", "------", "------", "------")

	for _, k := range kernels {
		t.Run(k.name, func(t *testing.T) {
			src := filepath.Join(benchDir, k.name+".mochi")
			outBin := filepath.Join(t.TempDir(), k.name)

			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("build: %v", err)
			}

			fi, err := os.Stat(outBin)
			if err != nil {
				t.Fatalf("stat: %v", err)
			}
			binSize := fi.Size()

			var durations []time.Duration
			for range runs {
				cmd := exec.Command(outBin)
				var stdout bytes.Buffer
				cmd.Stdout = &stdout
				cmd.Stderr = os.Stderr
				start := time.Now()
				if err := cmd.Run(); err != nil {
					t.Fatalf("run: %v", err)
				}
				dur := time.Since(start)
				durations = append(durations, dur)

				if got := stdout.String(); got != k.expect {
					t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", k.expect, got)
				}
			}

			slices.Sort(durations)
			minMs := float64(durations[0]) / float64(time.Millisecond)
			medMs := float64(durations[runs/2]) / float64(time.Millisecond)
			maxMs := float64(durations[runs-1]) / float64(time.Millisecond)

			t.Logf("%-16s  %8s  %10s  %10s  %10s",
				k.name,
				fmtBytes(binSize),
				fmt.Sprintf("%.2fms", minMs),
				fmt.Sprintf("%.2fms", medMs),
				fmt.Sprintf("%.2fms", maxMs),
			)
		})
	}
}

func fmtBytes(n int64) string {
	switch {
	case n >= 1<<20:
		return fmt.Sprintf("%.1fMiB", float64(n)/(1<<20))
	case n >= 1<<10:
		return fmt.Sprintf("%.1fKiB", float64(n)/(1<<10))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
