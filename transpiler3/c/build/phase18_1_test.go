//go:build !windows

package build

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"syscall"
	"testing"
	"time"
)

// TestPhase18ExtendedMetrics is the MEP-45 Phase 18.1 gate. It extends the
// Phase 18.0 harness with four additional measurements per kernel:
//
//   - compile_ms: wall-clock time for Driver.Build (parse + lower + emit + cc)
//   - rss_kb: peak RSS of the run binary (via ProcessState.SysUsage; 0 on
//     platforms that do not expose it)
//   - stripped_kb: binary size after `strip` (the release shipping size)
//   - vm3_med_ms: median wall-clock of 5 `mochi run` invocations (skipped
//     if the `mochi` binary is not on PATH)
//
// The 2x vm3 gate: aot_med_ms must be <= 2 * vm3_med_ms. Skipped when
// `mochi` is not available, so CI without a pre-built mochi still passes.
//
// Platform: Windows skipped (no strip, no POSIX resource usage API).
func TestPhase18ExtendedMetrics(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 18.1 extended metrics skipped on Windows")
	}

	root := repoRoot(t)
	benchDir := filepath.Join(root, "tests", "transpiler3", "c", "bench")

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

	mochiPath, _ := exec.LookPath("mochi")
	if mochiPath == "" {
		t.Log("mochi not on PATH; 2x vm3 gate skipped (metrics-only run)")
	}

	const runs = 5
	t.Logf("%-16s  %10s  %10s  %10s  %10s  %10s  %10s",
		"kernel", "compile_ms", "aot_med_ms", "vm3_med_ms", "ratio", "rss_kb", "stripped_kb")
	t.Logf("%-16s  %10s  %10s  %10s  %10s  %10s  %10s",
		"------", "----------", "----------", "----------", "-----", "------", "-----------")

	for _, k := range kernels {
		t.Run(k.name, func(t *testing.T) {
			src := filepath.Join(benchDir, k.name+".mochi")
			outBin := filepath.Join(t.TempDir(), k.name)

			// -- compile time --
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			compileStart := time.Now()
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("build: %v", err)
			}
			compileMs := float64(time.Since(compileStart)) / float64(time.Millisecond)

			// -- AOT run (5x) --
			var aotDurations []time.Duration
			var peakRSSKB int64
			for range runs {
				cmd := exec.Command(outBin)
				var stdout bytes.Buffer
				cmd.Stdout = &stdout
				cmd.Stderr = os.Stderr
				start := time.Now()
				if err := cmd.Run(); err != nil {
					t.Fatalf("run aot: %v", err)
				}
				aotDurations = append(aotDurations, time.Since(start))

				if got := stdout.String(); got != k.expect {
					t.Fatalf("stdout mismatch: want %q got %q", k.expect, got)
				}
				if ru, ok := cmd.ProcessState.SysUsage().(*syscall.Rusage); ok && ru != nil {
					rss := rssKB(ru)
					if rss > peakRSSKB {
						peakRSSKB = rss
					}
				}
			}
			slices.Sort(aotDurations)
			aotMedMs := float64(aotDurations[runs/2]) / float64(time.Millisecond)

			// -- stripped binary size --
			strippedKB := strippedSizeKB(t, outBin)

			// -- vm3 run (5x) --
			vm3MedMs := 0.0
			ratio := "n/a"
			if mochiPath != "" {
				var vm3Durations []time.Duration
				for range runs {
					cmd := exec.Command(mochiPath, "run", src)
					cmd.Stdout = io.Discard
					cmd.Stderr = os.Stderr
					start := time.Now()
					if err := cmd.Run(); err != nil {
						t.Logf("mochi run %s: %v (skipping 2x gate)", k.name, err)
						vm3MedMs = 0
						break
					}
					vm3Durations = append(vm3Durations, time.Since(start))
				}
				if len(vm3Durations) == runs {
					slices.Sort(vm3Durations)
					vm3MedMs = float64(vm3Durations[runs/2]) / float64(time.Millisecond)
					r := aotMedMs / vm3MedMs
					ratio = fmt.Sprintf("%.2fx", r)
					// 2x gate: aot must be no more than 2x slower than vm3.
					if r > 2.0 {
						t.Errorf("2x gate failed: aot %.2fms > 2 * vm3 %.2fms (ratio %.2fx)",
							aotMedMs, vm3MedMs, r)
					}
				}
			}

			t.Logf("%-16s  %10s  %10s  %10s  %10s  %10s  %10s",
				k.name,
				fmt.Sprintf("%.0fms", compileMs),
				fmt.Sprintf("%.2fms", aotMedMs),
				fmt.Sprintf("%.2fms", vm3MedMs),
				ratio,
				fmt.Sprintf("%dKiB", peakRSSKB),
				fmt.Sprintf("%dKiB", strippedKB),
			)
		})
	}
}

// strippedSizeKB copies the binary, runs strip on it, and returns the
// resulting file size in KiB. Returns 0 if strip is not available.
func strippedSizeKB(t *testing.T, bin string) int64 {
	t.Helper()
	stripPath, err := exec.LookPath("strip")
	if err != nil {
		return 0
	}
	stripped := bin + ".stripped"
	data, err := os.ReadFile(bin)
	if err != nil {
		return 0
	}
	if err := os.WriteFile(stripped, data, 0o755); err != nil {
		return 0
	}
	_ = exec.Command(stripPath, stripped).Run()
	fi, err := os.Stat(stripped)
	if err != nil {
		return 0
	}
	return fi.Size() / 1024
}

// rssKB extracts the peak RSS in KiB from a syscall.Rusage. On macOS the
// Maxrss field is in bytes; on Linux it is in KiB.
func rssKB(ru *syscall.Rusage) int64 {
	rss := ru.Maxrss
	if runtime.GOOS == "darwin" {
		return int64(rss) / 1024
	}
	return int64(rss)
}
