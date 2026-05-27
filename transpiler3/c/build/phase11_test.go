package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// Phase 11 gate tests: cross-compile the primitives/add_ints fixture for
// each tier-1 triple and (where possible) run the produced binary.
//
// Run-gate availability:
//   - x86_64-linux-gnu  (11.0): native on Linux x86_64 CI
//   - x86_64-linux-musl (11.1): zig cc cross + native run on Linux x86_64
//   - aarch64-linux-gnu (11.2): zig cc cross + qemu-aarch64-static run
//   - aarch64-linux-musl(11.3): zig cc cross + qemu-aarch64-static run
//   - aarch64-darwin    (11.4): native on macOS arm64
//   - x86_64-darwin     (11.5): native on macOS x86_64 or zig cc cross on arm64
//   - x86_64-windows-msvc(11.6): Windows CI (Phase 11.6)
//   - x86_64-windows-gnu (11.7): Windows CI (Phase 11.7)
//
// All cross tests are gated on MOCHI_TEST_ZIG_DOWNLOAD=1 to avoid
// hitting the network in the default test run.

const phase11Fixture = "primitives/add_ints"
const phase11Expect = "3\n"

// runPhase11Cross builds the add_ints fixture for the given triple
// using zig cc, runs the binary under runner (if non-nil), and asserts
// the output matches phase11Expect.
func runPhase11Cross(t *testing.T, triple string, runner func(bin string) ([]byte, error)) {
	t.Helper()
	if os.Getenv("MOCHI_TEST_ZIG_DOWNLOAD") != "1" {
		t.Skipf("set MOCHI_TEST_ZIG_DOWNLOAD=1 to enable cross-compile gate (triple=%s)", triple)
	}

	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", phase11Fixture, "add_ints.mochi")
	want := []byte(phase11Expect)

	outBin := filepath.Join(t.TempDir(), "add_ints_"+triple)
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if err := d.Build(src, outBin, triple, ""); err != nil {
		t.Fatalf("Driver.Build(triple=%s): %v", triple, err)
	}

	if runner == nil {
		t.Logf("no run-gate for triple=%s (compile-only check passed)", triple)
		return
	}
	got, err := runner(outBin)
	if err != nil {
		t.Fatalf("run %s binary: %v", triple, err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch: want %q got %q", want, got)
	}
}

// TestPhase11NativeLinux is the Phase 11.0 gate. Builds add_ints natively
// on Linux x86_64 (host cc, no cross) and runs it.
func TestPhase11NativeLinux(t *testing.T) {
	if runtime.GOOS != "linux" || runtime.GOARCH != "amd64" {
		t.Skip("Phase 11.0 native gate runs only on linux/amd64")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", phase11Fixture, "add_ints.mochi")
	outBin := filepath.Join(t.TempDir(), "add_ints_linux_native")
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build (native linux): %v", err)
	}
	cmd := exec.Command(outBin)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run native linux binary: %v", err)
	}
	if got := stdout.String(); got != phase11Expect {
		t.Fatalf("want %q got %q", phase11Expect, got)
	}
}

// TestPhase11NativeDarwin is the Phase 11.4/11.5 gate. Builds add_ints
// natively on macOS (aarch64 or x86_64) and runs it.
func TestPhase11NativeDarwin(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("Phase 11.4/11.5 native gate runs only on macOS")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", phase11Fixture, "add_ints.mochi")
	outBin := filepath.Join(t.TempDir(), "add_ints_darwin_native")
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build (native darwin): %v", err)
	}
	cmd := exec.Command(outBin)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run native darwin binary: %v", err)
	}
	if got := stdout.String(); got != phase11Expect {
		t.Fatalf("want %q got %q", phase11Expect, got)
	}
}

// TestPhase11MuslX86 is the Phase 11.1 gate. Cross-builds add_ints for
// x86_64-linux-musl using zig cc and runs it (native on Linux x86_64;
// skipped on other hosts).
func TestPhase11MuslX86(t *testing.T) {
	nativeRun := runtime.GOOS == "linux" && runtime.GOARCH == "amd64"
	runFn := func(bin string) ([]byte, error) {
		if !nativeRun {
			return nil, nil
		}
		cmd := exec.Command(bin)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		if err := cmd.Run(); err != nil {
			return nil, err
		}
		return stdout.Bytes(), nil
	}
	if !nativeRun {
		runFn = nil
	}
	runPhase11Cross(t, "x86_64-linux-musl", runFn)
}

// TestPhase11AArch64LinuxMusl is the Phase 11.3 gate. Cross-builds add_ints
// for aarch64-linux-musl using zig cc and (when qemu-aarch64-static is
// available) runs it via qemu.
func TestPhase11AArch64LinuxMusl(t *testing.T) {
	qemu, qemuErr := exec.LookPath("qemu-aarch64-static")
	if qemuErr != nil {
		qemu, qemuErr = exec.LookPath("qemu-aarch64")
	}
	runFn := func(bin string) ([]byte, error) {
		if qemuErr != nil {
			return nil, nil // compile-only when no qemu
		}
		cmd := exec.Command(qemu, bin)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		if err := cmd.Run(); err != nil {
			return nil, err
		}
		return stdout.Bytes(), nil
	}
	if qemuErr != nil {
		runFn = nil
	}
	runPhase11Cross(t, "aarch64-linux-musl", runFn)
}

// TestPhase11AArch64LinuxGnu is the Phase 11.2 gate. Cross-builds add_ints
// for aarch64-linux-gnu using zig cc and (when qemu-aarch64-static and the
// aarch64 glibc sysroot are available) runs it via qemu with QEMU_LD_PREFIX.
func TestPhase11AArch64LinuxGnu(t *testing.T) {
	qemu, qemuErr := exec.LookPath("qemu-aarch64-static")
	if qemuErr != nil {
		qemu, qemuErr = exec.LookPath("qemu-aarch64")
	}
	runFn := func(bin string) ([]byte, error) {
		if qemuErr != nil {
			return nil, nil
		}
		cmd := exec.Command(qemu, bin)
		// aarch64-linux-gnu binaries are glibc-linked; qemu-user-static
		// needs QEMU_LD_PREFIX pointing at the cross sysroot so the
		// dynamic linker can resolve libc.so.6 and ld-linux-aarch64.so.1.
		cmd.Env = append(os.Environ(), "QEMU_LD_PREFIX=/usr/aarch64-linux-gnu")
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		if err := cmd.Run(); err != nil {
			return nil, err
		}
		return stdout.Bytes(), nil
	}
	if qemuErr != nil {
		runFn = nil
	}
	runPhase11Cross(t, "aarch64-linux-gnu", runFn)
}

// TestPhase11X86DarwinCross is the Phase 11.5 gate. Cross-builds add_ints
// for x86_64-macos-none using zig cc on an arm64 macOS host and runs the
// produced binary (Rosetta 2 executes it transparently on Apple Silicon).
// On an x86_64 macOS host the native run gate is already covered by
// TestPhase11NativeDarwin, so this test skips on non-arm64 macOS.
func TestPhase11X86DarwinCross(t *testing.T) {
	if runtime.GOOS != "darwin" || runtime.GOARCH != "arm64" {
		t.Skip("Phase 11.5 cross gate runs only on darwin/arm64 (arm64 host cross-compiling for x86_64)")
	}
	runFn := func(bin string) ([]byte, error) {
		cmd := exec.Command(bin)
		var stdout bytes.Buffer
		cmd.Stdout = &stdout
		if err := cmd.Run(); err != nil {
			return nil, err
		}
		return stdout.Bytes(), nil
	}
	runPhase11Cross(t, "x86_64-macos-none", runFn)
}

// TestPhase11WindowsGnu is the Phase 11.7 gate. Cross-builds add_ints for
// x86_64-windows-gnu using zig cc; on a Windows host the binary is run
// natively. On non-Windows hosts only the compile step is verified.
func TestPhase11WindowsGnu(t *testing.T) {
	var runFn func(bin string) ([]byte, error)
	if runtime.GOOS == "windows" {
		runFn = func(bin string) ([]byte, error) {
			cmd := exec.Command(bin + ".exe")
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			if err := cmd.Run(); err != nil {
				return nil, err
			}
			out := strings.ReplaceAll(string(stdout.Bytes()), "\r\n", "\n")
			return []byte(out), nil
		}
	}
	runPhase11Cross(t, "x86_64-windows-gnu", runFn)
}

// TestPhase11NativeWindows is the Phase 11.6 gate. Builds add_ints natively
// on a Windows host (host cc, no cross) and runs it.
func TestPhase11NativeWindows(t *testing.T) {
	if runtime.GOOS != "windows" {
		t.Skip("Phase 11.6 native gate runs only on Windows")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", phase11Fixture, "add_ints.mochi")
	outBin := filepath.Join(t.TempDir(), "add_ints_windows_native.exe")
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build (native windows): %v", err)
	}
	cmd := exec.Command(outBin)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run native windows binary: %v", err)
	}
	got := strings.ReplaceAll(stdout.String(), "\r\n", "\n")
	if got != phase11Expect {
		t.Fatalf("want %q got %q", phase11Expect, got)
	}
}
