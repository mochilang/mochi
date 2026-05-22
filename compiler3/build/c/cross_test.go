package cbuild

import (
	"encoding/binary"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	cgen "mochi/compiler3/emit/c"
	"mochi/compiler3/ir"
)

// Phase 5.0 cross-target test suite. The §9 phase-1 matrix lists five
// must-have targets; Phase 4.0 covers the host (x86_64 Linux). Phase
// 5.0 unlocks the other four by routing through `zig cc -target=<triple>`.
// We do not (and cannot) execute the produced binaries on this host,
// so the gate is "the binary is the expected file format for the
// requested triple". Format is verified by reading the magic bytes
// (ELF e_machine for Linux triples, Wasm 1.0 magic for wasm32-wasi).
//
// Skip the whole suite when `zig` is not on PATH. The host-target tests
// in driver_test.go remain the load-bearing gate when zig is absent.

// hasZig reports whether `zig` is on PATH. Used to skip cross-target
// tests on machines that ship only `cc`; the host gate is unaffected.
func hasZig() bool {
	_, err := exec.LookPath("zig")
	return err == nil
}

// elfMachine reads the e_machine field of an ELF file (offset 0x12,
// 2 bytes, little-endian). Returns 0 if the file is not an ELF.
const (
	elfMachineX86_64  = 0x3E
	elfMachineAArch64 = 0xB7
)

func elfMachine(t *testing.T, path string) uint16 {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	if len(data) < 0x14 {
		t.Fatalf("file %s too short to be ELF (%d bytes)", path, len(data))
	}
	if string(data[:4]) != "\x7fELF" {
		t.Fatalf("file %s is not ELF (magic=%q)", path, data[:4])
	}
	return binary.LittleEndian.Uint16(data[0x12:0x14])
}

// machoCPUType reads the cputype field of a Mach-O file (offset 4,
// 4 bytes, little-endian). Returns 0 if the file is not Mach-O 64.
// Magic 0xFEEDFACF is Mach-O 64-bit; cputype 0x01000007 is x86_64
// and 0x0100000C is arm64.
const (
	machoMagic64       = 0xFEEDFACF
	machoCPUTypeX86_64 = 0x01000007
	machoCPUTypeArm64  = 0x0100000C
)

func machoCPUType(t *testing.T, path string) uint32 {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	if len(data) < 8 {
		t.Fatalf("file %s too short to be Mach-O (%d bytes)", path, len(data))
	}
	if magic := binary.LittleEndian.Uint32(data[0:4]); magic != machoMagic64 {
		t.Fatalf("file %s is not Mach-O 64 (magic=0x%X)", path, magic)
	}
	return binary.LittleEndian.Uint32(data[4:8])
}

// isWasm reports whether the file at path starts with the Wasm 1.0
// magic (`\0asm` followed by version 1).
func isWasm(t *testing.T, path string) bool {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	if len(data) < 8 {
		return false
	}
	return string(data[:4]) == "\x00asm" &&
		binary.LittleEndian.Uint32(data[4:8]) == 1
}

// crossProgram builds a one-function Program that returns 42. The
// emitter wraps it in `int main(void)` via Program.Main so the
// produced object links as a standalone executable. This is the same
// shape TestBuildHelloEndToEnd uses on the host; Phase 5.0 reuses it
// across triples to keep the cross-target gate aligned with the
// host gate (identical input, identical expected return).
func crossProgram() *cgen.Program {
	fn := &ir.Function{Name: "answer", Result: ir.TypeI64}
	bid := fn.AddBlock()
	c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 42})
	blk := fn.Block(bid)
	blk.Values = []uint32{c}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: c}
	return &cgen.Program{Funcs: []*ir.Function{fn}, Main: "answer"}
}

// runner names a foreign-arch executor: the command + flag prefix
// that runs a binary built for some target on this host. When PATH
// has no runner for a triple, the test skips the run-gate (the
// build-gate still fires).
//
// Linux ELFs run under qemu-user (qemu-x86_64 / qemu-aarch64). Wasm
// modules run under wasmtime (`wasmtime run --`). The framework is
// extensible: every new triple lands its runner here, in one place.
type runner struct {
	cmd  string
	args []string
}

func findRunner(triple string) (runner, bool) {
	switch triple {
	case "x86_64-linux-musl", "x86_64-linux-gnu":
		// Linux x86_64 ELF runs natively on a linux/amd64 host, or
		// under qemu-user on any host with the binary on PATH.
		if runtime.GOOS == "linux" && runtime.GOARCH == "amd64" {
			return runner{}, true
		}
		if p, err := exec.LookPath("qemu-x86_64"); err == nil {
			return runner{cmd: p}, true
		}
		if p, err := exec.LookPath("qemu-x86_64-static"); err == nil {
			return runner{cmd: p}, true
		}
	case "aarch64-linux-musl", "aarch64-linux-gnu":
		if runtime.GOOS == "linux" && runtime.GOARCH == "arm64" {
			return runner{}, true
		}
		if p, err := exec.LookPath("qemu-aarch64"); err == nil {
			return runner{cmd: p}, true
		}
		if p, err := exec.LookPath("qemu-aarch64-static"); err == nil {
			return runner{cmd: p}, true
		}
	case "aarch64-macos":
		// Native execution on arm64 Darwin. On x86_64 Darwin this
		// would require Rosetta-reverse (does not exist); skip there.
		if runtime.GOOS == "darwin" && runtime.GOARCH == "arm64" {
			return runner{}, true
		}
	case "x86_64-macos":
		// Native on x86_64 Darwin, or under Rosetta 2 on arm64 Darwin
		// (which the OS handles transparently, no runner prefix
		// needed). On non-Darwin hosts there is no way to run a
		// Mach-O binary, so we skip.
		if runtime.GOOS == "darwin" {
			return runner{}, true
		}
	case "wasm32-wasi":
		if p, err := exec.LookPath("wasmtime"); err == nil {
			return runner{cmd: p, args: []string{"run", "--"}}, true
		}
		if p, err := exec.LookPath("wasmer"); err == nil {
			return runner{cmd: p, args: []string{"run"}}, true
		}
		if p, err := exec.LookPath("wasm3"); err == nil {
			return runner{cmd: p}, true
		}
	}
	return runner{}, false
}

// runBinary executes binPath through r and returns its stdout. Fails
// the test on non-zero exit. The trailing newline is preserved so
// callers can match the emitter's "%lld\n" output byte-for-byte.
// When r.cmd is empty, the binary is executed directly (native host
// matches target ABI; no emulator prefix needed).
func runBinary(t *testing.T, r runner, binPath string) string {
	t.Helper()
	var cmd *exec.Cmd
	if r.cmd == "" {
		cmd = exec.Command(binPath)
	} else {
		argv := append(append([]string{}, r.args...), binPath)
		cmd = exec.Command(r.cmd, argv...)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run %s: %v\n%s", strings.Join(cmd.Args, " "), err, out)
	}
	return string(out)
}

// assertCrossOutput is the Phase 5.0 run-gate: when a runner is
// available, execute the foreign-arch binary and assert stdout
// matches the host gate's output ("42\n"). When no runner is on
// PATH, skip the run-gate (the build-gate's file-format check has
// already fired) and log a hint pointing at the install command.
func assertCrossOutput(t *testing.T, triple, binPath string) {
	t.Helper()
	r, ok := findRunner(triple)
	if !ok {
		t.Logf("no runner for %s on PATH; skipping run-gate (install qemu / wasmtime to enable)", triple)
		return
	}
	got := runBinary(t, r, binPath)
	const want = "42\n"
	if got != want {
		t.Errorf("cross-run %s via %s: stdout = %q, want %q", triple, r.cmd, got, want)
	}
}

// TestBuildCrossX86_64Linux is the Phase 5.0 gate for the §9
// x86_64 Linux row: `mochi build --target=c --triple=x86_64-linux-musl`
// produces an ELF that names x86_64 in its e_machine.
func TestBuildCrossX86_64Linux(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.0 cross-target gate requires zig cc")
	}
	dir := t.TempDir()
	binPath := filepath.Join(dir, "answer.x86_64-linux")
	_, err := Build(crossProgram(), Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     "x86_64-linux-musl",
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if got := elfMachine(t, binPath); got != elfMachineX86_64 {
		t.Errorf("e_machine = 0x%X, want 0x%X (x86_64)", got, elfMachineX86_64)
	}
	assertCrossOutput(t, "x86_64-linux-musl", binPath)
}

// TestBuildCrossAArch64Linux is the Phase 5.0 gate for the §9
// aarch64 Linux row.
func TestBuildCrossAArch64Linux(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.0 cross-target gate requires zig cc")
	}
	dir := t.TempDir()
	binPath := filepath.Join(dir, "answer.aarch64-linux")
	_, err := Build(crossProgram(), Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     "aarch64-linux-musl",
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if got := elfMachine(t, binPath); got != elfMachineAArch64 {
		t.Errorf("e_machine = 0x%X, want 0x%X (aarch64)", got, elfMachineAArch64)
	}
	assertCrossOutput(t, "aarch64-linux-musl", binPath)
}

// TestBuildCrossAArch64Macos is the Phase 5.0 gate for the §9
// aarch64 macOS row. On an Apple Silicon host the binary runs
// natively; on other hosts the build-gate (Mach-O cputype check)
// still fires and the run-gate skips.
func TestBuildCrossAArch64Macos(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.0 cross-target gate requires zig cc")
	}
	dir := t.TempDir()
	binPath := filepath.Join(dir, "answer.aarch64-macos")
	_, err := Build(crossProgram(), Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     "aarch64-macos",
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if got := machoCPUType(t, binPath); got != machoCPUTypeArm64 {
		t.Errorf("cputype = 0x%X, want 0x%X (arm64)", got, machoCPUTypeArm64)
	}
	assertCrossOutput(t, "aarch64-macos", binPath)
}

// TestBuildCrossX86_64Macos is the Phase 5.0 gate for the §9
// x86_64 macOS row. On an Apple Silicon host Rosetta 2 runs the
// binary transparently; on an x86_64 Darwin host it runs natively.
func TestBuildCrossX86_64Macos(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.0 cross-target gate requires zig cc")
	}
	dir := t.TempDir()
	binPath := filepath.Join(dir, "answer.x86_64-macos")
	_, err := Build(crossProgram(), Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     "x86_64-macos",
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if got := machoCPUType(t, binPath); got != machoCPUTypeX86_64 {
		t.Errorf("cputype = 0x%X, want 0x%X (x86_64)", got, machoCPUTypeX86_64)
	}
	assertCrossOutput(t, "x86_64-macos", binPath)
}

// TestBuildCrossWasm32WASI is the Phase 5.0 gate for the §9
// wasm32 row: the produced file starts with the Wasm 1.0 magic.
func TestBuildCrossWasm32WASI(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.0 cross-target gate requires zig cc")
	}
	dir := t.TempDir()
	binPath := filepath.Join(dir, "answer.wasm")
	_, err := Build(crossProgram(), Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     "wasm32-wasi",
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if !isWasm(t, binPath) {
		t.Errorf("file %s is not a Wasm 1.0 module", binPath)
	}
	assertCrossOutput(t, "wasm32-wasi", binPath)
}
