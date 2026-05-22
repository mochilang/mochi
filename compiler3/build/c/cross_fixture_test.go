package cbuild

import (
	"os"
	"path/filepath"
	"testing"
)

// Phase 5.2 cross-fixture suite. Phase 5.0 proved each §9 triple can
// produce a binary of the right format from a synthesized one-function
// Program; Phase 5.2 raises the bar to "a real BG bench fixture (the
// §10.7 user-facing surface) cross-builds to each triple and, when a
// foreign-arch runner is available, byte-matches the host gate's
// stdout". This is the load-bearing gate that flips the Phase 5
// umbrella row to LANDED: the user-facing AOT capability (compile a
// real Mochi program, run it on the foreign arch) is now wired for
// every must-have triple in §9.
//
// The fixture is regex_redux: ~30 lines of int arithmetic, a `for`
// loop and two `if` branches, output is the bare integer `69`. We
// pick this fixture for three reasons:
//
//   1. Output is one line of bare ASCII (no JSON wrapper, no duration
//      field), so the run-gate can byte-match exactly.
//   2. It exercises the same op surface (let, var, for, if, %, +, *,
//      ==, print int) that every §9 triple's libc must support. No
//      hash maps, no heap, no syscalls beyond stdout: if this passes
//      cross, every smaller scalar fixture passes too.
//   3. Phase 4.3.x already pinned its host-target output ("69\n");
//      Phase 5.2 reuses that ground truth verbatim, so a regression in
//      the cross-build is unambiguously a cross-target bug, not a
//      fixture drift.

// regexReduxExpect is the canonical regex_redux stdout, pinned by
// TestBuildSourceRegexReduxBgFixture on the host gate.
const regexReduxExpect = "69\n"

// buildFixtureCross writes the regex_redux fixture into dir, cross-
// builds it for triple, and returns the binary path. Fails the test
// on any error from the parse/lower/emit/cc pipeline.
func buildFixtureCross(t *testing.T, triple, suffix string) string {
	t.Helper()
	src := readBenchFixture(t, "regex_redux", 0)
	dir := t.TempDir()
	srcPath := filepath.Join(dir, "regex_redux.mochi")
	if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	binPath := filepath.Join(dir, "regex_redux"+suffix)
	if _, err := BuildSource(srcPath, Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     triple,
	}); err != nil {
		t.Fatalf("BuildSource(triple=%s): %v", triple, err)
	}
	return binPath
}

// assertCrossFixtureOutput runs binPath through the runner for triple
// (if available) and asserts stdout matches the host gate's output.
// When no runner is on PATH, logs a skip hint (the build-gate has
// already fired by the time this is called). Reuses the Phase 5.0
// findRunner+runBinary framework so every new triple's runner lands
// in one place.
func assertCrossFixtureOutput(t *testing.T, triple, binPath string) {
	t.Helper()
	r, ok := findRunner(triple)
	if !ok {
		t.Logf("no runner for %s on PATH; skipping run-gate (install qemu / wasmtime to enable)", triple)
		return
	}
	got := runBinary(t, r, binPath)
	if got != regexReduxExpect {
		t.Errorf("cross-run regex_redux on %s via %s: stdout = %q, want %q",
			triple, r.cmd, got, regexReduxExpect)
	}
}

// TestBuildSourceRegexReduxBgFixtureCrossX86_64Linux is the Phase 5.2
// gate for the §9 x86_64 Linux row applied to the §10.7 BG surface:
// the unmodified regex_redux fixture cross-builds to an x86_64 ELF
// and, under a qemu-x86_64 runner (or natively on linux/amd64),
// prints "69\n".
func TestBuildSourceRegexReduxBgFixtureCrossX86_64Linux(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.2 cross-fixture gate requires zig cc")
	}
	bin := buildFixtureCross(t, "x86_64-linux-musl", ".x86_64-linux")
	if got := elfMachine(t, bin); got != elfMachineX86_64 {
		t.Errorf("e_machine = 0x%X, want 0x%X (x86_64)", got, elfMachineX86_64)
	}
	assertCrossFixtureOutput(t, "x86_64-linux-musl", bin)
}

// TestBuildSourceRegexReduxBgFixtureCrossAArch64Linux is the Phase 5.2
// gate for the §9 aarch64 Linux row applied to the §10.7 BG surface.
func TestBuildSourceRegexReduxBgFixtureCrossAArch64Linux(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.2 cross-fixture gate requires zig cc")
	}
	bin := buildFixtureCross(t, "aarch64-linux-musl", ".aarch64-linux")
	if got := elfMachine(t, bin); got != elfMachineAArch64 {
		t.Errorf("e_machine = 0x%X, want 0x%X (aarch64)", got, elfMachineAArch64)
	}
	assertCrossFixtureOutput(t, "aarch64-linux-musl", bin)
}

// TestBuildSourceRegexReduxBgFixtureCrossAArch64Macos is the Phase 5.2
// gate for the §9 aarch64 macOS row applied to the §10.7 BG surface.
// On an Apple Silicon recording host the run-gate fires natively; on
// non-Darwin hosts it skips and the build-gate still fires.
func TestBuildSourceRegexReduxBgFixtureCrossAArch64Macos(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.2 cross-fixture gate requires zig cc")
	}
	bin := buildFixtureCross(t, "aarch64-macos", ".aarch64-macos")
	if got := machoCPUType(t, bin); got != machoCPUTypeArm64 {
		t.Errorf("cputype = 0x%X, want 0x%X (arm64)", got, machoCPUTypeArm64)
	}
	assertCrossFixtureOutput(t, "aarch64-macos", bin)
}

// TestBuildSourceRegexReduxBgFixtureCrossX86_64Macos is the Phase 5.2
// gate for the §9 x86_64 macOS row applied to the §10.7 BG surface.
// On an Apple Silicon host Rosetta 2 runs the binary transparently;
// on x86_64 Darwin it runs natively.
func TestBuildSourceRegexReduxBgFixtureCrossX86_64Macos(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.2 cross-fixture gate requires zig cc")
	}
	bin := buildFixtureCross(t, "x86_64-macos", ".x86_64-macos")
	if got := machoCPUType(t, bin); got != machoCPUTypeX86_64 {
		t.Errorf("cputype = 0x%X, want 0x%X (x86_64)", got, machoCPUTypeX86_64)
	}
	assertCrossFixtureOutput(t, "x86_64-macos", bin)
}

// TestBuildSourceRegexReduxBgFixtureCrossWasm32WASI is the Phase 5.2
// gate for the §9 wasm32-wasi row applied to the §10.7 BG surface:
// the unmodified regex_redux fixture cross-builds to a Wasm module
// and runs under wasmtime / wasmer / wasm3 (whichever is on PATH),
// printing "69\n".
func TestBuildSourceRegexReduxBgFixtureCrossWasm32WASI(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.2 cross-fixture gate requires zig cc")
	}
	bin := buildFixtureCross(t, "wasm32-wasi", ".wasm")
	if !isWasm(t, bin) {
		t.Errorf("file %s is not a Wasm 1.0 module", bin)
	}
	assertCrossFixtureOutput(t, "wasm32-wasi", bin)
}

// reverseComplementExpect is the canonical reverse_complement stdout,
// pinned by TestBuildSourceReverseComplementBgFixture on the host gate.
// (N=4096, (N/4)*287 = 293888.)
const reverseComplementExpect = "293888\n"

// TestBuildSourceReverseComplementBgFixtureCrossAArch64Macos is a
// second Phase 5.2 fixture sample on the recording host. The fixture
// allocates an N-byte buffer, fills it via the same LCG used by
// fasta, then reverses and complements in place. The buffer is heap-
// allocated through the C runtime's list<int> path (OpNewListI64 +
// OpListI64Push + OpListI64Get), so the cross-run validates that the
// runtime's malloc/grow path works under each triple's libc. We add
// it only on aarch64-macos (the host's native arch) so the run-gate
// fires without an emulator; the analogous gates for the other triples
// follow as the matrix expands.
func TestBuildSourceReverseComplementBgFixtureCrossAArch64Macos(t *testing.T) {
	if !hasZig() {
		t.Skip("zig not on PATH; Phase 5.2 cross-fixture gate requires zig cc")
	}
	src := readBenchFixture(t, "reverse_complement", 0)
	dir := t.TempDir()
	srcPath := filepath.Join(dir, "reverse_complement.mochi")
	if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	binPath := filepath.Join(dir, "reverse_complement.aarch64-macos")
	if _, err := BuildSource(srcPath, Options{
		OutDir:     dir,
		BinaryPath: binPath,
		Triple:     "aarch64-macos",
	}); err != nil {
		t.Fatalf("BuildSource: %v", err)
	}
	if got := machoCPUType(t, binPath); got != machoCPUTypeArm64 {
		t.Errorf("cputype = 0x%X, want 0x%X (arm64)", got, machoCPUTypeArm64)
	}
	r, ok := findRunner("aarch64-macos")
	if !ok {
		t.Logf("no runner for aarch64-macos on PATH; skipping run-gate")
		return
	}
	got := runBinary(t, r, binPath)
	if got != reverseComplementExpect {
		t.Errorf("cross-run reverse_complement on aarch64-macos via %s: stdout = %q, want %q",
			r.cmd, got, reverseComplementExpect)
	}
}

