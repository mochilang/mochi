package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase12WasmWasi is the MEP-45 Phase 12.0 gate. It compiles the
// primitives/add_ints fixture for wasm32-wasi using zig cc (which ships
// wasi-libc and the wasm-ld linker) and, when wasmtime is available on PATH,
// runs the produced .wasm binary and asserts byte-equal output.
//
// Gated on MOCHI_TEST_ZIG_DOWNLOAD=1 to avoid network access in the default
// test run. The CI workflow sets this unconditionally.
//
// wasm32-wasi notes:
//   - zig cc ships wasi-libc so no separate wasi-sdk is needed (Phase 12.0
//     scope explicitly defers wasi-sdk vendoring in favour of the zig path).
//   - The mochi runtime (setjmp/malloc/printf/etc.) is fully supported by
//     wasi-libc; no GC-specific changes are required.
//   - -Wl,-no_uuid and -fsanitize flags are skipped for wasm targets by
//     the driver (see isWasm guard in build/driver.go).
func TestPhase12WasmWasi(t *testing.T) {
	if os.Getenv("MOCHI_TEST_ZIG_DOWNLOAD") != "1" {
		t.Skip("set MOCHI_TEST_ZIG_DOWNLOAD=1 to enable WASM/WASI gate")
	}

	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "primitives", "add_ints", "add_ints.mochi")
	want := []byte("3\n")

	outBin := filepath.Join(t.TempDir(), "add_ints_wasm")
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if err := d.Build(src, outBin, "wasm32-wasi", ""); err != nil {
		t.Fatalf("Driver.Build(wasm32-wasi): %v", err)
	}

	wasmtime, wasmtimeErr := exec.LookPath("wasmtime")
	if wasmtimeErr != nil {
		t.Logf("wasmtime not found on PATH; compile-only check passed (no run gate)")
		return
	}

	cmd := exec.Command(wasmtime, "run", outBin)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("wasmtime run: %v", err)
	}
	if !bytes.Equal(stdout.Bytes(), want) {
		t.Fatalf("output mismatch: want %q got %q", want, stdout.Bytes())
	}
}
