package wasm_test

import (
	"context"
	"testing"

	"mochi/package3/swift/wasm"
)

// TestPhase17Wasm is the MEP-69 Phase 17 gate for SwiftWasm target.
// Skips if swiftwasm toolchain is not available.
func TestPhase17Wasm(t *testing.T) {
	t.Run("EmptyPackageDir", func(t *testing.T) {
		_, err := wasm.Build(context.Background(), wasm.BuildSpec{})
		if err == nil {
			t.Error("expected error for empty PackageDir")
		}
	})

	t.Run("NonExistentPackageDir", func(t *testing.T) {
		_, err := wasm.Build(context.Background(), wasm.BuildSpec{
			PackageDir: "/nonexistent/swift/package",
		})
		if err == nil {
			t.Error("expected error for non-existent PackageDir")
		}
	})

	t.Run("WasmTarget", func(t *testing.T) {
		if wasm.WasmTarget != "wasm32-unknown-wasi" {
			t.Errorf("WasmTarget: got %q, want wasm32-unknown-wasi", wasm.WasmTarget)
		}
	})
}
