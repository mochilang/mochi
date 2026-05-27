package build

import "testing"

// runJvmFixture compiles mochiPath to an uberjar, runs it, and checks stdout
// against the content of wantFile byte-for-byte.
// Stub in Phase 0 -- implemented in Phase 1.
func runJvmFixture(t *testing.T, _ /* mochiPath */, _ /* wantFile */ string) {
	t.Helper()
	t.Skip("runJvmFixture not yet implemented (Phase 0 skeleton)")
}
