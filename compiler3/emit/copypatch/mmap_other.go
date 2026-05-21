//go:build !(linux && amd64)

package copypatch

import "fmt"

// NewLinuxAMD64Mapping on non-(linux/amd64) returns an error. The
// Phase 1 scope is x86_64 Linux only (MEP-42 §9 row 1); darwin/amd64
// and linux/arm64 get their own platform-specific files in Phase 1.5.
func NewLinuxAMD64Mapping(size int) (rw, rx []byte, err error) {
	return nil, nil, fmt.Errorf("copypatch: dual-mapping unsupported on this GOOS/GOARCH (phase 1 ships linux/amd64 only)")
}

// ReleaseLinuxAMD64Mapping is a no-op on non-(linux/amd64). The
// signature is preserved so callers can write platform-agnostic
// teardown code that compiles on every host.
func ReleaseLinuxAMD64Mapping(rw, rx []byte) error {
	return nil
}
