//go:build !(linux && arm64)

package copypatch

import "fmt"

// NewLinuxARM64Mapping on non-(linux/arm64) returns an error. Phase
// 1.1 ships the linux/arm64 spelling of the dual-mapping; macOS arm64
// uses the `MAP_JIT` + `pthread_jit_write_protect_np` path
// (mmap_darwin_arm64.go, Phase 1.3). Callers must consult the
// platform-appropriate constructor; this stub exists so callers can
// link platform-agnostic teardown code without per-host build tags.
func NewLinuxARM64Mapping(size int) (rw, rx []byte, err error) {
	return nil, nil, fmt.Errorf("copypatch: NewLinuxARM64Mapping unsupported on this GOOS/GOARCH (phase 1.1 ships linux/arm64)")
}

// ReleaseLinuxARM64Mapping is a no-op on non-(linux/arm64). The
// signature is preserved so callers can write platform-agnostic
// teardown code that compiles on every host.
func ReleaseLinuxARM64Mapping(rw, rx []byte) error {
	return nil
}
