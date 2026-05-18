//go:build !arm64

package vm2jit

import "mochi/runtime/vm2"

func lowerFnARM64(_ *vm2.Function) ([]uint32, error) {
	return nil, ErrUnsupported
}

// jitProfitable is the !arm64 stub. The arm64 backend has the deopt
// machinery that this gate guards against; on other archs there is
// no real JIT to be unprofitable, so the answer is trivially true.
func jitProfitable(_ *vm2.Function) bool { return true }
