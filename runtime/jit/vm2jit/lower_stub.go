//go:build !arm64

package vm2jit

import "mochi/runtime/vm2"

func lowerFnARM64(_ *vm2.Function) ([]uint32, error) {
	return nil, ErrUnsupported
}
