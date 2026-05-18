//go:build !arm64

package vm3jit

import "mochi/runtime/vm3"

func lowerARM64(*vm3.Function, Options) ([]uint32, error) { return nil, ErrUnsupported }
