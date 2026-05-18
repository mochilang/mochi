//go:build !amd64

package vm3jit

import "mochi/runtime/vm3"

func lowerAMD64(*vm3.Function, Options) ([]byte, error) { return nil, ErrUnsupported }
