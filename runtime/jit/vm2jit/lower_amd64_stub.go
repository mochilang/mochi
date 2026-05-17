//go:build !amd64

package vm2jit

import "mochi/runtime/vm2"

func lowerAMD64(_ *vm2.Function, _ int, _ vm2.Instr) ([]uint32, error) {
	return nil, ErrUnsupported
}
