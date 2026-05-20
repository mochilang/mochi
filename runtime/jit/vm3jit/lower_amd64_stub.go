//go:build !amd64

package vm3jit

import "mochi/runtime/vm3"

func lowerAMD64(*vm3.Function, Options) ([]byte, error) { return nil, ErrUnsupported }

// usesR12ForF64AMD64 and mapKernelOperandClobberAMD64 are referenced by
// checkCellBankAdmissibleAMD64 in compile.go (a tagless file). The real
// definitions live in lower_amd64.go; these stubs let non-AMD64 builds
// link. checkCellBankAdmissibleAMD64 is only called when
// hostArch == ArchAMD64, so the stub bodies are unreachable at runtime
// on non-AMD64.
func usesR12ForF64AMD64(*vm3.Function) bool   { return false }
func mapKernelOperandClobberAMD64(vm3.Op) bool { return false }
