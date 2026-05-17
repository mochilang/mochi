//go:build !arm64 && !amd64

package vm2jit

const hostArch = Arch(-1) // unsupported; Compile returns ErrUnsupported
