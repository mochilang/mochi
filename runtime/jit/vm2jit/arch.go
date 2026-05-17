package vm2jit

// Arch selects the native code backend.
type Arch int

const (
	ARM64 Arch = iota // darwin/arm64 (primary)
	AMD64             // linux/amd64 (parity gate)
)

// HostArch returns the Arch for the current build target.
// Callers use this when no explicit arch is specified.
func HostArch() Arch {
	return hostArch // set by arch-specific build tag file
}
