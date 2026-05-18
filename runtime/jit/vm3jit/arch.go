package vm3jit

// Arch identifies a JIT backend target. vm3jit Phase 6.0 only supports
// ArchARM64; the value space is shared with vm2jit so future code that
// routes both JITs can use the same constants.
type Arch int8

const (
	ArchUnsupported Arch = -1
	ArchARM64       Arch = 0
	ArchAMD64       Arch = 1
)
