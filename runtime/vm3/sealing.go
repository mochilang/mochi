package vm3

// Sealing is the runtime-side counterpart to MEP-43 Phase 10's
// ffi.Seal[T] / ffi.Unseal[T] type-system markers. The Go transpiler
// target uses the FFI helpers because Go's type system already
// prevents an arbitrary callee from forging a handle from an integer.
// The bytecode interpreter has no such floor: any Cell value an
// untrusted IR can produce could in principle be re-typed as a handle
// by an attacker that found a verifier hole. The Sealer below closes
// that gap by storing the original Cell in a per-VM table and
// returning a fresh opaque sealID. An unsealer must present both the
// sealID *and* the correct key to recover the original Cell.
//
// Threat model:
//
//   - Boundary 4 (FFI): see docs/security/threat-model.md §4.4. A Mochi
//     callee that exposes a Cell across an `effect meta` boundary
//     hands the foreign code a sealID, not the underlying handle.
//     The foreign code cannot dereference the sealID as a Cell
//     (it lives in a different value space) and cannot guess the key.
//
//   - Generation opacity (rule class C, MEP-41 §6.4): the sealID is
//     monotonically increasing and bears no relation to the inner
//     Cell's gen/idx fields, so observing a sealID leaks nothing
//     about its handle's slot or generation.
//
// Wire-up: each VM owns one Sealer. The interpreter's OpSeal/OpUnseal
// opcodes (deferred to Phase 3.2, see MEP-41 §8) call into this table.
// Until those opcodes land, the Sealer is used by Go FFI wrappers
// that need a stronger floor than the identity Seal[T]/Unseal[T] of
// runtime/mochi/ffi/seal.go.
type Sealer struct {
	next    uint64
	entries map[uint64]sealedEntry
}

// sealedEntry pairs a stored Cell with a salted hash of the seal key.
// The salt is the sealID itself, so the same key across distinct
// sealIDs produces distinct hashes; this prevents an attacker who
// learns one (sealID, key) pair from impersonating another seal
// merely by replaying the key.
type sealedEntry struct {
	cell    Cell
	keyHash uint64
}

// NewSealer returns an empty Sealer. A VM's sealing table is owned
// by the VM and not shared across VMs; this matches the per-VM
// threat model (one verifier per VM, MEP-41 §5).
func NewSealer() *Sealer {
	return &Sealer{entries: make(map[uint64]sealedEntry)}
}

// Seal stores c under a fresh sealID and returns that ID. The same
// key may be presented to Unseal to recover c. Sealing the same Cell
// twice produces two distinct sealIDs.
func (s *Sealer) Seal(c Cell, key uint64) uint64 {
	s.next++
	id := s.next
	s.entries[id] = sealedEntry{cell: c, keyHash: keyHash(key, id)}
	return id
}

// Unseal returns the original Cell stored under id, but only if the
// presented key matches the one that sealed it. A wrong key or an
// unknown id returns (zero Cell, false).
func (s *Sealer) Unseal(id, key uint64) (Cell, bool) {
	e, ok := s.entries[id]
	if !ok {
		return 0, false
	}
	if e.keyHash != keyHash(key, id) {
		return 0, false
	}
	return e.cell, true
}

// Forget removes id from the table regardless of key. Used when a
// sealed handle's Cell is freed; the sealID becomes permanently
// unsealable. Returns true if the id was present.
func (s *Sealer) Forget(id uint64) bool {
	if _, ok := s.entries[id]; !ok {
		return false
	}
	delete(s.entries, id)
	return true
}

// Len reports how many active seals are held. Useful for leak checks
// in long-running tests (a VM that seals N handles and unseals N
// without Forget will retain N entries until shutdown).
func (s *Sealer) Len() int {
	return len(s.entries)
}

// keyHash mixes key with sealID via the classic SplitMix64 finisher.
// The mixer is non-cryptographic but sufficient for the threat model
// here: an attacker who does not hold the key has no shortcut to
// produce the same hash for the same (id, _) pair, and the salt
// prevents trivial key replay across IDs. The constants are SplitMix64's
// well-known mixing constants and are not load-bearing — any avalanche
// hash with adequate diffusion would serve.
func keyHash(key, id uint64) uint64 {
	x := key ^ id
	x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
	x = (x ^ (x >> 27)) * 0x94d049bb133111eb
	x = x ^ (x >> 31)
	return x
}
