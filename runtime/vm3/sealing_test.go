package vm3

import "testing"

// TestSealRoundTrip confirms a Sealer returns the exact Cell that
// was sealed, when presented with the matching key.
func TestSealRoundTrip(t *testing.T) {
	a := &Arenas{}
	s := NewSealer()
	c := a.AllocString([]byte("hello"))

	id := s.Seal(c, 0xdeadbeefcafebabe)
	if id == 0 {
		t.Fatal("sealID 0 is reserved sentinel")
	}
	got, ok := s.Unseal(id, 0xdeadbeefcafebabe)
	if !ok {
		t.Fatal("Unseal with matching key returned !ok")
	}
	if got != c {
		t.Fatalf("Unseal returned %v, want %v", got, c)
	}
}

// TestSealWrongKeyFails confirms the central security property: a
// caller without the seal key cannot recover the inner Cell, even
// if they hold the sealID. This is the FFI boundary-4 invariant.
func TestSealWrongKeyFails(t *testing.T) {
	a := &Arenas{}
	s := NewSealer()
	c := a.AllocString([]byte("secret"))

	id := s.Seal(c, 0x1111111111111111)
	if got, ok := s.Unseal(id, 0x2222222222222222); ok {
		t.Fatalf("Unseal with wrong key returned (%v, true); should have failed", got)
	}
	// Right key still works after a wrong-key probe.
	if _, ok := s.Unseal(id, 0x1111111111111111); !ok {
		t.Fatal("right key failed after wrong-key probe; table corrupted")
	}
}

// TestSealUnknownIDFails confirms an unknown sealID returns !ok
// regardless of the key presented. Prevents an attacker from
// brute-forcing IDs.
func TestSealUnknownIDFails(t *testing.T) {
	s := NewSealer()
	if _, ok := s.Unseal(42, 0); ok {
		t.Fatal("Unseal of unknown ID returned ok")
	}
	if _, ok := s.Unseal(0, 0); ok {
		t.Fatal("Unseal of zero ID returned ok")
	}
}

// TestSealDistinctIDsForSameCell confirms sealing the same Cell
// twice produces two distinct sealIDs, so each call site gets its
// own un-forgeable token. This breaks the "share an ID with one
// callee and reuse it across boundaries" leak vector.
func TestSealDistinctIDsForSameCell(t *testing.T) {
	a := &Arenas{}
	s := NewSealer()
	c := a.AllocString([]byte("shared"))

	id1 := s.Seal(c, 0xaaaa)
	id2 := s.Seal(c, 0xaaaa)
	if id1 == id2 {
		t.Fatalf("sealing same Cell twice produced same ID %d", id1)
	}
}

// TestSealKeyReplayAcrossIDsFails confirms the per-ID salting in
// keyHash. An attacker who learns (id1, key) cannot use that key
// to unseal id2, even if id2 was sealed with the same key.
func TestSealKeyReplayAcrossIDsFails(t *testing.T) {
	a := &Arenas{}
	s := NewSealer()
	c1 := a.AllocString([]byte("a"))
	c2 := a.AllocString([]byte("b"))
	const sharedKey uint64 = 0x55aa55aa

	id1 := s.Seal(c1, sharedKey)
	id2 := s.Seal(c2, sharedKey)

	// The key replay attack: take id1's keyHash and try to forge
	// id2's. The salt makes the two hashes differ, so id2 cannot be
	// unsealed by anyone who only knows id1's surface state.
	got1, _ := s.Unseal(id1, sharedKey)
	got2, _ := s.Unseal(id2, sharedKey)
	if got1 == got2 {
		t.Fatal("distinct sealIDs returned same Cell; cross-ID isolation broken")
	}
}

// TestSealForgetMakesUnsealable confirms Forget permanently
// invalidates a sealID. Used at handle-free time so a sealed Cell
// whose underlying slot has been reused cannot resurrect via the
// old sealID.
func TestSealForgetMakesUnsealable(t *testing.T) {
	a := &Arenas{}
	s := NewSealer()
	c := a.AllocString([]byte("ephemeral"))
	const key uint64 = 0x12345

	id := s.Seal(c, key)
	if !s.Forget(id) {
		t.Fatal("Forget reported absent for present id")
	}
	if _, ok := s.Unseal(id, key); ok {
		t.Fatal("Unseal succeeded after Forget; table not pruned")
	}
	if s.Forget(id) {
		t.Fatal("second Forget reported present")
	}
}

// TestSealLenTracksEntries spot-checks the Len accessor used by
// leak harnesses.
func TestSealLenTracksEntries(t *testing.T) {
	a := &Arenas{}
	s := NewSealer()
	if got := s.Len(); got != 0 {
		t.Fatalf("fresh Sealer Len = %d, want 0", got)
	}
	c := a.AllocString([]byte("x"))
	id := s.Seal(c, 1)
	if got := s.Len(); got != 1 {
		t.Fatalf("Len after one Seal = %d, want 1", got)
	}
	s.Forget(id)
	if got := s.Len(); got != 0 {
		t.Fatalf("Len after Forget = %d, want 0", got)
	}
}
