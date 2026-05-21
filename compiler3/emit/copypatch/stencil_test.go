package copypatch

import (
	"testing"
)

// TestRelocKindString covers the diagnostic-string branches. The
// branches are tiny but every Stencil error message routes through
// them, so a regression here would corrupt every other error message
// in the package.
func TestRelocKindString(t *testing.T) {
	cases := []struct {
		k    RelocKind
		want string
	}{
		{RelocInvalid, "invalid"},
		{RelocImm32, "imm32"},
		{RelocImm64, "imm64"},
		{RelocPCRel32, "pcrel32"},
		{RelocAbs64, "abs64"},
		{RelocKind(99), "?"},
	}
	for _, c := range cases {
		if got := c.k.String(); got != c.want {
			t.Errorf("RelocKind(%d).String() = %q, want %q", c.k, got, c.want)
		}
	}
}

// TestSymbolIDString covers every named SymbolID plus the fallback.
// Same rationale as TestRelocKindString.
func TestSymbolIDString(t *testing.T) {
	cases := []struct {
		s    SymbolID
		want string
	}{
		{SymInvalid, "invalid"},
		{SymArenaBase, "arena_base"},
		{SymFrame, "frame"},
		{SymVMCtx, "vm_ctx"},
		{SymSlowPathDeref, "slow_path_deref"},
		{SymSlowPathDeopt, "slow_path_deopt"},
		{SymOpRetTarget, "op_ret_target"},
		{SymbolID(99), "?"},
	}
	for _, c := range cases {
		if got := c.s.String(); got != c.want {
			t.Errorf("SymbolID(%d).String() = %q, want %q", c.s, got, c.want)
		}
	}
}

// TestRelocWidth checks the per-kind width table the validator and
// patcher both consult. A drift between this table and the encoder
// would let an out-of-bounds write through.
func TestRelocWidth(t *testing.T) {
	cases := []struct {
		k    RelocKind
		want uint32
	}{
		{RelocImm32, 4},
		{RelocImm64, 8},
		{RelocPCRel32, 4},
		{RelocAbs64, 8},
		{RelocInvalid, 0},
	}
	for _, c := range cases {
		if got := relocWidth(c.k); got != c.want {
			t.Errorf("relocWidth(%s) = %d, want %d", c.k, got, c.want)
		}
	}
}

// TestStencilValidate covers the validator's negative space: empty
// Bytes, RelocInvalid kind, SymInvalid symbol, out-of-range offset,
// and overlapping relocs. Each case must produce a non-nil error
// naming the failing field.
func TestStencilValidate(t *testing.T) {
	good := Stencil{
		Op:    0, // OpInvalid is the ret stencil's tag in the amd64 table
		Bytes: []byte{0xC3},
	}
	if err := good.validate(); err != nil {
		t.Fatalf("unexpected error on valid stencil: %v", err)
	}

	cases := []struct {
		name string
		s    Stencil
		want string
	}{
		{
			name: "empty bytes",
			s:    Stencil{Bytes: nil},
			want: "empty Bytes",
		},
		{
			name: "invalid kind",
			s: Stencil{
				Bytes:  []byte{0, 0, 0, 0},
				Relocs: []RelocSite{{Kind: RelocInvalid, Symbol: SymArenaBase}},
			},
			want: "RelocInvalid kind",
		},
		{
			name: "invalid symbol",
			s: Stencil{
				Bytes:  []byte{0, 0, 0, 0},
				Relocs: []RelocSite{{Kind: RelocImm32, Symbol: SymInvalid}},
			},
			want: "SymInvalid symbol",
		},
		{
			name: "out of range",
			s: Stencil{
				Bytes:  []byte{0, 0},
				Relocs: []RelocSite{{Kind: RelocImm32, Symbol: SymArenaBase, Offset: 0}},
			},
			want: "extends past Bytes len",
		},
		{
			name: "overlap",
			s: Stencil{
				Bytes: make([]byte, 16),
				Relocs: []RelocSite{
					{Kind: RelocImm32, Symbol: SymArenaBase, Offset: 0},
					{Kind: RelocImm32, Symbol: SymArenaBase, Offset: 2},
				},
			},
			want: "overlap",
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			err := c.s.validate()
			if err == nil {
				t.Fatalf("expected error mentioning %q, got nil", c.want)
			}
			if !contains(err.Error(), c.want) {
				t.Errorf("error %q does not contain %q", err.Error(), c.want)
			}
		})
	}
}

// TestStencilsHostValid runs every host stencil through validate() so
// a regression in the hand-written table (off-by-one offset,
// accidental overlap, RelocInvalid leak) is caught at package-test
// time, not at runtime. The test is arch-portable: it consults
// archStencils() so amd64, arm64, and the empty fallback are all
// covered without per-arch duplication.
func TestStencilsHostValid(t *testing.T) {
	tab := archStencils()
	if tab == nil {
		t.Skip("no stencil table for this GOARCH (phase 1 target matrix lists 5 hosts)")
	}
	for op, s := range tab {
		if err := s.validate(); err != nil {
			t.Errorf("stencil %s invalid: %v", op, err)
		}
	}
}

// TestSymbolTableSetGet covers the table accessors. The implementation
// is two array writes, but the bounds check is load-bearing: a
// stencilgen bug that emits an out-of-range SymbolID must panic, not
// silently corrupt adjacent storage.
func TestSymbolTableSetGet(t *testing.T) {
	var st SymbolTable
	st.Set(SymArenaBase, 0xDEAD_BEEF)
	if got := st.Get(SymArenaBase); got != 0xDEAD_BEEF {
		t.Errorf("Get(SymArenaBase) = 0x%x, want 0xDEADBEEF", got)
	}
	// Out-of-range Get returns zero, not panic. This is the read path;
	// only the write path panics (write is the stencilgen-bug case).
	if got := st.Get(SymbolID(len(st) + 10)); got != 0 {
		t.Errorf("Get(out-of-range) = 0x%x, want 0", got)
	}
	// Out-of-range Set panics.
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("Set(out-of-range) did not panic")
		}
	}()
	st.Set(SymbolID(len(st)+10), 0)
}

// contains is a tiny substring helper so the test file does not pull
// in "strings" for one call. Matches strings.Contains.
func contains(s, sub string) bool {
	if len(sub) == 0 {
		return true
	}
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub {
			return true
		}
	}
	return false
}
