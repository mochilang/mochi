package typebridge

import "testing"

// TestEqualSelf checks every shape compares equal to itself, and a
// targeted set of mutations is detected as inequal.
func TestEqualSelf(t *testing.T) {
	samples := []Type{
		{Kind: KindBool},
		{Kind: KindInt, Width: 32},
		{Kind: KindList, Elem: &Type{Kind: KindString}},
		{Kind: KindMap, Key: &Type{Kind: KindString}, Elem: &Type{Kind: KindInt}},
		{Kind: KindStruct, Fields: []Field{{Name: "A", Type: Type{Kind: KindInt}, Exported: true}}},
		{Kind: KindNamed, Name: "Reader", PkgPath: "strings"},
		{Kind: KindIface, Name: "Reader", PkgPath: "io", Methods: []Method{{Name: "Read", Exported: true, Signature: Type{Kind: KindFunc}}}},
		{Kind: KindFunc, Params: []Type{{Kind: KindInt}}, Results: []Type{{Kind: KindInt}}},
		{Kind: KindChan, ChanDir: ChanBoth, Elem: &Type{Kind: KindInt}},
		{Kind: KindOpaque, OpaqueReason: OpaqueUnsafePointer, GoType: "unsafe.Pointer"},
	}
	for _, s := range samples {
		if !s.Equal(s) {
			t.Errorf("not self-equal: %+v", s)
		}
	}
}

func TestEqualMutations(t *testing.T) {
	a := Type{Kind: KindList, Elem: &Type{Kind: KindInt}}
	b := Type{Kind: KindList, Elem: &Type{Kind: KindString}}
	if a.Equal(b) {
		t.Fatal("list<int> should not equal list<string>")
	}

	c := Type{Kind: KindInt, Width: 32}
	d := Type{Kind: KindInt, Width: 64}
	if c.Equal(d) {
		t.Fatal("int32 should not equal int64")
	}

	e := Type{Kind: KindNamed, Name: "Reader", PkgPath: "strings"}
	f := Type{Kind: KindNamed, Name: "Reader", PkgPath: "bytes"}
	if e.Equal(f) {
		t.Fatal("strings.Reader should not equal bytes.Reader")
	}

	g := Type{Kind: KindOpaque, OpaqueReason: OpaqueUnsafePointer, GoType: "unsafe.Pointer"}
	h := Type{Kind: KindOpaque, OpaqueReason: OpaqueUintptr, GoType: "uintptr"}
	if g.Equal(h) {
		t.Fatal("unsafe.Pointer should not equal uintptr opaque")
	}
}

func TestEqualNilElem(t *testing.T) {
	a := Type{Kind: KindList}
	b := Type{Kind: KindList, Elem: &Type{Kind: KindInt}}
	if a.Equal(b) {
		t.Fatal("nil Elem should not equal non-nil Elem")
	}
}
