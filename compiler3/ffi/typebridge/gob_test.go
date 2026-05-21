package typebridge

import (
	"bytes"
	"encoding/gob"
	"testing"
)

// TestGobRoundTrip encodes and decodes a battery of Type values
// through gob and asserts deep equality. This is the property the
// Phase 2 cache layer relies on.
func TestGobRoundTrip(t *testing.T) {
	samples := []Type{
		{Kind: KindBool},
		{Kind: KindInt},
		{Kind: KindInt, Width: 32},
		{Kind: KindString},
		{Kind: KindBytes},
		{Kind: KindList, Elem: &Type{Kind: KindString}},
		{Kind: KindArray, ArrayLen: 4, Elem: &Type{Kind: KindFloat, Width: 32}},
		{Kind: KindMap, Key: &Type{Kind: KindString}, Elem: &Type{Kind: KindList, Elem: &Type{Kind: KindInt}}},
		{Kind: KindRef, Elem: &Type{Kind: KindNamed, Name: "File", PkgPath: "os"}},
		{Kind: KindIface, Name: "Reader", PkgPath: "io", Methods: []Method{
			{Name: "Read", Exported: true, Signature: Type{Kind: KindFunc, Params: []Type{{Kind: KindBytes}}, Results: []Type{{Kind: KindInt}, {Kind: KindIface, Name: "error"}}}},
		}},
		{Kind: KindFunc, Params: []Type{{Kind: KindList, Elem: &Type{Kind: KindInt}}}, Variadic: true},
		{Kind: KindChan, ChanDir: ChanRecv, Elem: &Type{Kind: KindInt}},
		{Kind: KindOpaque, OpaqueReason: OpaqueUnsafePointer, GoType: "unsafe.Pointer"},
		{Kind: KindStruct, Fields: []Field{
			{Name: "Name", Type: Type{Kind: KindString}, Tag: `json:"name"`, Exported: true},
			{Name: "Age", Type: Type{Kind: KindInt}, Exported: true},
		}},
		{Kind: KindNamed, Name: "Pair", PkgPath: "syn", TypeArgs: []Type{{Kind: KindString}, {Kind: KindInt}}},
	}
	for i, s := range samples {
		var buf bytes.Buffer
		if err := gob.NewEncoder(&buf).Encode(s); err != nil {
			t.Fatalf("[%d] encode: %v", i, err)
		}
		var got Type
		if err := gob.NewDecoder(&buf).Decode(&got); err != nil {
			t.Fatalf("[%d] decode: %v", i, err)
		}
		if !s.Equal(got) {
			t.Errorf("[%d] not equal after round-trip:\n  in : %+v\n  out: %+v", i, s, got)
		}
	}
}

// TestGobVersionMismatch confirms the decoder refuses an unknown
// version byte.
func TestGobVersionMismatch(t *testing.T) {
	var typ Type
	err := typ.GobDecode([]byte{0xFF, 0x00, 0x00})
	if err == nil {
		t.Fatal("expected version-mismatch error")
	}
}

// TestGobEmpty confirms the decoder refuses empty input.
func TestGobEmpty(t *testing.T) {
	var typ Type
	if err := typ.GobDecode(nil); err == nil {
		t.Fatal("expected empty-payload error")
	}
}

// TestGobVersionByte locks in the current wire-format version. A
// bump must be paired with cache invalidation in the Phase 2
// resolver.
func TestGobVersionByte(t *testing.T) {
	data, err := Type{Kind: KindBool}.GobEncode()
	if err != nil {
		t.Fatal(err)
	}
	if len(data) == 0 || data[0] != gobVersion {
		t.Fatalf("first byte = 0x%02x, want 0x%02x", data[0], gobVersion)
	}
}
