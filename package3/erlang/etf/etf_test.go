package etf

import (
	"bytes"
	"compress/zlib"
	"errors"
	"math"
	"math/big"
	"testing"
)

// roundtrip encodes v and decodes it back, checking for equality.
func roundtrip(t *testing.T, v interface{}) interface{} {
	t.Helper()
	enc, err := Encode(v)
	if err != nil {
		t.Fatalf("Encode(%T %v): %v", v, v, err)
	}
	got, err := Decode(enc)
	if err != nil {
		t.Fatalf("Decode after Encode(%T %v): %v", v, v, err)
	}
	return got
}

func TestEncodeDecode_SmallInteger(t *testing.T) {
	for _, v := range []int64{0, 1, 127, 255} {
		got := roundtrip(t, v)
		if got != v {
			t.Errorf("roundtrip(%d) = %v, want %d", v, got, v)
		}
	}
}

func TestEncodeDecode_Integer(t *testing.T) {
	cases := []int64{256, 1000, -1, -128, -32768, 2147483647, -2147483648}
	for _, v := range cases {
		got := roundtrip(t, v)
		if got != v {
			t.Errorf("roundtrip(%d) = %v (%T), want %d", v, got, got, v)
		}
	}
}

func TestEncodeDecode_LargeInteger(t *testing.T) {
	// Values outside int32 range → SMALL_BIG_EXT on encode, decoded back as int64.
	cases := []int64{int64(math.MaxInt32) + 1, int64(math.MinInt32) - 1, math.MaxInt64}
	for _, v := range cases {
		got := roundtrip(t, v)
		if got != v {
			t.Errorf("roundtrip(%d) = %v (%T), want %d", v, got, got, v)
		}
	}
}

func TestEncodeDecode_Float(t *testing.T) {
	cases := []float64{0.0, 1.0, -1.0, 3.14159, math.Pi, math.E, math.MaxFloat64, math.SmallestNonzeroFloat64}
	for _, v := range cases {
		got := roundtrip(t, v)
		f, ok := got.(float64)
		if !ok {
			t.Errorf("roundtrip(%f) returned %T, want float64", v, got)
			continue
		}
		if math.IsNaN(v) {
			if !math.IsNaN(f) {
				t.Errorf("roundtrip(NaN) = %v", f)
			}
			continue
		}
		if f != v {
			t.Errorf("roundtrip(%v) = %v, want %v", v, f, v)
		}
	}
}

func TestEncodeDecode_Atom(t *testing.T) {
	cases := []Atom{"ok", "error", "true", "false", "undefined", "hackney", "cowboy", ""}
	for _, v := range cases {
		got := roundtrip(t, v)
		a, ok := got.(Atom)
		if !ok {
			t.Errorf("roundtrip(Atom(%q)) returned %T", string(v), got)
			continue
		}
		if a != v {
			t.Errorf("roundtrip(Atom(%q)) = Atom(%q), want Atom(%q)", string(v), string(a), string(v))
		}
	}
}

func TestEncodeDecode_AtomLong(t *testing.T) {
	// Atom with > 255 bytes should use ATOM_UTF8_EXT (not SMALL_ATOM_UTF8_EXT).
	long := Atom(bytes.Repeat([]byte("a"), 300))
	got := roundtrip(t, long)
	if got != long {
		t.Errorf("roundtrip(long atom) mismatch")
	}
}

func TestEncodeDecode_Binary(t *testing.T) {
	cases := [][]byte{
		nil,
		{},
		{0, 1, 2, 3},
		[]byte("hello, world"),
		bytes.Repeat([]byte{0xff}, 1000),
	}
	for _, v := range cases {
		enc, err := Encode(v)
		if err != nil {
			t.Fatalf("Encode([]byte): %v", err)
		}
		got, err := Decode(enc)
		if err != nil {
			t.Fatalf("Decode([]byte): %v", err)
		}
		b, ok := got.([]byte)
		if !ok {
			t.Errorf("roundtrip([]byte) returned %T", got)
			continue
		}
		if !bytes.Equal(b, v) {
			t.Errorf("roundtrip([]byte) mismatch (len %d vs %d)", len(b), len(v))
		}
	}
}

func TestEncodeDecode_NilAndEmptyList(t *testing.T) {
	// nil encodes as NIL_EXT, decodes as List(nil).
	enc, err := Encode(nil)
	if err != nil {
		t.Fatal(err)
	}
	got, err := Decode(enc)
	if err != nil {
		t.Fatal(err)
	}
	l, ok := got.(List)
	if !ok {
		t.Fatalf("Decode(nil) returned %T, want List", got)
	}
	if len(l) != 0 {
		t.Errorf("expected empty List, got %v", l)
	}

	// Empty List also encodes as NIL_EXT.
	enc2, err := Encode(List(nil))
	if err != nil {
		t.Fatal(err)
	}
	got2, err := Decode(enc2)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := got2.(List); !ok {
		t.Errorf("Decode(List{}) returned %T", got2)
	}
}

func TestEncodeDecode_List(t *testing.T) {
	list := List{int64(1), int64(2), int64(3)}
	got := roundtrip(t, list)
	l, ok := got.(List)
	if !ok {
		t.Fatalf("roundtrip(List) returned %T", got)
	}
	if len(l) != 3 {
		t.Fatalf("len = %d, want 3", len(l))
	}
	for i, want := range []int64{1, 2, 3} {
		if l[i] != want {
			t.Errorf("list[%d] = %v, want %d", i, l[i], want)
		}
	}
}

func TestEncodeDecode_Tuple(t *testing.T) {
	tuple := Tuple{Atom("ok"), int64(42)}
	got := roundtrip(t, tuple)
	tup, ok := got.(Tuple)
	if !ok {
		t.Fatalf("roundtrip(Tuple) returned %T", got)
	}
	if len(tup) != 2 {
		t.Fatalf("len = %d, want 2", len(tup))
	}
	if tup[0] != Atom("ok") {
		t.Errorf("tup[0] = %v, want ok", tup[0])
	}
	if tup[1] != int64(42) {
		t.Errorf("tup[1] = %v, want 42", tup[1])
	}
}

func TestEncodeDecode_NestedTuple(t *testing.T) {
	// {ok, {nested, 99}} — simulates a common Erlang return shape.
	inner := Tuple{Atom("nested"), int64(99)}
	outer := Tuple{Atom("ok"), inner}
	got := roundtrip(t, outer)
	tup, ok := got.(Tuple)
	if !ok {
		t.Fatalf("roundtrip nested tuple returned %T", got)
	}
	innerGot, ok := tup[1].(Tuple)
	if !ok {
		t.Fatalf("inner element is %T, want Tuple", tup[1])
	}
	if innerGot[0] != Atom("nested") || innerGot[1] != int64(99) {
		t.Errorf("inner tuple mismatch: %v", innerGot)
	}
}

func TestEncodeDecode_OkErrorPattern(t *testing.T) {
	// {ok, <<"hello">>} — the standard success branch.
	ok_ := Tuple{Atom("ok"), []byte("hello")}
	got := roundtrip(t, ok_)
	tup := got.(Tuple)
	if tup[0] != Atom("ok") {
		t.Errorf("{ok,...} tup[0] = %v", tup[0])
	}
	if !bytes.Equal(tup[1].([]byte), []byte("hello")) {
		t.Errorf("{ok,...} tup[1] mismatch")
	}

	// {error, <<"connection refused">>}
	errTup := Tuple{Atom("error"), []byte("connection refused")}
	got2 := roundtrip(t, errTup)
	tup2 := got2.(Tuple)
	if tup2[0] != Atom("error") {
		t.Errorf("{error,...} tup[0] = %v", tup2[0])
	}
}

func TestEncodeDecode_Bool(t *testing.T) {
	// bool encodes as atoms true/false.
	for _, v := range []bool{true, false} {
		enc, err := Encode(v)
		if err != nil {
			t.Fatal(err)
		}
		got, err := Decode(enc)
		if err != nil {
			t.Fatal(err)
		}
		// decoded back as Atom("true") or Atom("false")
		var wantAtom Atom
		if v {
			wantAtom = "true"
		} else {
			wantAtom = "false"
		}
		if got != wantAtom {
			t.Errorf("bool(%v) round-tripped to %v, want Atom(%q)", v, got, string(wantAtom))
		}
	}
}

func TestEncodeDecode_Pid(t *testing.T) {
	p := Pid{Node: Atom("nonode@nohost"), ID: 0x57, Serial: 0, Creation: 0}
	got := roundtrip(t, p)
	p2, ok := got.(Pid)
	if !ok {
		t.Fatalf("roundtrip(Pid) returned %T", got)
	}
	if p2.Node != p.Node || p2.ID != p.ID || p2.Serial != p.Serial || p2.Creation != p.Creation {
		t.Errorf("Pid mismatch: got %+v, want %+v", p2, p)
	}
}

func TestEncodeDecode_Reference(t *testing.T) {
	r := Reference{Node: Atom("nonode@nohost"), Creation: 1, IDs: []uint32{0xdeadbeef, 0xcafebabe}}
	got := roundtrip(t, r)
	r2, ok := got.(Reference)
	if !ok {
		t.Fatalf("roundtrip(Reference) returned %T", got)
	}
	if r2.Node != r.Node || r2.Creation != r.Creation {
		t.Errorf("Reference mismatch: got %+v, want %+v", r2, r)
	}
	if len(r2.IDs) != len(r.IDs) {
		t.Fatalf("Reference.IDs len: got %d, want %d", len(r2.IDs), len(r.IDs))
	}
	for i := range r.IDs {
		if r2.IDs[i] != r.IDs[i] {
			t.Errorf("Reference.IDs[%d]: got %d, want %d", i, r2.IDs[i], r.IDs[i])
		}
	}
}

func TestEncodeDecode_ErlPort(t *testing.T) {
	p := ErlPort{Node: Atom("nonode@nohost"), ID: 1234, Creation: 0}
	got := roundtrip(t, p)
	p2, ok := got.(ErlPort)
	if !ok {
		t.Fatalf("roundtrip(ErlPort) returned %T", got)
	}
	if p2.Node != p.Node || p2.ID != p.ID {
		t.Errorf("ErlPort mismatch: got %+v, want %+v", p2, p)
	}
}

func TestDecode_VersionMagicError(t *testing.T) {
	_, err := Decode([]byte{130, tagSmallInteger, 42})
	if err == nil {
		t.Fatal("expected error for wrong version magic")
	}
}

func TestDecode_TooShort(t *testing.T) {
	_, err := Decode([]byte{})
	if err == nil {
		t.Fatal("expected error for empty input")
	}
	_, err = Decode([]byte{versionMagic})
	if err == nil {
		t.Fatal("expected error for magic-only input")
	}
}

func TestDecode_UnsupportedTag(t *testing.T) {
	// Tag 70 = NEW_FLOAT_EXT — supported; let's try an unsupported one: 77
	data := []byte{versionMagic, 77, 0, 0}
	_, err := Decode(data)
	if err == nil {
		t.Fatal("expected ErrUnsupportedTag")
	}
	var ust *ErrUnsupportedTag
	if !errors.As(err, &ust) {
		t.Errorf("expected *ErrUnsupportedTag, got %T: %v", err, err)
	}
	if ust.Tag != 77 {
		t.Errorf("ErrUnsupportedTag.Tag = %d, want 77", ust.Tag)
	}
}

func TestEncode_UnsupportedType(t *testing.T) {
	_, err := Encode(struct{ X int }{X: 1})
	if err == nil {
		t.Fatal("expected error for unsupported struct type")
	}
}

func TestDecode_CompressedTerm(t *testing.T) {
	// Build a real compressed term: compress(Encode(Atom("hello"))) manually.
	// Step 1: encode the inner term WITHOUT magic byte.
	inner := []byte{tagSmallAtomUTF8, 5, 'h', 'e', 'l', 'l', 'o'}
	// Step 2: zlib compress inner.
	var compressed bytes.Buffer
	w, _ := zlib.NewWriterLevel(&compressed, 6)
	w.Write(inner)
	w.Close()
	// Step 3: assemble: magic + tagCompressed + uint32(len(inner)) + compressed.
	var buf bytes.Buffer
	buf.WriteByte(versionMagic)
	buf.WriteByte(tagCompressed)
	var l [4]byte
	// uncompressed length = len(inner)
	b := l[:]
	b[0] = byte(len(inner) >> 24)
	b[1] = byte(len(inner) >> 16)
	b[2] = byte(len(inner) >> 8)
	b[3] = byte(len(inner))
	buf.Write(b)
	buf.Write(compressed.Bytes())
	got, err := Decode(buf.Bytes())
	if err != nil {
		t.Fatalf("Decode compressed: %v", err)
	}
	if got != Atom("hello") {
		t.Errorf("Decode compressed = %v, want Atom(hello)", got)
	}
}

func TestDecode_BigInt_NotFitsInt64(t *testing.T) {
	// Construct an ETF SMALL_BIG_EXT for 2^65 (does not fit in int64).
	// 2^65 = 0x02_00000000_00000000 (9 bytes little-endian: 0,0,0,0,0,0,0,0,2)
	digits := []byte{0, 0, 0, 0, 0, 0, 0, 0, 2} // little-endian 2^65
	raw := []byte{versionMagic, tagSmallBig, byte(len(digits)), 0}
	raw = append(raw, digits...)
	got, err := Decode(raw)
	if err != nil {
		t.Fatalf("Decode big_int: %v", err)
	}
	bi, ok := got.(*big.Int)
	if !ok {
		t.Fatalf("expected *big.Int for 2^65, got %T (%v)", got, got)
	}
	expected := new(big.Int).Lsh(big.NewInt(1), 65)
	if bi.Cmp(expected) != 0 {
		t.Errorf("big int = %v, want %v", bi, expected)
	}
}

func TestDecode_StringExt_IsCharlist(t *testing.T) {
	// STRING_EXT encodes "abc" as charlist [97, 98, 99].
	raw := []byte{versionMagic, tagString, 0, 3, 'a', 'b', 'c'}
	got, err := Decode(raw)
	if err != nil {
		t.Fatalf("Decode STRING_EXT: %v", err)
	}
	l, ok := got.(List)
	if !ok {
		t.Fatalf("STRING_EXT returned %T, want List", got)
	}
	if len(l) != 3 {
		t.Fatalf("len = %d, want 3", len(l))
	}
	for i, want := range []int64{'a', 'b', 'c'} {
		if l[i] != want {
			t.Errorf("charlist[%d] = %v, want %d", i, l[i], want)
		}
	}
}

// Test with hand-crafted Erlang-generated ETF bytes for a real term.
// term_to_binary({attribute, 1, spec, {{foo, 2}, []}}) in Erlang would
// produce something we can validate structurally. We test with a hand-built
// byte sequence for {ok, 42}.
func TestDecode_OkTuple_ManualBytes(t *testing.T) {
	// {ok, 42} in ETF (hand-built):
	// 131 (magic), 104 (small_tuple), 2 (arity),
	//   119 (small_atom_utf8), 2, 'o', 'k'    -> Atom("ok")
	//   97 (small_integer), 42                 -> int64(42)
	raw := []byte{131, 104, 2, 119, 2, 'o', 'k', 97, 42}
	got, err := Decode(raw)
	if err != nil {
		t.Fatalf("Decode {ok,42}: %v", err)
	}
	tup, ok := got.(Tuple)
	if !ok {
		t.Fatalf("expected Tuple, got %T", got)
	}
	if len(tup) != 2 {
		t.Fatalf("tuple len = %d", len(tup))
	}
	if tup[0] != Atom("ok") {
		t.Errorf("tup[0] = %v, want ok", tup[0])
	}
	if tup[1] != int64(42) {
		t.Errorf("tup[1] = %v, want 42", tup[1])
	}
}
