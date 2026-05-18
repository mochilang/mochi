package vm3

import (
	"math"
	"testing"
)

func TestCellInt(t *testing.T) {
	for _, v := range []int64{0, 1, -1, 42, -42, MaxInlineInt, MinInlineInt} {
		c := CInt(v)
		if !c.IsInt() {
			t.Fatalf("CInt(%d): IsInt false", v)
		}
		if got := c.Int(); got != v {
			t.Fatalf("CInt(%d) round-trip: got %d", v, got)
		}
	}
}

func TestCellFloat(t *testing.T) {
	for _, v := range []float64{0, 1, -1, 3.14, math.Inf(1), math.Inf(-1)} {
		c := CFloat(v)
		if !c.IsFloat() {
			t.Fatalf("CFloat(%v): IsFloat false", v)
		}
		if got := c.Float(); got != v {
			t.Fatalf("CFloat(%v) round-trip: got %v", v, got)
		}
	}
	// NaN canonicalizes; the cell must still read as float.
	c := CFloat(math.NaN())
	if !c.IsFloat() {
		t.Fatalf("CFloat(NaN): IsFloat false")
	}
}

func TestCellBool(t *testing.T) {
	if !CBool(true).IsBool() || !CBool(false).IsBool() {
		t.Fatal("IsBool false on CBool")
	}
	if !CBool(true).Bool() || CBool(false).Bool() {
		t.Fatal("Bool decode wrong")
	}
}

func TestCellNull(t *testing.T) {
	if !CNull().IsNull() {
		t.Fatal("IsNull false on CNull")
	}
}

func TestCellSStr(t *testing.T) {
	for _, s := range [][]byte{nil, {'a'}, {'a', 'b'}, {'a', 'b', 'c', 'd', 'e'}} {
		c := CSStr(s)
		if !c.IsSStr() {
			t.Fatalf("CSStr(%q): IsSStr false", s)
		}
		if got := c.SStrLen(); got != len(s) {
			t.Fatalf("CSStr(%q) len: got %d want %d", s, got, len(s))
		}
		var buf [MaxInlineStr]byte
		got := c.SStrBytes(&buf)
		if len(got) != len(s) {
			t.Fatalf("SStrBytes len: got %d want %d", len(got), len(s))
		}
		for i := range got {
			if got[i] != s[i] {
				t.Fatalf("SStrBytes byte %d: got %x want %x", i, got[i], s[i])
			}
		}
	}
}

func TestHandleRoundTrip(t *testing.T) {
	for _, tag := range []ArenaTag{
		ArenaString, ArenaList, ArenaMap, ArenaSet, ArenaStruct,
		ArenaClosure, ArenaBignum, ArenaBytes, ArenaPair,
		ArenaF64Arr, ArenaI64Arr, ArenaU8Arr,
	} {
		for _, gen := range []uint16{0, 1, 0xFFF} {
			for _, idx := range []uint32{0, 1, 0xFFFF_FFFF} {
				c := MakeHandle(tag, gen, idx)
				if !c.IsHandle() {
					t.Fatalf("MakeHandle(%v, %d, %d): IsHandle false", tag, gen, idx)
				}
				gotTag, gotGen, gotIdx := c.DecodeHandle()
				if gotTag != tag || gotGen != gen&0xFFF || gotIdx != idx {
					t.Fatalf("DecodeHandle(%v, %d, %d) = (%v, %d, %d)",
						tag, gen, idx, gotTag, gotGen, gotIdx)
				}
			}
		}
	}
}

func TestDeoptRoundTrip(t *testing.T) {
	for _, pc := range []int{0, 1, 42, 1 << 20} {
		c := EncodeDeopt(pc)
		if !c.IsDeopt() {
			t.Fatalf("EncodeDeopt(%d): IsDeopt false", pc)
		}
		got, ok := DecodeDeopt(c)
		if !ok || got != pc {
			t.Fatalf("DecodeDeopt(%d): got (%d, %v)", pc, got, ok)
		}
	}
	if _, ok := DecodeDeopt(CInt(7)); ok {
		t.Fatal("DecodeDeopt on int: ok true")
	}
}

func TestFitsInline(t *testing.T) {
	if !FitsInline(0) || !FitsInline(MaxInlineInt) || !FitsInline(MinInlineInt) {
		t.Fatal("FitsInline false on edge")
	}
	if FitsInline(MaxInlineInt + 1) || FitsInline(MinInlineInt - 1) {
		t.Fatal("FitsInline true past edge")
	}
}
