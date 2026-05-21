package typebridge

import (
	"go/types"
	"testing"
)

// TestBasics covers every *types.Basic the bridge accepts. Each
// row asserts both the structural mapping and the OpaqueReason for
// shapes that degrade.
func TestBasics(t *testing.T) {
	cases := []struct {
		name   string
		basic  types.BasicKind
		want   Type
	}{
		{"bool", types.Bool, Type{Kind: KindBool}},
		{"int", types.Int, Type{Kind: KindInt}},
		{"int8", types.Int8, Type{Kind: KindInt, Width: 8}},
		{"int16", types.Int16, Type{Kind: KindInt, Width: 16}},
		{"int32", types.Int32, Type{Kind: KindInt, Width: 32}},
		{"int64", types.Int64, Type{Kind: KindInt, Width: 64}},
		{"uint", types.Uint, Type{Kind: KindUint}},
		{"uint8", types.Uint8, Type{Kind: KindUint, Width: 8}},
		{"uint16", types.Uint16, Type{Kind: KindUint, Width: 16}},
		{"uint32", types.Uint32, Type{Kind: KindUint, Width: 32}},
		{"uint64", types.Uint64, Type{Kind: KindUint, Width: 64}},
		{"uintptr", types.Uintptr, Type{Kind: KindOpaque, OpaqueReason: OpaqueUintptr, GoType: "uintptr"}},
		{"float32", types.Float32, Type{Kind: KindFloat, Width: 32}},
		{"float64", types.Float64, Type{Kind: KindFloat, Width: 64}},
		{"complex64", types.Complex64, Type{Kind: KindOpaque, OpaqueReason: OpaqueComplex, GoType: "complex64"}},
		{"complex128", types.Complex128, Type{Kind: KindOpaque, OpaqueReason: OpaqueComplex, GoType: "complex128"}},
		{"string", types.String, Type{Kind: KindString}},
		{"unsafe.Pointer", types.UnsafePointer, Type{Kind: KindOpaque, OpaqueReason: OpaqueUnsafePointer, GoType: "unsafe.Pointer"}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got := GoToMochi(types.Typ[c.basic])
			if !got.Equal(c.want) {
				t.Fatalf("GoToMochi(%s) = %+v\n  want = %+v", c.name, got, c.want)
			}
		})
	}
}

// TestBytesAlias confirms []byte produces KindBytes (not KindList of
// uint8), per MEP-44 §2.
func TestBytesAlias(t *testing.T) {
	slice := types.NewSlice(types.Typ[types.Uint8])
	got := GoToMochi(slice)
	if got.Kind != KindBytes {
		t.Fatalf("[]byte -> %s, want bytes", got.Kind)
	}
}

// TestSliceOfInt confirms []int produces KindList{Elem: int}.
func TestSliceOfInt(t *testing.T) {
	slice := types.NewSlice(types.Typ[types.Int])
	got := GoToMochi(slice)
	if got.Kind != KindList || got.Elem == nil || got.Elem.Kind != KindInt {
		t.Fatalf("[]int -> %+v, want list<int>", got)
	}
}

// TestArrayOfFloat32 confirms [4]float32 carries ArrayLen=4.
func TestArrayOfFloat32(t *testing.T) {
	arr := types.NewArray(types.Typ[types.Float32], 4)
	got := GoToMochi(arr)
	if got.Kind != KindArray || got.ArrayLen != 4 || got.Elem.Kind != KindFloat || got.Elem.Width != 32 {
		t.Fatalf("[4]float32 -> %+v", got)
	}
}

// TestMapStringInt confirms map[string]int produces KindMap with
// Key=string Elem=int.
func TestMapStringInt(t *testing.T) {
	m := types.NewMap(types.Typ[types.String], types.Typ[types.Int])
	got := GoToMochi(m)
	if got.Kind != KindMap || got.Key.Kind != KindString || got.Elem.Kind != KindInt {
		t.Fatalf("map[string]int -> %+v", got)
	}
}

// TestPointer confirms *int produces KindRef{Elem: int}.
func TestPointer(t *testing.T) {
	p := types.NewPointer(types.Typ[types.Int])
	got := GoToMochi(p)
	if got.Kind != KindRef || got.Elem.Kind != KindInt {
		t.Fatalf("*int -> %+v", got)
	}
}

// TestChannel confirms chan int, <-chan int, chan<- int all map.
func TestChannel(t *testing.T) {
	for _, c := range []struct {
		dir  types.ChanDir
		want ChanDir
	}{
		{types.SendRecv, ChanBoth},
		{types.SendOnly, ChanSend},
		{types.RecvOnly, ChanRecv},
	} {
		ch := types.NewChan(c.dir, types.Typ[types.Int])
		got := GoToMochi(ch)
		if got.Kind != KindChan || got.ChanDir != c.want || got.Elem.Kind != KindInt {
			t.Fatalf("chan dir=%v -> %+v", c.dir, got)
		}
	}
}

// TestStruct walks an anonymous struct with mixed fields.
func TestStruct(t *testing.T) {
	fields := []*types.Var{
		types.NewField(0, nil, "Name", types.Typ[types.String], false),
		types.NewField(0, nil, "Age", types.Typ[types.Int], false),
	}
	tags := []string{`json:"name"`, ""}
	s := types.NewStruct(fields, tags)
	got := GoToMochi(s)
	if got.Kind != KindStruct {
		t.Fatalf("struct kind=%s", got.Kind)
	}
	if len(got.Fields) != 2 {
		t.Fatalf("field count = %d", len(got.Fields))
	}
	if got.Fields[0].Name != "Name" || got.Fields[0].Tag != `json:"name"` {
		t.Fatalf("field[0] = %+v", got.Fields[0])
	}
	if got.Fields[1].Type.Kind != KindInt {
		t.Fatalf("field[1] type = %+v", got.Fields[1].Type)
	}
}

// TestFuncVariadic checks that the Variadic flag is preserved.
func TestFuncVariadic(t *testing.T) {
	params := types.NewTuple(types.NewVar(0, nil, "x", types.NewSlice(types.Typ[types.Int])))
	results := types.NewTuple()
	sig := types.NewSignatureType(nil, nil, nil, params, results, true)
	got := GoToMochi(sig)
	if got.Kind != KindFunc {
		t.Fatalf("kind=%s", got.Kind)
	}
	if !got.Variadic {
		t.Fatalf("Variadic flag dropped")
	}
	if len(got.Params) != 1 || got.Params[0].Kind != KindList {
		t.Fatalf("params=%+v", got.Params)
	}
}

// TestEmptyInterface confirms `interface{}` (anonymous, no methods)
// maps to KindIface with empty Name and Methods, which MochiToGo
// renders as `any`.
func TestEmptyInterface(t *testing.T) {
	iface := types.NewInterfaceType(nil, nil)
	iface.Complete()
	got := GoToMochi(iface)
	if got.Kind != KindIface {
		t.Fatalf("kind=%s", got.Kind)
	}
	if got.Name != "" || len(got.Methods) != 0 {
		t.Fatalf("expected anonymous empty iface, got %+v", got)
	}
	if MochiToGo(got) != "any" {
		t.Fatalf("MochiToGo(empty iface) = %q, want any", MochiToGo(got))
	}
}

// TestNilPanics verifies the contract that GoToMochi(nil) panics.
func TestNilPanics(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Fatal("expected panic on nil")
		}
	}()
	GoToMochi(nil)
}
