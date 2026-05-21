package typebridge

import (
	"testing"
)

// TestMochiToGo covers the rendering of every primitive Type and a
// handful of containers. The round-trip property (parsing the
// output back through go/types matches the input) is in
// roundtrip_test.go; this test pins the literal string form so
// reviewers see exactly what the emitter will write into Go source.
func TestMochiToGo(t *testing.T) {
	cases := []struct {
		name string
		t    Type
		want string
	}{
		{"bool", Type{Kind: KindBool}, "bool"},
		{"int", Type{Kind: KindInt}, "int"},
		{"int32", Type{Kind: KindInt, Width: 32}, "int32"},
		{"uint8", Type{Kind: KindUint, Width: 8}, "uint8"},
		{"float64", Type{Kind: KindFloat, Width: 64}, "float64"},
		{"string", Type{Kind: KindString}, "string"},
		{"bytes", Type{Kind: KindBytes}, "[]byte"},
		{"list-int", Type{Kind: KindList, Elem: &Type{Kind: KindInt}}, "[]int"},
		{"array-3-string", Type{Kind: KindArray, ArrayLen: 3, Elem: &Type{Kind: KindString}}, "[3]string"},
		{"map-string-int", Type{Kind: KindMap, Key: &Type{Kind: KindString}, Elem: &Type{Kind: KindInt}}, "map[string]int"},
		{"ref-int", Type{Kind: KindRef, Elem: &Type{Kind: KindInt}}, "*int"},
		{"chan-int", Type{Kind: KindChan, ChanDir: ChanBoth, Elem: &Type{Kind: KindInt}}, "chan int"},
		{"chan-send-int", Type{Kind: KindChan, ChanDir: ChanSend, Elem: &Type{Kind: KindInt}}, "chan<- int"},
		{"chan-recv-int", Type{Kind: KindChan, ChanDir: ChanRecv, Elem: &Type{Kind: KindInt}}, "<-chan int"},
		{"any", Type{Kind: KindIface}, "any"},
		{"named-strings-reader", Type{Kind: KindNamed, Name: "Reader", PkgPath: "strings"}, "strings.Reader"},
		{"iface-io-reader", Type{Kind: KindIface, Name: "Reader", PkgPath: "io"}, "io.Reader"},
		{"opaque-unsafe", Type{Kind: KindOpaque, OpaqueReason: OpaqueUnsafePointer, GoType: "unsafe.Pointer"}, "unsafe.Pointer"},
		{"struct-empty", Type{Kind: KindStruct}, "struct{}"},
		{
			"struct-name-age",
			Type{Kind: KindStruct, Fields: []Field{
				{Name: "Name", Type: Type{Kind: KindString}, Exported: true},
				{Name: "Age", Type: Type{Kind: KindInt}, Exported: true},
			}},
			"struct{Name string; Age int}",
		},
		{
			"func-add",
			Type{
				Kind:    KindFunc,
				Params:  []Type{{Kind: KindInt}, {Kind: KindInt}},
				Results: []Type{{Kind: KindInt}},
			},
			"func(int, int) int",
		},
		{
			"func-variadic",
			Type{
				Kind:     KindFunc,
				Params:   []Type{{Kind: KindList, Elem: &Type{Kind: KindInt}}},
				Variadic: true,
			},
			"func(...int)",
		},
		{
			"func-multi-ret",
			Type{
				Kind:    KindFunc,
				Results: []Type{{Kind: KindInt}, {Kind: KindIface, Name: "error"}},
			},
			"func() (int, error)",
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got := MochiToGo(c.t)
			if got != c.want {
				t.Fatalf("MochiToGo = %q\nwant       = %q", got, c.want)
			}
		})
	}
}
