package typemap

import (
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

// TestPhase5Typemap is the MEP-74 phase 5 sentinel. It drives a small
// hand-built apisurface.File describing a module with one record-
// bridgeable struct (Point), one handle-only struct (Mutex; private
// field), one alias (Name = string), one func, and one chan-valued
// function. Every variant in the closed Mochi-type grammar
// (Scalar, List, Map, Record, Handle, Func, Option, Any) must appear
// somewhere in the resulting mapping, and TransferDirection must
// match the documented lowering rule for each.
func TestPhase5Typemap(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Module:        "example.com/p5",
		Packages: []apisurface.Package{
			{
				ImportPath: "example.com/p5",
				Imports:    []string{"io"},
				Types: []apisurface.Type{
					{
						Name: "Point",
						Kind: apisurface.KindStruct,
						Fields: []apisurface.Field{
							{Name: "X", Type: "int", Exported: true},
							{Name: "Y", Type: "int", Exported: true},
						},
					},
					{
						Name: "Mutex",
						Kind: apisurface.KindStruct,
						Fields: []apisurface.Field{
							{Name: "state", Type: "int32", Exported: false},
						},
					},
					{
						Name:    "Name",
						Kind:    apisurface.KindAlias,
						AliasOf: "string",
					},
				},
			},
		},
	}
	surface, err := apisurface.Load(file, apisurface.LoadOptions{})
	if err != nil {
		t.Fatalf("apisurface.Load: %v", err)
	}
	m := NewMapper(surface)

	cases := []struct {
		expr      string
		wantKind  string // discriminator (record, handle<...>, list<...>, option<...>, etc.)
		wantDir   TransferDirection
		mustHave  string // substring required in Mochi.String()
	}{
		{"int", "scalar", Copy, "int"},
		{"float64", "scalar", Copy, "float"},
		{"bool", "scalar", Copy, "bool"},
		{"string", "scalar", Copy, "string"},
		{"[]byte", "scalar", Copy, "bytes"},
		{"[]string", "list", Copy, "list<string>"},
		{"[]example.com/p5.Point", "list", Copy, "list<Point>"},
		{"map[string]int", "map", Copy, "map<string, int>"},
		{"*int", "option", Copy, "option<int>"},
		{"*example.com/p5.Point", "option", Copy, "option<Point>"},
		{"example.com/p5.Point", "record", Copy, "Point"},
		{"example.com/p5.Mutex", "handle", Handle, "handle<Mutex>"},
		{"example.com/p5.Name", "scalar", Copy, "string"},
		{"chan int", "handle", Handle, "handle<chan<int>>"},
		{"func(int) (string, error)", "func", Handle, "fn(int) -> (string, error)"},
		{"interface{}", "any", Handle, "any"},
		{"any", "any", Handle, "any"},
		{"io.Reader", "handle", Handle, "handle<io.Reader>"},
		{"...string", "list", Copy, "list<string>"},
		{"[4]byte", "list", Copy, "list<int>"},
		{"map[string][]int", "map", Copy, "map<string, list<int>>"},
	}

	seenKinds := map[string]bool{}
	for _, tc := range cases {
		gt, perr := apisurface.ParseType(tc.expr)
		if perr != nil {
			t.Errorf("ParseType(%q): %v", tc.expr, perr)
			continue
		}
		mp, mErr := m.Map(gt)
		if mErr != nil {
			t.Errorf("Map(%q): %v", tc.expr, mErr)
			continue
		}
		if mp.Direction != tc.wantDir {
			t.Errorf("Map(%q).Direction = %v; want %v", tc.expr, mp.Direction, tc.wantDir)
		}
		got := mp.Mochi.String()
		if !strings.Contains(got, tc.mustHave) {
			t.Errorf("Map(%q).Mochi.String() = %q; want substring %q", tc.expr, got, tc.mustHave)
		}
		switch mp.Mochi.(type) {
		case ScalarType:
			seenKinds["scalar"] = true
		case ListType:
			seenKinds["list"] = true
		case MochiMap:
			seenKinds["map"] = true
		case RecordType:
			seenKinds["record"] = true
		case HandleType:
			seenKinds["handle"] = true
		case FuncType:
			seenKinds["func"] = true
		case OptionType:
			seenKinds["option"] = true
		case AnyType:
			seenKinds["any"] = true
		}
	}

	// Every variant of MochiType must be exercised by the sentinel.
	for _, k := range []string{"scalar", "list", "map", "record", "handle", "func", "option", "any"} {
		if !seenKinds[k] {
			t.Errorf("sentinel did not exercise MochiType kind %q", k)
		}
	}
}
