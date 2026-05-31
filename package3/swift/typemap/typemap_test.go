package typemap_test

import (
	"testing"

	swifterrors "mochi/package3/swift/errors"
	"mochi/package3/swift/typemap"
)

func TestMapPrimitives(t *testing.T) {
	cases := []struct {
		input     string
		wantKind  typemap.Kind
		wantMochi string
	}{
		{"Int", typemap.KindInt, "i64"},
		{"Int32", typemap.KindInt, "i32"},
		{"Int64", typemap.KindInt, "i64"},
		{"UInt8", typemap.KindInt, "u8"},
		{"Float", typemap.KindFloat, "f32"},
		{"Double", typemap.KindFloat, "f64"},
		{"Bool", typemap.KindBool, "bool"},
		{"String", typemap.KindString, "str"},
		{"Void", typemap.KindUnit, "unit"},
		{"()", typemap.KindUnit, "unit"},
	}
	for _, tc := range cases {
		m := typemap.Map(tc.input)
		if m.Kind != tc.wantKind {
			t.Errorf("Map(%q).Kind = %v, want %v", tc.input, m.Kind, tc.wantKind)
		}
		if m.MochiType != tc.wantMochi {
			t.Errorf("Map(%q).MochiType = %q, want %q", tc.input, m.MochiType, tc.wantMochi)
		}
	}
}

func TestMapCollections(t *testing.T) {
	cases := []struct {
		input     string
		wantKind  typemap.Kind
		wantMochi string
	}{
		{"[Int]", typemap.KindList, "list<i64>"},
		{"[String]", typemap.KindList, "list<str>"},
		{"[String: Int]", typemap.KindMap, "map<str, i64>"},
		{"Set<String>", typemap.KindSet, "set<str>"},
		{"String?", typemap.KindOption, "option<str>"},
		{"Optional<Int>", typemap.KindOption, "option<i64>"},
	}
	for _, tc := range cases {
		m := typemap.Map(tc.input)
		if m.Kind != tc.wantKind {
			t.Errorf("Map(%q).Kind = %v, want %v", tc.input, m.Kind, tc.wantKind)
		}
		if m.MochiType != tc.wantMochi {
			t.Errorf("Map(%q).MochiType = %q, want %q", tc.input, m.MochiType, tc.wantMochi)
		}
	}
}

func TestMapSkip(t *testing.T) {
	cases := []struct {
		input      string
		wantReason swifterrors.SkipReason
	}{
		{"UnsafePointer<Int>", swifterrors.SkipUnsafePointer},
		{"UnsafeMutablePointer<UInt8>", swifterrors.SkipUnsafePointer},
		{"(Int) -> String", swifterrors.SkipClosure},
		{"any Codable", swifterrors.SkipExistential},
		{"some Collection", swifterrors.SkipOpaque},
		{"T", swifterrors.SkipGeneric},
		{"TaskGroup", swifterrors.SkipConcurrencyType},
	}
	for _, tc := range cases {
		m := typemap.Map(tc.input)
		if m.Kind != typemap.KindSkip {
			t.Errorf("Map(%q).Kind = %v, want KindSkip", tc.input, m.Kind)
			continue
		}
		if m.SkipReason != tc.wantReason {
			t.Errorf("Map(%q).SkipReason = %v, want %v", tc.input, m.SkipReason, tc.wantReason)
		}
	}
}

func TestMapRecord(t *testing.T) {
	m := typemap.Map("MyCustomType")
	if m.Kind != typemap.KindRecord {
		t.Errorf("Map(MyCustomType).Kind = %v, want KindRecord", m.Kind)
	}
	if m.MochiType != "MyCustomType" {
		t.Errorf("Map(MyCustomType).MochiType = %q, want MyCustomType", m.MochiType)
	}
}
