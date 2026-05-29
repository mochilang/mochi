package rustdoc

import (
	"errors"
	"strings"
	"testing"
)

const minimalDoc = `{
  "root": "0:0:0",
  "crate_version": "1.0.0",
  "includes_private": false,
  "index": {
    "0:0:0": {
      "id": "0:0:0",
      "crate_id": 0,
      "name": "demo",
      "visibility": "public",
      "inner": {"module": {"is_crate": true, "items": [], "is_stripped": false}}
    }
  },
  "paths": {
    "0:0:0": {"crate_id": 0, "path": ["demo"], "kind": "module"}
  },
  "external_crates": {},
  "format_version": 39
}`

func TestParseMinimal(t *testing.T) {
	doc, err := Parse(strings.NewReader(minimalDoc))
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if doc.Root != "0:0:0" {
		t.Errorf("Root = %q", doc.Root)
	}
	if doc.CrateVersion != "1.0.0" {
		t.Errorf("CrateVersion = %q", doc.CrateVersion)
	}
	if doc.FormatVersion != 39 {
		t.Errorf("FormatVersion = %d", doc.FormatVersion)
	}
	if len(doc.Index) != 1 {
		t.Errorf("len(Index) = %d", len(doc.Index))
	}
	if root := doc.Index["0:0:0"]; root.Inner.Kind() != "module" {
		t.Errorf("root.Inner.Kind = %q", root.Inner.Kind())
	}
}

func TestParseUnsupportedVersion(t *testing.T) {
	in := strings.Replace(minimalDoc, `"format_version": 39`, `"format_version": 1`, 1)
	_, err := Parse(strings.NewReader(in))
	var unsup *ErrUnsupportedFormatVersion
	if !errors.As(err, &unsup) {
		t.Fatalf("Parse(format_version=1) err = %v; want ErrUnsupportedFormatVersion", err)
	}
	if unsup.Got != 1 {
		t.Errorf("ErrUnsupportedFormatVersion.Got = %d; want 1", unsup.Got)
	}
}

func TestParseFutureVersion(t *testing.T) {
	in := strings.Replace(minimalDoc, `"format_version": 39`, `"format_version": 99`, 1)
	_, err := Parse(strings.NewReader(in))
	var unsup *ErrUnsupportedFormatVersion
	if !errors.As(err, &unsup) {
		t.Fatalf("Parse(format_version=99) err = %v; want ErrUnsupportedFormatVersion", err)
	}
}

func TestSupportedFormatVersionsContains(t *testing.T) {
	want := map[int]bool{37: true, 38: true, 39: true}
	if len(SupportedFormatVersions) != len(want) {
		t.Errorf("SupportedFormatVersions = %v", SupportedFormatVersions)
	}
	for _, v := range SupportedFormatVersions {
		if !want[v] {
			t.Errorf("unexpected version %d", v)
		}
	}
}

func TestParseBytes(t *testing.T) {
	doc, err := ParseBytes([]byte(minimalDoc))
	if err != nil {
		t.Fatalf("ParseBytes: %v", err)
	}
	if doc.CrateVersion != "1.0.0" {
		t.Errorf("CrateVersion = %q", doc.CrateVersion)
	}
}

func TestParseBadJSON(t *testing.T) {
	if _, err := Parse(strings.NewReader("not json")); err == nil {
		t.Errorf("Parse(garbage) err = nil; want error")
	}
}

func TestVisibilityVariants(t *testing.T) {
	cases := []struct {
		raw  string
		kind VisibilityKind
		isPub bool
	}{
		{`"public"`, VisibilityPublic, true},
		{`"default"`, VisibilityDefault, false},
		{`{"crate": "demo"}`, VisibilityCrate, false},
		{`{"restricted": {"parent": "0:0:0", "path": "demo::inner"}}`, VisibilityRestricted, false},
	}
	for _, tc := range cases {
		var v Visibility
		if err := v.UnmarshalJSON([]byte(tc.raw)); err != nil {
			t.Errorf("UnmarshalJSON(%s) err = %v", tc.raw, err)
			continue
		}
		if v.Kind != tc.kind {
			t.Errorf("UnmarshalJSON(%s).Kind = %v; want %v", tc.raw, v.Kind, tc.kind)
		}
		if v.IsPublic() != tc.isPub {
			t.Errorf("UnmarshalJSON(%s).IsPublic = %v; want %v", tc.raw, v.IsPublic(), tc.isPub)
		}
	}
}

func TestVisibilityUnknown(t *testing.T) {
	var v Visibility
	if err := v.UnmarshalJSON([]byte(`"weird"`)); err == nil {
		t.Errorf("UnmarshalJSON(weird) err = nil")
	}
}
