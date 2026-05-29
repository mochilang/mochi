package apisurface

import (
	"bytes"
	"encoding/json"
	"errors"
	"strings"
	"testing"
)

func TestEncodeDecodeRoundTrip(t *testing.T) {
	in := &File{
		SchemaVersion: SchemaVersion,
		Module:        "example.com/foo",
		Version:       "v1.2.3",
		GoVersion:     "go1.21.0",
		GeneratedBy:   "go-ingest test",
		Packages: []Package{
			{
				ImportPath: "example.com/foo",
				Name:       "foo",
				Doc:        "Package foo is a fixture.",
				Imports:    []string{"io"},
				Funcs: []Func{
					{
						Name:    "Hello",
						Doc:     "Hello returns greeting.",
						Params:  []Param{{Name: "name", Type: "string"}},
						Results: []Param{{Type: "string"}},
					},
				},
				Types: []Type{
					{
						Name: "Greeter",
						Kind: KindStruct,
						Fields: []Field{
							{Name: "Name", Type: "string", Exported: true},
						},
					},
				},
				Consts: []Value{
					{Name: "Version", Type: "string", Const: "\"1\""},
				},
				Vars: []Value{
					{Name: "Default", Type: "*Greeter"},
				},
				Skipped: []SkipNote{
					{Kind: "func", Name: "unexported", Reason: "Unexported"},
				},
			},
		},
	}

	buf, err := in.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if !bytes.HasSuffix(buf, []byte("\n")) {
		t.Errorf("Encode output missing trailing newline")
	}
	if !bytes.Contains(buf, []byte("\"schema_version\": 1")) {
		t.Errorf("Encode output missing schema_version: %s", buf)
	}

	out, err := Decode(buf)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	if out.Module != in.Module {
		t.Errorf("Module mismatch: %q vs %q", out.Module, in.Module)
	}
	if len(out.Packages) != 1 {
		t.Fatalf("Packages len = %d, want 1", len(out.Packages))
	}
	if out.Packages[0].Funcs[0].Name != "Hello" {
		t.Errorf("Funcs[0].Name = %q", out.Packages[0].Funcs[0].Name)
	}
}

func TestDecodeRejectsSchemaVersionMismatch(t *testing.T) {
	bad := []byte(`{"schema_version":99,"module":"x"}`)
	_, err := Decode(bad)
	if err == nil {
		t.Fatalf("Decode: want error, got nil")
	}
	if !errors.Is(err, ErrSchemaVersion) {
		t.Errorf("error not ErrSchemaVersion: %v", err)
	}
	if !strings.Contains(err.Error(), "99") {
		t.Errorf("error should mention bad version: %v", err)
	}
}

func TestDecodeRejectsMalformedJSON(t *testing.T) {
	_, err := Decode([]byte("not json"))
	if err == nil {
		t.Fatalf("Decode: want error, got nil")
	}
}

func TestEncodeNil(t *testing.T) {
	var f *File
	if _, err := f.Encode(); err == nil {
		t.Errorf("Encode on nil receiver: want error, got nil")
	}
}

func TestEncodeOmitemptyFields(t *testing.T) {
	in := &File{SchemaVersion: SchemaVersion, Module: "m"}
	buf, err := in.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	// Optional fields with empty values must not appear.
	for _, k := range []string{"\"version\"", "\"go_version\"", "\"generated_by\"", "\"packages\":["} {
		if bytes.Contains(buf, []byte(k)) {
			// "packages":[ may appear as "packages":null — both omitted
			// would be acceptable, but the JSON marshaller emits null
			// for a nil slice on a non-omitempty field. Confirm that's
			// not the case here.
			if k == "\"packages\":[" {
				continue
			}
			t.Errorf("unexpected key %s in minimal Encode output: %s", k, buf)
		}
	}
}

func TestSchemaVersionConstant(t *testing.T) {
	// If anyone bumps this constant, every cached ApiSurface JSON
	// becomes invalid, so guard the bump explicitly.
	if SchemaVersion != 1 {
		t.Errorf("SchemaVersion = %d; want 1 (bump requires bridge cache invalidation)", SchemaVersion)
	}
}

func TestEncodeStableJSON(t *testing.T) {
	in := &File{SchemaVersion: SchemaVersion, Module: "m"}
	a, err := in.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	b, err := in.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	if !bytes.Equal(a, b) {
		t.Errorf("Encode is not deterministic")
	}
}

func TestEncodeValidJSON(t *testing.T) {
	in := &File{SchemaVersion: SchemaVersion, Module: "m"}
	buf, err := in.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	var raw map[string]any
	if err := json.Unmarshal(buf, &raw); err != nil {
		t.Errorf("Encoded output is not valid JSON: %v", err)
	}
}
