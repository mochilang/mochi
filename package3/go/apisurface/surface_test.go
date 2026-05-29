package apisurface

import (
	"errors"
	"testing"
)

func makeSampleFile() *File {
	return &File{
		SchemaVersion: SchemaVersion,
		Module:        "example.com/foo",
		Packages: []Package{
			{
				ImportPath: "example.com/foo",
				Name:       "foo",
				Imports:    []string{"io"},
				Funcs: []Func{
					{
						Name:    "Read",
						Params:  []Param{{Name: "r", Type: "io.Reader"}, {Name: "buf", Type: "[]byte"}},
						Results: []Param{{Type: "int"}, {Type: "error"}},
					},
					{
						Name:    "Generic",
						Params:  []Param{{Name: "v", Type: "T"}},
						TypeParams: []TypeParam{{Name: "T", Constraint: "any"}},
					},
					{
						Name:     "Varied",
						Params:   []Param{{Name: "args", Type: "...string"}},
						Variadic: true,
					},
				},
				Types: []Type{
					{
						Name: "Greeter",
						Kind: KindStruct,
						Fields: []Field{
							{Name: "Name", Type: "string", Exported: true},
							{Name: "R", Type: "io.Reader", Exported: true},
						},
						Methods: []Func{
							{Name: "Hello", Receiver: "Greeter", Results: []Param{{Type: "string"}}},
						},
					},
					{
						Name:    "Alias",
						Kind:    KindAlias,
						AliasOf: "string",
					},
					{
						Name: "Iface",
						Kind: KindInterface,
						InterfaceMethods: []Func{
							{Name: "Read", Params: []Param{{Name: "p", Type: "[]byte"}}, Results: []Param{{Type: "int"}, {Type: "error"}}},
						},
					},
				},
				Consts: []Value{
					{Name: "Version", Type: "string", Const: "\"1\""},
				},
				Vars: []Value{
					{Name: "Default", Type: "*Greeter"},
				},
			},
		},
	}
}

func TestSurfaceLoadHappyPath(t *testing.T) {
	s, err := Load(makeSampleFile(), LoadOptions{})
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if len(s.Packages) != 1 {
		t.Fatalf("Packages len = %d", len(s.Packages))
	}
	if got := s.LookupFunc("example.com/foo", "Read"); got == nil {
		t.Errorf("LookupFunc(Read) = nil")
	} else {
		if len(got.Params) != 2 || len(got.Results) != 2 {
			t.Errorf("Read shape: params=%d results=%d", len(got.Params), len(got.Results))
		}
		// io.Reader param parses as NamedType io.Reader.
		n, ok := got.Params[0].Type.(NamedType)
		if !ok || n.PackagePath != "io" || n.Name != "Reader" {
			t.Errorf("Params[0].Type = %#v", got.Params[0].Type)
		}
		// []byte param parses as SliceType.
		if _, ok := got.Params[1].Type.(SliceType); !ok {
			t.Errorf("Params[1].Type = %T", got.Params[1].Type)
		}
	}
	if got := s.LookupType("example.com/foo", "Greeter"); got == nil {
		t.Errorf("LookupType(Greeter) = nil")
	} else {
		if got.Kind != KindStruct {
			t.Errorf("Greeter.Kind = %q", got.Kind)
		}
		if len(got.Fields) != 2 {
			t.Errorf("Greeter.Fields = %+v", got.Fields)
		}
		if len(got.Methods) != 1 {
			t.Errorf("Greeter.Methods = %+v", got.Methods)
		}
	}
	if got := s.LookupMethod("example.com/foo", "Greeter", "Hello"); got == nil {
		t.Errorf("LookupMethod(Greeter.Hello) = nil")
	}
	if got := s.LookupType("example.com/foo", "Alias"); got == nil || got.AliasOf == nil {
		t.Errorf("Alias: %+v", got)
	}
}

func TestSurfaceLoadGeneric(t *testing.T) {
	s, err := Load(makeSampleFile(), LoadOptions{})
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	gen := s.LookupFunc("example.com/foo", "Generic")
	if gen == nil {
		t.Fatalf("Generic missing")
	}
	if len(gen.TypeParams) != 1 || gen.TypeParams[0].Name != "T" {
		t.Errorf("TypeParams = %+v", gen.TypeParams)
	}
	if c, ok := gen.TypeParams[0].Constraint.(BasicType); !ok || c.Name != "any" {
		t.Errorf("Constraint = %#v", gen.TypeParams[0].Constraint)
	}
}

func TestSurfaceLoadVariadic(t *testing.T) {
	s, err := Load(makeSampleFile(), LoadOptions{})
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	v := s.LookupFunc("example.com/foo", "Varied")
	if v == nil {
		t.Fatalf("Varied missing")
	}
	if _, ok := v.Params[0].Type.(EllipsisType); !ok {
		t.Errorf("Varied param type = %T", v.Params[0].Type)
	}
}

func TestSurfaceLoadInterface(t *testing.T) {
	s, _ := Load(makeSampleFile(), LoadOptions{})
	td := s.LookupType("example.com/foo", "Iface")
	if td == nil || td.Kind != KindInterface {
		t.Fatalf("Iface = %+v", td)
	}
	if len(td.InterfaceMethods) != 1 {
		t.Errorf("Iface.InterfaceMethods = %+v", td.InterfaceMethods)
	}
}

func TestSurfaceLoadRejectsBadSchema(t *testing.T) {
	bad := &File{SchemaVersion: 99}
	_, err := Load(bad, LoadOptions{})
	if err == nil {
		t.Fatalf("Load: want error")
	}
	if !errors.Is(err, ErrSurfaceLoad) {
		t.Errorf("error = %v", err)
	}
}

func TestSurfaceLoadRejectsNil(t *testing.T) {
	if _, err := Load(nil, LoadOptions{}); err == nil {
		t.Errorf("Load(nil): want error")
	}
}

func TestSurfaceLoadParseError(t *testing.T) {
	bad := makeSampleFile()
	bad.Packages[0].Funcs[0].Params[0].Type = "[" // malformed
	_, err := Load(bad, LoadOptions{})
	if err == nil {
		t.Errorf("Load: want error on malformed type")
	}
}

func TestSurfaceLoadStrictCrossReferences(t *testing.T) {
	bad := makeSampleFile()
	// Reference an external package not in Imports.
	bad.Packages[0].Funcs = append(bad.Packages[0].Funcs, Func{
		Name:   "Phantom",
		Params: []Param{{Name: "x", Type: "phantom.Type"}},
	})
	if _, err := Load(bad, LoadOptions{StrictCrossReferences: true}); err == nil {
		t.Errorf("Load strict: want error on missing import")
	}
	if _, err := Load(bad, LoadOptions{}); err != nil {
		t.Errorf("Load non-strict: should accept missing import: %v", err)
	}
}

func TestSurfacePackagePaths(t *testing.T) {
	s, _ := Load(makeSampleFile(), LoadOptions{})
	paths := s.PackagePaths()
	if len(paths) != 1 || paths[0] != "example.com/foo" {
		t.Errorf("PackagePaths = %v", paths)
	}
}

func TestSurfaceLookupMissingPkg(t *testing.T) {
	s, _ := Load(makeSampleFile(), LoadOptions{})
	if s.LookupFunc("missing", "x") != nil {
		t.Errorf("LookupFunc on missing pkg: want nil")
	}
	if s.LookupType("missing", "x") != nil {
		t.Errorf("LookupType on missing pkg: want nil")
	}
	if s.LookupMethod("missing", "x", "y") != nil {
		t.Errorf("LookupMethod on missing pkg: want nil")
	}
}

func TestWalkTypes(t *testing.T) {
	mapT := MapType{
		Key:   BasicType{Name: "string"},
		Value: SliceType{Elem: NamedType{PackagePath: "io", Name: "Reader"}},
	}
	count := 0
	_ = walkTypes(mapT, func(t GoType) error {
		count++
		return nil
	})
	// MapType + key Basic + value Slice + slice elem Named = 4 nodes.
	if count != 4 {
		t.Errorf("walkTypes node count = %d, want 4", count)
	}
}
