package metacli_test

import (
	"testing"

	"mochi/package3/dotnet/metacli"
)

func makeTestMeta() *metacli.AssemblyMetadata {
	return &metacli.AssemblyMetadata{
		Assembly:        "TestLib",
		Version:         "1.0.0",
		TargetFramework: "net6.0",
		Types: []metacli.TypeDef{
			{
				Namespace: "TestLib",
				Name:      "PublicClass",
				Kind:      metacli.KindClass,
				Methods: []metacli.MethodDef{
					{Name: "DoWork", ReturnType: metacli.TypeRef{FullName: "System.Void", Kind: metacli.TypeRefVoid}},
				},
			},
			{
				Namespace:  "TestLib",
				Name:       "ObsoleteClass",
				Kind:       metacli.KindClass,
				IsObsolete: true,
			},
			{
				Namespace: "TestLib",
				Name:      "DelegateType",
				Kind:      metacli.KindDelegate,
			},
			{
				Namespace: "TestLib",
				Name:      "Outer+Nested",
				Kind:      metacli.KindClass,
			},
			{
				Namespace:  "TestLib",
				Name:       "AbstractBase",
				Kind:       metacli.KindClass,
				IsAbstract: true,
			},
			{
				Namespace: "TestLib",
				Name:      "MyEnum",
				Kind:      metacli.KindEnum,
				EnumValues: []metacli.EnumValue{
					{Name: "A", Value: 0},
				},
			},
		},
	}
}

func TestExtract_typeCount(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	// Expect: PublicClass, AbstractBase, MyEnum (not Obsolete, Delegate, Nested)
	if len(s.Types) != 3 {
		t.Errorf("got %d types; want 3", len(s.Types))
		for _, tt := range s.Types {
			t.Logf("  type: %s", tt.FullName)
		}
	}
}

func TestExtract_filtersObsolete(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.ShortName == "ObsoleteClass" {
			t.Error("ObsoleteClass should be filtered out")
		}
	}
}

func TestExtract_filtersDelegate(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.ShortName == "DelegateType" {
			t.Error("DelegateType should be filtered out")
		}
	}
}

func TestExtract_filtersNested(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.ShortName == "Outer+Nested" {
			t.Error("nested type should be filtered out")
		}
	}
}

func TestExtract_shortName(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.FullName == "TestLib.PublicClass" {
			if tt.ShortName != "PublicClass" {
				t.Errorf("ShortName = %q; want PublicClass", tt.ShortName)
			}
			return
		}
	}
	t.Error("PublicClass not found in surface")
}

func TestExtract_fullName(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.ShortName == "PublicClass" {
			if tt.FullName != "TestLib.PublicClass" {
				t.Errorf("FullName = %q; want TestLib.PublicClass", tt.FullName)
			}
			return
		}
	}
	t.Error("PublicClass not found")
}

func TestExtract_methodExtraction(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.ShortName == "PublicClass" {
			if len(tt.Methods) != 1 {
				t.Errorf("got %d methods; want 1", len(tt.Methods))
			}
			if tt.Methods[0].Name != "DoWork" {
				t.Errorf("method name = %q; want DoWork", tt.Methods[0].Name)
			}
			return
		}
	}
	t.Error("PublicClass not found")
}

func TestExtract_filtersObsoleteMethods(t *testing.T) {
	meta := &metacli.AssemblyMetadata{
		Assembly: "Lib",
		Types: []metacli.TypeDef{
			{
				Namespace: "Lib",
				Name:      "Foo",
				Kind:      metacli.KindClass,
				Methods: []metacli.MethodDef{
					{Name: "Good", ReturnType: metacli.TypeRef{FullName: "System.Void", Kind: metacli.TypeRefVoid}},
					{Name: "Old", IsObsolete: true, ReturnType: metacli.TypeRef{FullName: "System.Void", Kind: metacli.TypeRefVoid}},
				},
			},
		},
	}
	s := metacli.Extract(meta)
	if len(s.Types) != 1 {
		t.Fatalf("unexpected type count %d", len(s.Types))
	}
	for _, m := range s.Types[0].Methods {
		if m.Name == "Old" {
			t.Error("obsolete method should be filtered")
		}
	}
}

func TestExtract_filtersIndexerProperties(t *testing.T) {
	meta := &metacli.AssemblyMetadata{
		Assembly: "Lib",
		Types: []metacli.TypeDef{
			{
				Namespace: "Lib",
				Name:      "Bar",
				Kind:      metacli.KindClass,
				Properties: []metacli.PropDef{
					{Name: "Normal", Type: metacli.TypeRef{FullName: "System.Int32", Kind: metacli.TypeRefPrimitive}, HasGetter: true},
					{Name: "Item", IsIndexer: true, Type: metacli.TypeRef{FullName: "System.Int32", Kind: metacli.TypeRefPrimitive}},
				},
			},
		},
	}
	s := metacli.Extract(meta)
	if len(s.Types) != 1 {
		t.Fatalf("unexpected type count")
	}
	for _, p := range s.Types[0].Properties {
		if p.Name == "Item" {
			t.Error("indexer property should be filtered")
		}
	}
}

func TestExtract_nilMeta(t *testing.T) {
	s := metacli.Extract(nil)
	if s == nil {
		t.Fatal("Extract(nil) should return non-nil")
	}
	if len(s.Types) != 0 {
		t.Error("Extract(nil) should return empty types")
	}
}

func TestExtract_assemblyFields(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	if s.Assembly != "TestLib" {
		t.Errorf("Assembly = %q; want TestLib", s.Assembly)
	}
	if s.Version != "1.0.0" {
		t.Errorf("Version = %q; want 1.0.0", s.Version)
	}
}

func TestExtract_abstractFlagPreserved(t *testing.T) {
	meta := makeTestMeta()
	s := metacli.Extract(meta)
	for _, tt := range s.Types {
		if tt.ShortName == "AbstractBase" {
			if !tt.IsAbstract {
				t.Error("AbstractBase.IsAbstract should be true")
			}
			return
		}
	}
	t.Error("AbstractBase not found")
}
