package generics_test

import (
	"testing"

	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/generics"
	"mochi/package3/dotnet/metacli"
	"mochi/package3/dotnet/typemap"
)

// ---------- ParseMonoTable ----------

func TestParseMonoTable_empty(t *testing.T) {
	tbl, err := generics.ParseMonoTable("")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tbl == nil {
		t.Fatal("expected non-nil table")
	}
	if len(tbl.Entries) != 0 {
		t.Errorf("expected 0 entries, got %d", len(tbl.Entries))
	}
}

func TestParseMonoTable_noMonomorphiseKey(t *testing.T) {
	toml := `[dotnet]
registry = "https://api.nuget.org/v3/index.json"
`
	tbl, err := generics.ParseMonoTable(toml)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(tbl.Entries) != 0 {
		t.Errorf("expected 0 entries for missing key, got %d", len(tbl.Entries))
	}
}

func TestParseMonoTable_singleEntry(t *testing.T) {
	toml := `monomorphise = [
    { item = "System.Linq.Enumerable.Select", type_args = ["System.String", "System.Int32"] }
]`
	tbl, err := generics.ParseMonoTable(toml)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(tbl.Entries) != 1 {
		t.Fatalf("expected 1 entry, got %d", len(tbl.Entries))
	}
	e := tbl.Entries[0]
	if e.Item != "System.Linq.Enumerable.Select" {
		t.Errorf("expected item name, got %s", e.Item)
	}
	if len(e.TypeArgs) != 2 {
		t.Fatalf("expected 2 type args, got %d", len(e.TypeArgs))
	}
	if e.TypeArgs[0] != "System.String" {
		t.Errorf("expected System.String, got %s", e.TypeArgs[0])
	}
	if e.TypeArgs[1] != "System.Int32" {
		t.Errorf("expected System.Int32, got %s", e.TypeArgs[1])
	}
}

func TestParseMonoTable_multipleEntries(t *testing.T) {
	toml := `monomorphise = [
    { item = "System.Collections.Generic.List.Add", type_args = ["System.String"] },
    { item = "System.Collections.Generic.List.Get", type_args = ["System.Int32"] }
]`
	tbl, err := generics.ParseMonoTable(toml)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(tbl.Entries) != 2 {
		t.Fatalf("expected 2 entries, got %d", len(tbl.Entries))
	}
}

func TestParseMonoTable_missingItemKey(t *testing.T) {
	toml := `monomorphise = [
    { type_args = ["System.String"] }
]`
	_, err := generics.ParseMonoTable(toml)
	if err == nil {
		t.Fatal("expected error for missing item key")
	}
}

func TestParseMonoTable_missingTypeArgs(t *testing.T) {
	toml := `monomorphise = [
    { item = "Foo.Bar.Method" }
]`
	_, err := generics.ParseMonoTable(toml)
	if err == nil {
		t.Fatal("expected error for missing type_args")
	}
}

// ---------- Instantiate ----------

func TestInstantiate_hit(t *testing.T) {
	tbl := &generics.MonoTable{
		Entries: []generics.MonoEntry{
			{Item: "System.Linq.Enumerable.Select", TypeArgs: []string{"System.String", "System.Int32"}},
		},
	}
	args, ok := tbl.Instantiate("System.Linq.Enumerable.Select")
	if !ok {
		t.Fatal("expected hit")
	}
	if len(args) != 2 {
		t.Fatalf("expected 2 type args, got %d", len(args))
	}
}

func TestInstantiate_miss(t *testing.T) {
	tbl := &generics.MonoTable{
		Entries: []generics.MonoEntry{
			{Item: "Foo.Bar.Method", TypeArgs: []string{"System.String"}},
		},
	}
	_, ok := tbl.Instantiate("NotInTable.Method")
	if ok {
		t.Fatal("expected miss")
	}
}

func TestInstantiate_nilTable(t *testing.T) {
	var tbl *generics.MonoTable
	_, ok := tbl.Instantiate("anything")
	if ok {
		t.Fatal("expected miss for nil table")
	}
}

func TestInstantiate_emptyTable(t *testing.T) {
	tbl := &generics.MonoTable{}
	_, ok := tbl.Instantiate("anything")
	if ok {
		t.Fatal("expected miss for empty table")
	}
}

// ---------- MonomorphiseMethod ----------

func makeGenericMethodDef() metacli.MethodDef {
	return metacli.MethodDef{
		Name:       "Select",
		IsStatic:   true,
		IsGeneric:  true,
		TypeParams: []string{"TSource", "TResult"},
		ReturnType: metacli.TypeRef{
			FullName: "TResult",
			Kind:     metacli.TypeRefGenericParam,
		},
		Parameters: []metacli.ParamDef{
			{
				Name: "input",
				Type: metacli.TypeRef{
					FullName: "TSource",
					Kind:     metacli.TypeRefGenericParam,
				},
			},
		},
	}
}

func TestMonomorphiseMethod_success(t *testing.T) {
	tbl := &generics.MonoTable{
		Entries: []generics.MonoEntry{
			{
				Item:     "System.Linq.Enumerable.Select",
				TypeArgs: []string{"System.String", "System.String"},
			},
		},
	}
	method := makeGenericMethodDef()
	sm, ok := generics.MonomorphiseMethod("System.Linq.Enumerable", "Select", method, tbl, typemap.DirectionOut)
	if !ok {
		t.Fatal("expected successful monomorphisation")
	}
	if sm.EntryPoint == "" {
		t.Error("expected non-empty entry point")
	}
	if sm.CSMethodName == "" {
		t.Error("expected non-empty CSMethodName")
	}
	if len(sm.Params) != 1 {
		t.Errorf("expected 1 param, got %d", len(sm.Params))
	}
}

func TestMonomorphiseMethod_missingEntry(t *testing.T) {
	tbl := &generics.MonoTable{}
	method := makeGenericMethodDef()
	_, ok := generics.MonomorphiseMethod("Foo.Bar", "Method", method, tbl, typemap.DirectionOut)
	if ok {
		t.Fatal("expected miss for missing entry")
	}
}

func TestMonomorphiseMethod_entryPointContainsTypeArg(t *testing.T) {
	tbl := &generics.MonoTable{
		Entries: []generics.MonoEntry{
			{
				Item:     "MyNS.MyClass.GetAsync",
				TypeArgs: []string{"System.String"},
			},
		},
	}
	method := metacli.MethodDef{
		Name:       "GetAsync",
		TypeParams: []string{"T"},
		ReturnType: metacli.TypeRef{FullName: "T", Kind: metacli.TypeRefGenericParam},
		IsAsync:    true,
	}
	sm, ok := generics.MonomorphiseMethod("MyNS.MyClass", "GetAsync", method, tbl, typemap.DirectionOut)
	if !ok {
		t.Fatal("expected successful monomorphisation")
	}
	if sm.IsAsync != true {
		t.Error("expected IsAsync preserved")
	}
}

// ---------- ApplyToSurface ----------

func makeGenericSurface() *metacli.ApiSurface {
	return &metacli.ApiSurface{
		Assembly: "TestAssembly",
		Types: []metacli.SurfaceType{
			{
				// Type with a generic method that IS in the MonoTable.
				FullName:  "MyNS.GenericType",
				ShortName: "GenericType",
				Methods: []metacli.SurfaceMethod{
					{
						Name: "Select",
						ReturnType: metacli.TypeRef{
							FullName: "TResult",
							Kind:     metacli.TypeRefGenericParam,
						},
						Params: []metacli.ParamDef{
							{
								Name: "item",
								Type: metacli.TypeRef{FullName: "TSource", Kind: metacli.TypeRefGenericParam},
							},
						},
					},
				},
			},
			{
				// Type with a generic method that is NOT in the MonoTable.
				FullName:  "MyNS.OtherGeneric",
				ShortName: "OtherGeneric",
				Methods: []metacli.SurfaceMethod{
					{
						Name: "Transform",
						ReturnType: metacli.TypeRef{
							FullName: "T",
							Kind:     metacli.TypeRefGenericParam,
						},
					},
				},
			},
			{
				// Non-generic type: passes through unchanged.
				FullName:  "MyNS.PlainType",
				ShortName: "PlainType",
				Methods: []metacli.SurfaceMethod{
					{
						Name: "DoWork",
						ReturnType: metacli.TypeRef{
							FullName: "System.String",
							Kind:     metacli.TypeRefPrimitive,
						},
					},
				},
			},
		},
	}
}

func TestApplyToSurface_nil(t *testing.T) {
	tbl := &generics.MonoTable{}
	out, skipped := generics.ApplyToSurface(nil, tbl)
	if out == nil {
		t.Fatal("expected non-nil output")
	}
	if len(skipped) != 0 {
		t.Errorf("expected 0 skipped, got %d", len(skipped))
	}
}

func TestApplyToSurface_genericInTable_notSkipped(t *testing.T) {
	tbl := &generics.MonoTable{
		Entries: []generics.MonoEntry{
			{Item: "MyNS.GenericType.Select", TypeArgs: []string{"System.String", "System.String"}},
		},
	}
	surface := makeGenericSurface()
	out, skipped := generics.ApplyToSurface(surface, tbl)

	// The OtherGeneric.Transform should be skipped.
	var foundSkip bool
	for _, s := range skipped {
		if s.ItemPath == "MyNS.OtherGeneric.Transform" {
			foundSkip = true
			if s.Reason != errors.SkipGeneric {
				t.Errorf("expected SkipGeneric, got %v", s.Reason)
			}
		}
	}
	if !foundSkip {
		t.Error("expected OtherGeneric.Transform in skipped list")
	}

	// GenericType.Select should NOT be skipped.
	for _, s := range skipped {
		if s.ItemPath == "MyNS.GenericType.Select" {
			t.Error("GenericType.Select should not be in skipped list (it has a MonoTable entry)")
		}
	}

	// PlainType should appear with its method intact.
	var foundPlain bool
	for _, st := range out.Types {
		if st.FullName == "MyNS.PlainType" {
			foundPlain = true
			if len(st.Methods) != 1 {
				t.Errorf("PlainType: expected 1 method, got %d", len(st.Methods))
			}
		}
	}
	if !foundPlain {
		t.Error("expected PlainType in output")
	}
}

func TestApplyToSurface_allGenericNoTable_allSkipped(t *testing.T) {
	tbl := &generics.MonoTable{} // empty
	surface := &metacli.ApiSurface{
		Types: []metacli.SurfaceType{
			{
				FullName: "Foo.Bar",
				Methods: []metacli.SurfaceMethod{
					{
						Name: "Process",
						ReturnType: metacli.TypeRef{
							FullName: "T",
							Kind:     metacli.TypeRefGenericParam,
						},
					},
				},
			},
		},
	}
	_, skipped := generics.ApplyToSurface(surface, tbl)
	if len(skipped) != 1 {
		t.Fatalf("expected 1 skip, got %d", len(skipped))
	}
	if skipped[0].Reason != errors.SkipGeneric {
		t.Errorf("expected SkipGeneric, got %v", skipped[0].Reason)
	}
}

func TestApplyToSurface_skippedContainsOverride(t *testing.T) {
	tbl := &generics.MonoTable{}
	surface := &metacli.ApiSurface{
		Types: []metacli.SurfaceType{
			{
				FullName: "Foo.Bar",
				Methods: []metacli.SurfaceMethod{
					{
						Name:       "GenMethod",
						ReturnType: metacli.TypeRef{FullName: "T", Kind: metacli.TypeRefGenericParam},
					},
				},
			},
		},
	}
	_, skipped := generics.ApplyToSurface(surface, tbl)
	if len(skipped) == 0 {
		t.Fatal("expected skipped entries")
	}
	if skipped[0].Override == "" {
		t.Error("expected non-empty Override hint in SkipReport")
	}
}
