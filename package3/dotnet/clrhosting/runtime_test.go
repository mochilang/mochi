package clrhosting_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/clrhosting"
	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

func TestRuntimeConfigJSON_net6(t *testing.T) {
	cfg := clrhosting.Config{TargetFramework: "net6.0"}
	got := clrhosting.RuntimeConfigJSON(cfg)
	if !strings.Contains(got, `"version": "6.0.0"`) {
		t.Errorf("expected 6.0.0, got: %s", got)
	}
	if !strings.Contains(got, `"tfm": "net6.0"`) {
		t.Errorf("expected tfm net6.0, got: %s", got)
	}
}

func TestRuntimeConfigJSON_net7(t *testing.T) {
	cfg := clrhosting.Config{TargetFramework: "net7.0"}
	got := clrhosting.RuntimeConfigJSON(cfg)
	if !strings.Contains(got, `"version": "7.0.0"`) {
		t.Errorf("expected 7.0.0, got: %s", got)
	}
}

func TestRuntimeConfigJSON_net8(t *testing.T) {
	cfg := clrhosting.Config{TargetFramework: "net8.0"}
	got := clrhosting.RuntimeConfigJSON(cfg)
	if !strings.Contains(got, `"version": "8.0.0"`) {
		t.Errorf("expected 8.0.0, got: %s", got)
	}
}

func TestRuntimeConfigJSON_net9(t *testing.T) {
	cfg := clrhosting.Config{TargetFramework: "net9.0"}
	got := clrhosting.RuntimeConfigJSON(cfg)
	if !strings.Contains(got, `"version": "9.0.0"`) {
		t.Errorf("expected 9.0.0, got: %s", got)
	}
}

func TestRuntimeConfigJSON_MicrosoftNETCoreApp(t *testing.T) {
	cfg := clrhosting.Config{TargetFramework: "net8.0"}
	got := clrhosting.RuntimeConfigJSON(cfg)
	if !strings.Contains(got, `"name": "Microsoft.NETCore.App"`) {
		t.Errorf("missing Microsoft.NETCore.App, got: %s", got)
	}
}

func TestRuntimeConfigJSON_runtimeOptions(t *testing.T) {
	cfg := clrhosting.Config{TargetFramework: "net8.0"}
	got := clrhosting.RuntimeConfigJSON(cfg)
	if !strings.Contains(got, `"runtimeOptions"`) {
		t.Errorf("missing runtimeOptions, got: %s", got)
	}
}

func TestCgoHeader_containsStruct(t *testing.T) {
	cfg := clrhosting.Config{
		Package:          "Newtonsoft.Json",
		MarshalFreeEntry: "mochi_marshal_free",
	}
	methods := []clrhosting.MethodBinding{
		{EntryPoint: "mochi_newtonsoftjson_JsonConvert_SerializeObject", CReturnType: "MochiString", CParams: "MochiString value"},
	}
	got := clrhosting.CgoHeader(cfg, methods)
	if !strings.Contains(got, "typedef struct { unsigned char* Ptr; int Len; } MochiString;") {
		t.Errorf("missing MochiString struct: %s", got)
	}
}

func TestCgoHeader_containsFreeEntry(t *testing.T) {
	cfg := clrhosting.Config{MarshalFreeEntry: "mochi_marshal_free"}
	got := clrhosting.CgoHeader(cfg, nil)
	if !strings.Contains(got, "extern void mochi_marshal_free(intptr_t ptr);") {
		t.Errorf("missing free entry: %s", got)
	}
}

func TestCgoHeader_defaultFreeEntry(t *testing.T) {
	cfg := clrhosting.Config{}
	got := clrhosting.CgoHeader(cfg, nil)
	if !strings.Contains(got, "mochi_marshal_free") {
		t.Errorf("missing default free entry: %s", got)
	}
}

func TestCgoHeader_entryPoints(t *testing.T) {
	cfg := clrhosting.Config{MarshalFreeEntry: "mochi_marshal_free"}
	methods := []clrhosting.MethodBinding{
		{EntryPoint: "mochi_pkg_Foo_Bar", CReturnType: "int32_t", CParams: "uint8_t x"},
		{EntryPoint: "mochi_pkg_Baz_Qux", CReturnType: "void", CParams: ""},
	}
	got := clrhosting.CgoHeader(cfg, methods)
	if !strings.Contains(got, "mochi_pkg_Foo_Bar") {
		t.Errorf("missing Foo_Bar: %s", got)
	}
	if !strings.Contains(got, "mochi_pkg_Baz_Qux") {
		t.Errorf("missing Baz_Qux: %s", got)
	}
}

func TestCgoHeader_importC(t *testing.T) {
	cfg := clrhosting.Config{}
	got := clrhosting.CgoHeader(cfg, nil)
	if !strings.Contains(got, `import "C"`) {
		t.Errorf(`missing import "C": %s`, got)
	}
}

func TestCgoHeader_cgoLDFLAGS(t *testing.T) {
	cfg := clrhosting.Config{}
	got := clrhosting.CgoHeader(cfg, nil)
	if !strings.Contains(got, "#cgo LDFLAGS: -ldl") {
		t.Errorf("missing LDFLAGS: %s", got)
	}
}

func TestBindingsFromShim_nil(t *testing.T) {
	got := clrhosting.BindingsFromShim(nil)
	if len(got) != 0 {
		t.Errorf("expected empty, got %d", len(got))
	}
}

func TestBindingsFromShim_empty(t *testing.T) {
	s := &shimgen.Shim{}
	got := clrhosting.BindingsFromShim(s)
	if len(got) != 0 {
		t.Errorf("expected empty, got %d", len(got))
	}
}

func TestBindingsFromShim_stringReturn(t *testing.T) {
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{
				EntryPoint:   "mochi_pkg_Type_Method",
				CSMethodName: "Type_Method",
				Return:       &typemap.Mapping{Kind: typemap.KindString},
			},
		},
	}
	got := clrhosting.BindingsFromShim(s)
	if len(got) != 1 {
		t.Fatalf("expected 1, got %d", len(got))
	}
	if got[0].CReturnType != "MochiString" {
		t.Errorf("expected MochiString, got %s", got[0].CReturnType)
	}
}

func TestBindingsFromShim_intReturn(t *testing.T) {
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{
				EntryPoint:   "mochi_pkg_Type_Method",
				CSMethodName: "Type_Method",
				Return:       &typemap.Mapping{Kind: typemap.KindInt},
			},
		},
	}
	got := clrhosting.BindingsFromShim(s)
	if got[0].CReturnType != "int32_t" {
		t.Errorf("expected int32_t, got %s", got[0].CReturnType)
	}
}

func TestBindingsFromShim_voidReturn(t *testing.T) {
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{
				EntryPoint:   "mochi_pkg_Type_Void",
				CSMethodName: "Type_Void",
				Return:       nil,
			},
		},
	}
	got := clrhosting.BindingsFromShim(s)
	if got[0].CReturnType != "void" {
		t.Errorf("expected void, got %s", got[0].CReturnType)
	}
}

func TestCTypeFor_kinds(t *testing.T) {
	tests := []struct {
		kind     typemap.Kind
		expected string
	}{
		{typemap.KindBool, "uint8_t"},
		{typemap.KindByte, "uint8_t"},
		{typemap.KindInt, "int32_t"},
		{typemap.KindInt64, "int64_t"},
		{typemap.KindUInt, "uint32_t"},
		{typemap.KindUInt64, "uint64_t"},
		{typemap.KindFloat, "float"},
		{typemap.KindFloat64, "double"},
		{typemap.KindChar, "uint16_t"},
		{typemap.KindString, "MochiString"},
		{typemap.KindBytes, "MochiString"},
		{typemap.KindUnit, "void"},
		{typemap.KindHandle, "intptr_t"},
		{typemap.KindRecord, "intptr_t"},
		{typemap.KindList, "intptr_t"},
		{typemap.KindMap, "intptr_t"},
		{typemap.KindSet, "intptr_t"},
		{typemap.KindEnum, "int32_t"},
		{typemap.KindOption, "intptr_t"},
		{typemap.KindTuple, "intptr_t"},
	}
	for _, tt := range tests {
		m := &typemap.Mapping{Kind: tt.kind}
		got := clrhosting.CTypeFor(m)
		if got != tt.expected {
			t.Errorf("CTypeFor(%v) = %q, want %q", tt.kind, got, tt.expected)
		}
	}
}

func TestCTypeFor_nil(t *testing.T) {
	got := clrhosting.CTypeFor(nil)
	if got != "void" {
		t.Errorf("expected void for nil, got %s", got)
	}
}

func TestEmitGoFile_packageName(t *testing.T) {
	cfg := clrhosting.Config{
		Package:          "Newtonsoft.Json",
		PackageVersion:   "13.0.3",
		TargetFramework:  "net8.0",
		ShimAssemblyName: "MochiShim.NewtonsoftJson",
		ShimDir:          "dotnet_shim/NewtonsoftJson",
		MarshalFreeEntry: "mochi_marshal_free",
	}
	got := clrhosting.EmitGoFile("dotnet_bridge_newtonsoftjson", cfg, nil)
	if !strings.Contains(got, "package dotnet_bridge_newtonsoftjson") {
		t.Errorf("missing package declaration: %s", got)
	}
}

func TestEmitGoFile_initFunction(t *testing.T) {
	cfg := clrhosting.Config{
		Package:          "Newtonsoft.Json",
		PackageVersion:   "13.0.3",
		TargetFramework:  "net8.0",
		ShimAssemblyName: "MochiShim.NewtonsoftJson",
		ShimDir:          "dotnet_shim/NewtonsoftJson",
		MarshalFreeEntry: "mochi_marshal_free",
	}
	got := clrhosting.EmitGoFile("dotnet_bridge_newtonsoftjson", cfg, nil)
	if !strings.Contains(got, "func init()") {
		t.Errorf("missing init(): %s", got)
	}
	if !strings.Contains(got, "loadShim") {
		t.Errorf("missing loadShim call: %s", got)
	}
}

func TestEmitGoFile_codeGenComment(t *testing.T) {
	cfg := clrhosting.Config{
		Package:          "Newtonsoft.Json",
		PackageVersion:   "13.0.3",
		TargetFramework:  "net8.0",
		ShimAssemblyName: "MochiShim.NewtonsoftJson",
		ShimDir:          "dotnet_shim/NewtonsoftJson",
		MarshalFreeEntry: "mochi_marshal_free",
	}
	got := clrhosting.EmitGoFile("dotnet_bridge_newtonsoftjson", cfg, nil)
	if !strings.Contains(got, "do not edit") {
		t.Errorf("missing do-not-edit comment: %s", got)
	}
}

func TestEmitGoFile_callFunction(t *testing.T) {
	cfg := clrhosting.Config{
		Package:          "Newtonsoft.Json",
		PackageVersion:   "13.0.3",
		TargetFramework:  "net8.0",
		ShimAssemblyName: "MochiShim.NewtonsoftJson",
		ShimDir:          "dotnet_shim/NewtonsoftJson",
		MarshalFreeEntry: "mochi_marshal_free",
	}
	methods := []clrhosting.MethodBinding{
		{
			EntryPoint:   "mochi_newtonsoftjson_JsonConvert_SerializeObject",
			CReturnType:  "MochiString",
			CParams:      "MochiString value",
			GoFuncName:   "CallJsonConvert_SerializeObject",
			GoParams:     "value MochiString",
			GoReturnType: "MochiString",
		},
	}
	got := clrhosting.EmitGoFile("dotnet_bridge_newtonsoftjson", cfg, methods)
	if !strings.Contains(got, "func CallJsonConvert_SerializeObject") {
		t.Errorf("missing CallJsonConvert_SerializeObject: %s", got)
	}
	if !strings.Contains(got, "C.mochi_newtonsoftjson_JsonConvert_SerializeObject") {
		t.Errorf("missing C call: %s", got)
	}
}

func TestEmitGoFile_shimDLLPath(t *testing.T) {
	cfg := clrhosting.Config{
		Package:          "Newtonsoft.Json",
		PackageVersion:   "13.0.3",
		TargetFramework:  "net8.0",
		ShimAssemblyName: "MochiShim.NewtonsoftJson",
		ShimDir:          "dotnet_shim/NewtonsoftJson",
		MarshalFreeEntry: "mochi_marshal_free",
	}
	got := clrhosting.EmitGoFile("dotnet_bridge_newtonsoftjson", cfg, nil)
	if !strings.Contains(got, "MochiShim.NewtonsoftJson.dll") {
		t.Errorf("missing dll path: %s", got)
	}
	if !strings.Contains(got, "MochiShim.NewtonsoftJson.runtimeconfig.json") {
		t.Errorf("missing runtimeconfig path: %s", got)
	}
}
