package nativeaot_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/nativeaot"
	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

// ---------- KnownCompatibility: all 20 packages ----------

func TestKnownCompatibility_SystemTextJson(t *testing.T) {
	if got := nativeaot.KnownCompatibility("System.Text.Json", ""); got != nativeaot.CompatFull {
		t.Errorf("expected CompatFull, got %v", got)
	}
}

func TestKnownCompatibility_MicrosoftExtensionsDI(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Microsoft.Extensions.DependencyInjection", ""); got != nativeaot.CompatFull {
		t.Errorf("expected CompatFull, got %v", got)
	}
}

func TestKnownCompatibility_MicrosoftExtensionsHttp(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Microsoft.Extensions.Http", ""); got != nativeaot.CompatFull {
		t.Errorf("expected CompatFull, got %v", got)
	}
}

func TestKnownCompatibility_NewtonsoftJson_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Newtonsoft.Json", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_Serilog_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Serilog", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_Dapper_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Dapper", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_NUnit_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("NUnit", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_xUnit_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("xUnit", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_FluentAssertions_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("FluentAssertions", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_AutoMapper_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("AutoMapper", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_MediatR_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("MediatR", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_FluentValidation_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("FluentValidation", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_Polly_Full(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Polly", ""); got != nativeaot.CompatFull {
		t.Errorf("expected CompatFull, got %v", got)
	}
}

func TestKnownCompatibility_Bogus_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Bogus", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_Moq_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Moq", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_RestSharp_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("RestSharp", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_StackExchangeRedis_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("StackExchange.Redis", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_Npgsql_Full(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Npgsql", ""); got != nativeaot.CompatFull {
		t.Errorf("expected CompatFull, got %v", got)
	}
}

func TestKnownCompatibility_AWSSdk_Partial(t *testing.T) {
	if got := nativeaot.KnownCompatibility("AWSSDK.Core", ""); got != nativeaot.CompatPartial {
		t.Errorf("expected CompatPartial, got %v", got)
	}
}

func TestKnownCompatibility_EFCore_Incompatible(t *testing.T) {
	if got := nativeaot.KnownCompatibility("Microsoft.EntityFrameworkCore", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestKnownCompatibility_Unknown(t *testing.T) {
	if got := nativeaot.KnownCompatibility("SomeRandomPackage.Unknown", "1.0.0"); got != nativeaot.CompatUnknown {
		t.Errorf("expected CompatUnknown, got %v", got)
	}
}

func TestKnownCompatibility_caseInsensitive(t *testing.T) {
	if got := nativeaot.KnownCompatibility("newtonsoft.json", ""); got != nativeaot.CompatIncompatible {
		t.Errorf("case-insensitive lookup failed, got %v", got)
	}
}

// ---------- Compatibility.String ----------

func TestCompatibility_String(t *testing.T) {
	cases := []struct {
		c    nativeaot.Compatibility
		want string
	}{
		{nativeaot.CompatUnknown, "CompatUnknown"},
		{nativeaot.CompatFull, "CompatFull"},
		{nativeaot.CompatPartial, "CompatPartial"},
		{nativeaot.CompatIncompatible, "CompatIncompatible"},
	}
	for _, tc := range cases {
		if got := tc.c.String(); got != tc.want {
			t.Errorf("String() = %q, want %q", got, tc.want)
		}
	}
}

// ---------- AOTCompatibilityFromNuspec ----------

func TestAOTCompatibilityFromNuspec_trueTag(t *testing.T) {
	xml := `<Project><PropertyGroup><IsAotCompatible>true</IsAotCompatible></PropertyGroup></Project>`
	if got := nativeaot.AOTCompatibilityFromNuspec(xml); got != nativeaot.CompatFull {
		t.Errorf("expected CompatFull, got %v", got)
	}
}

func TestAOTCompatibilityFromNuspec_falseTag(t *testing.T) {
	xml := `<Project><PropertyGroup><IsAotCompatible>false</IsAotCompatible></PropertyGroup></Project>`
	if got := nativeaot.AOTCompatibilityFromNuspec(xml); got != nativeaot.CompatIncompatible {
		t.Errorf("expected CompatIncompatible, got %v", got)
	}
}

func TestAOTCompatibilityFromNuspec_noTag(t *testing.T) {
	xml := `<Project><PropertyGroup></PropertyGroup></Project>`
	if got := nativeaot.AOTCompatibilityFromNuspec(xml); got != nativeaot.CompatUnknown {
		t.Errorf("expected CompatUnknown, got %v", got)
	}
}

func TestAOTCompatibilityFromNuspec_empty(t *testing.T) {
	if got := nativeaot.AOTCompatibilityFromNuspec(""); got != nativeaot.CompatUnknown {
		t.Errorf("expected CompatUnknown for empty, got %v", got)
	}
}

// ---------- EmitAOTCsproj ----------

func TestEmitAOTCsproj_containsPublishAot(t *testing.T) {
	s := &shimgen.Shim{Package: "Newtonsoft.Json", PackageVersion: "13.0.3", TargetFramework: "net8.0"}
	got := nativeaot.EmitAOTCsproj(s, "linux-x64")
	if !strings.Contains(got, "PublishAot") {
		t.Errorf("expected PublishAot in output, got: %s", got)
	}
}

func TestEmitAOTCsproj_containsRuntimeID(t *testing.T) {
	s := &shimgen.Shim{Package: "Polly", PackageVersion: "8.0.0", TargetFramework: "net8.0"}
	got := nativeaot.EmitAOTCsproj(s, "linux-x64")
	if !strings.Contains(got, "linux-x64") {
		t.Errorf("expected linux-x64 in output, got: %s", got)
	}
}

func TestEmitAOTCsproj_containsTrimmerRoot(t *testing.T) {
	s := &shimgen.Shim{Package: "Polly", PackageVersion: "8.0.0", TargetFramework: "net8.0"}
	got := nativeaot.EmitAOTCsproj(s, "")
	if !strings.Contains(got, "TrimmerRoot") {
		t.Errorf("expected TrimmerRoot in output, got: %s", got)
	}
}

func TestEmitAOTCsproj_emptyRuntimeID(t *testing.T) {
	s := &shimgen.Shim{Package: "Polly", PackageVersion: "8.0.0", TargetFramework: "net8.0"}
	got := nativeaot.EmitAOTCsproj(s, "")
	// Should not contain an empty RuntimeIdentifier element.
	if strings.Contains(got, "<RuntimeIdentifier></RuntimeIdentifier>") {
		t.Error("should not emit empty RuntimeIdentifier")
	}
}

// ---------- AOTPublishArgs ----------

func TestAOTPublishArgs_basic(t *testing.T) {
	args := nativeaot.AOTPublishArgs("linux-x64")
	if len(args) == 0 {
		t.Fatal("expected non-empty args")
	}
	if args[0] != "publish" {
		t.Errorf("expected first arg to be 'publish', got %s", args[0])
	}
}

func TestAOTPublishArgs_containsRelease(t *testing.T) {
	args := nativeaot.AOTPublishArgs("linux-x64")
	found := false
	for _, a := range args {
		if a == "Release" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected Release in args: %v", args)
	}
}

func TestAOTPublishArgs_containsRuntimeID(t *testing.T) {
	args := nativeaot.AOTPublishArgs("win-x64")
	found := false
	for _, a := range args {
		if a == "win-x64" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected win-x64 in args: %v", args)
	}
}

func TestAOTPublishArgs_containsPublishAotFlag(t *testing.T) {
	args := nativeaot.AOTPublishArgs("osx-arm64")
	found := false
	for _, a := range args {
		if strings.Contains(a, "PublishAot=true") {
			found = true
		}
	}
	if !found {
		t.Errorf("expected PublishAot=true in args: %v", args)
	}
}

func TestAOTPublishArgs_emptyRuntimeID(t *testing.T) {
	args := nativeaot.AOTPublishArgs("")
	for _, a := range args {
		if a == "-r" {
			t.Error("should not include -r flag for empty runtime ID")
		}
	}
}

// ---------- ValidateForAOT ----------

func makeHandleShim() *shimgen.Shim {
	handleMapping := typemap.Mapping{
		Kind:       typemap.KindHandle,
		CLRName:    "Newtonsoft.Json.JsonConverter",
		HandleName: "JsonConverter",
	}
	return &shimgen.Shim{
		Package: "Newtonsoft.Json",
		Methods: []shimgen.ShimMethod{
			{
				CSMethodName: "JsonConvert_Deserialize",
				Return:       &handleMapping,
			},
			{
				CSMethodName: "JsonConvert_ParseArgs",
				Return:       nil,
				Params: []shimgen.ShimParam{
					{Name: "obj", Mapping: handleMapping},
				},
			},
		},
	}
}

func TestValidateForAOT_nil(t *testing.T) {
	warnings := nativeaot.ValidateForAOT(nil)
	if len(warnings) != 0 {
		t.Errorf("expected 0 warnings for nil shim, got %d", len(warnings))
	}
}

func TestValidateForAOT_handleTypeProducesWarnings(t *testing.T) {
	s := makeHandleShim()
	warnings := nativeaot.ValidateForAOT(s)
	if len(warnings) == 0 {
		t.Fatal("expected warnings for handle-typed methods")
	}
}

func TestValidateForAOT_warningContainsMethodName(t *testing.T) {
	s := makeHandleShim()
	warnings := nativeaot.ValidateForAOT(s)
	found := false
	for _, w := range warnings {
		if w.Method == "JsonConvert_Deserialize" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected warning for JsonConvert_Deserialize, got: %v", warnings)
	}
}

func TestValidateForAOT_warningHasReason(t *testing.T) {
	s := makeHandleShim()
	warnings := nativeaot.ValidateForAOT(s)
	for _, w := range warnings {
		if w.Reason == "" {
			t.Errorf("warning for %s has empty reason", w.Method)
		}
	}
}

func TestValidateForAOT_plainScalarsNoWarnings(t *testing.T) {
	strMapping := typemap.Mapping{Kind: typemap.KindString}
	s := &shimgen.Shim{
		Methods: []shimgen.ShimMethod{
			{
				CSMethodName: "Foo_Bar",
				Return:       &strMapping,
				Params:       []shimgen.ShimParam{{Name: "x", Mapping: typemap.Mapping{Kind: typemap.KindInt}}},
			},
		},
	}
	warnings := nativeaot.ValidateForAOT(s)
	if len(warnings) != 0 {
		t.Errorf("expected 0 warnings for plain scalar types, got %d: %v", len(warnings), warnings)
	}
}
