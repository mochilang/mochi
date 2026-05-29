package shimgen_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/errors"
	"mochi/package3/dotnet/metacli"
	"mochi/package3/dotnet/shimgen"
)

// staticSurface returns a hand-crafted ApiSurface for testing.
// It contains:
//   - A static class with 3 static methods (string->string, int->bool, async string->string)
//   - A non-static class with 2 instance methods
//   - A method with an un-mappable parameter (pointer type) -> should produce SkipReport
//   - A property with a getter
func staticSurface() *metacli.ApiSurface {
	return &metacli.ApiSurface{
		Assembly:        "TestLib",
		Version:         "1.0.0",
		TargetFramework: "net8.0",
		Types: []metacli.SurfaceType{
			{
				FullName:  "TestLib.StringHelper",
				ShortName: "StringHelper",
				Namespace: "TestLib",
				Kind:      metacli.KindClass,
				IsStatic:  true,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "Reverse",
						IsStatic: true,
						ReturnType: metacli.TypeRef{
							FullName: "System.String",
							Kind:     metacli.TypeRefPrimitive,
						},
						Params: []metacli.ParamDef{
							{
								Name: "input",
								Type: metacli.TypeRef{FullName: "System.String", Kind: metacli.TypeRefPrimitive},
							},
						},
					},
					{
						Name:     "IsEmpty",
						IsStatic: true,
						ReturnType: metacli.TypeRef{
							FullName: "System.Boolean",
							Kind:     metacli.TypeRefPrimitive,
						},
						Params: []metacli.ParamDef{
							{
								Name: "value",
								Type: metacli.TypeRef{FullName: "System.Int32", Kind: metacli.TypeRefPrimitive},
							},
						},
					},
					{
						Name:    "FetchAsync",
						IsStatic: true,
						IsAsync:  true,
						ReturnType: metacli.TypeRef{
							FullName: "System.Threading.Tasks.Task`1",
							Kind:     metacli.TypeRefGenericInst,
							TypeArgs: []metacli.TypeRef{
								{FullName: "System.String", Kind: metacli.TypeRefPrimitive},
							},
						},
						Params: []metacli.ParamDef{
							{
								Name: "url",
								Type: metacli.TypeRef{FullName: "System.String", Kind: metacli.TypeRefPrimitive},
							},
						},
					},
				},
			},
			{
				FullName:  "TestLib.Processor",
				ShortName: "Processor",
				Namespace: "TestLib",
				Kind:      metacli.KindClass,
				IsStatic:  false,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "Process",
						IsStatic: false,
						ReturnType: metacli.TypeRef{
							FullName: "System.Int32",
							Kind:     metacli.TypeRefPrimitive,
						},
						Params: []metacli.ParamDef{
							{
								Name: "data",
								Type: metacli.TypeRef{FullName: "System.String", Kind: metacli.TypeRefPrimitive},
							},
						},
					},
					{
						Name:     "Reset",
						IsStatic: false,
						ReturnType: metacli.TypeRef{
							FullName: "System.Void",
							Kind:     metacli.TypeRefVoid,
						},
						Params: nil,
					},
				},
				Properties: []metacli.SurfaceProp{
					{
						Name:      "Count",
						HasGetter: true,
						IsStatic:  false,
						Type: metacli.TypeRef{
							FullName: "System.Int32",
							Kind:     metacli.TypeRefPrimitive,
						},
					},
				},
			},
			{
				FullName:  "TestLib.Dangerous",
				ShortName: "Dangerous",
				Namespace: "TestLib",
				Kind:      metacli.KindClass,
				IsStatic:  true,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "UnsafeOp",
						IsStatic: true,
						ReturnType: metacli.TypeRef{
							FullName: "System.Void",
							Kind:     metacli.TypeRefVoid,
						},
						Params: []metacli.ParamDef{
							{
								Name: "ptr",
								Type: metacli.TypeRef{FullName: "System.IntPtr", Kind: metacli.TypeRefPointer},
							},
						},
					},
				},
			},
		},
	}
}

func TestSynthEntryPointNames(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	// Verify we can find entry points for the static class methods.
	found := map[string]bool{}
	for _, m := range s.Methods {
		found[m.EntryPoint] = true
	}

	// Entry points should be lowercase with underscores.
	for ep := range found {
		if ep != strings.ToLower(ep) {
			t.Errorf("entry point %q is not lowercase", ep)
		}
		if !strings.HasPrefix(ep, "mochi_") {
			t.Errorf("entry point %q missing mochi_ prefix", ep)
		}
	}
}

func TestSynthMethodCount(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	// Expected: Reverse, IsEmpty, FetchAsync, Process, Reset, get_Count = 6 methods.
	// UnsafeOp should be skipped.
	want := 6
	if len(s.Methods) != want {
		t.Errorf("Synth produced %d methods; want %d\nmethods: %v", len(s.Methods), want, methodNames(s))
	}
}

func TestSynthSkippedPopulated(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	if len(s.Skipped) == 0 {
		t.Fatal("expected at least one SkipReport for UnsafeOp")
	}
	found := false
	for _, sr := range s.Skipped {
		if strings.Contains(sr.ItemPath, "UnsafeOp") {
			found = true
			if sr.Reason != errors.SkipPointer {
				t.Errorf("UnsafeOp reason = %v; want SkipPointer", sr.Reason)
			}
		}
	}
	if !found {
		t.Errorf("no SkipReport for UnsafeOp; skipped: %v", s.Skipped)
	}
}

func TestSynthIsAsyncSet(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	found := false
	for _, m := range s.Methods {
		if strings.Contains(m.EntryPoint, "fetchasync") {
			found = true
			if !m.IsAsync {
				t.Errorf("FetchAsync method: IsAsync = false; want true")
			}
			// The return type should be the unwrapped string, not Task<string>.
			if m.Return == nil {
				t.Errorf("FetchAsync method: Return is nil; want string mapping")
			}
		}
	}
	if !found {
		t.Errorf("FetchAsync method not found in shim methods")
	}
}

func TestSynthNonAsyncNotMarked(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	for _, m := range s.Methods {
		if strings.Contains(m.EntryPoint, "reverse") {
			if m.IsAsync {
				t.Errorf("Reverse method marked as async; want false")
			}
		}
	}
}

func TestSynthPropertyGetterEntryPoint(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	found := false
	for _, m := range s.Methods {
		if strings.Contains(m.EntryPoint, "get_") || strings.Contains(m.CSMethodName, "get_") {
			found = true
		}
	}
	if !found {
		t.Errorf("property getter method not found in shim methods\nmethods: %v", methodNames(s))
	}
}

func TestSynthVoidReturnIsNil(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)

	for _, m := range s.Methods {
		if strings.Contains(m.EntryPoint, "_reset") {
			if m.Return != nil {
				t.Errorf("Reset method: Return should be nil for void, got %v", m.Return)
			}
		}
	}
}

func TestSynthAbstractClassSkipped(t *testing.T) {
	surface := &metacli.ApiSurface{
		Types: []metacli.SurfaceType{
			{
				FullName:   "TestLib.AbstractBase",
				ShortName:  "AbstractBase",
				Kind:       metacli.KindClass,
				IsAbstract: true,
				IsStatic:   false,
			},
		},
	}
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)
	if len(s.Methods) != 0 {
		t.Errorf("abstract class should produce no methods; got %d", len(s.Methods))
	}
	if len(s.Skipped) == 0 {
		t.Error("abstract class should produce a SkipReport")
	}
	if s.Skipped[0].Reason != errors.SkipAbstract {
		t.Errorf("abstract skip reason = %v; want SkipAbstract", s.Skipped[0].Reason)
	}
}

func TestSynthRefParamSkipped(t *testing.T) {
	surface := &metacli.ApiSurface{
		Types: []metacli.SurfaceType{
			{
				FullName:  "TestLib.RefUser",
				ShortName: "RefUser",
				Kind:      metacli.KindClass,
				IsStatic:  true,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "WithRef",
						IsStatic: true,
						ReturnType: metacli.TypeRef{FullName: "System.Void", Kind: metacli.TypeRefVoid},
						Params: []metacli.ParamDef{
							{Name: "x", Type: metacli.TypeRef{FullName: "System.Int32", Kind: metacli.TypeRefPrimitive}, IsRef: true},
						},
					},
				},
			},
		},
	}
	s := shimgen.Synth("TestLib", "1.0.0", "net8.0", surface)
	if len(s.Methods) != 0 {
		t.Errorf("ref param method should be skipped; got %d methods", len(s.Methods))
	}
	if len(s.Skipped) == 0 {
		t.Error("ref param method should produce a SkipReport")
	}
}

func TestSynthPackageFields(t *testing.T) {
	surface := staticSurface()
	s := shimgen.Synth("My.Package", "2.3.4", "net9.0", surface)
	if s.Package != "My.Package" {
		t.Errorf("Package = %q; want My.Package", s.Package)
	}
	if s.PackageVersion != "2.3.4" {
		t.Errorf("PackageVersion = %q; want 2.3.4", s.PackageVersion)
	}
	if s.TargetFramework != "net9.0" {
		t.Errorf("TargetFramework = %q; want net9.0", s.TargetFramework)
	}
}

func TestSynthNilSurface(t *testing.T) {
	s := shimgen.Synth("pkg", "1.0.0", "net8.0", nil)
	if s == nil {
		t.Fatal("Synth(nil surface) returned nil")
	}
	if len(s.Methods) != 0 {
		t.Errorf("nil surface: expected 0 methods; got %d", len(s.Methods))
	}
}

func methodNames(s *shimgen.Shim) []string {
	names := make([]string, len(s.Methods))
	for i, m := range s.Methods {
		names[i] = m.EntryPoint
	}
	return names
}
