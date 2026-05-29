package build_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/dotnet/build"
	"mochi/package3/dotnet/metacli"
)

// buildEndToEndSurface constructs a minimal ApiSurface with 5 types that
// exercise the full pipeline:
//
//  1. A static class with string->string, int->bool, and async string->string methods.
//  2. A class with 2 instance methods returning handles.
//  3. An enum type.
//  4. A struct type.
//  5. A type with a generic method (should get SkipGeneric in the shim).
func buildEndToEndSurface(pkg string) *metacli.ApiSurface {
	return &metacli.ApiSurface{
		Assembly:        pkg,
		Version:         "2.0.0",
		TargetFramework: "net8.0",
		Types: []metacli.SurfaceType{
			// 1. Static utility class.
			{
				FullName:  pkg + ".Utils",
				ShortName: "Utils",
				Namespace: pkg,
				Kind:      metacli.KindClass,
				IsStatic:  true,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "Format",
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
						Name:     "IsValid",
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
						Name:     "FetchAsync",
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
								Name: "key",
								Type: metacli.TypeRef{FullName: "System.String", Kind: metacli.TypeRefPrimitive},
							},
						},
					},
				},
			},
			// 2. Instance class with handle-returning methods.
			{
				FullName:  pkg + ".Client",
				ShortName: "Client",
				Namespace: pkg,
				Kind:      metacli.KindClass,
				IsStatic:  false,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "Connect",
						IsStatic: false,
						ReturnType: metacli.TypeRef{
							FullName: pkg + ".Connection",
							Kind:     metacli.TypeRefClass,
						},
					},
					{
						Name:     "Disconnect",
						IsStatic: false,
						ReturnType: metacli.TypeRef{
							FullName: "System.Void",
							Kind:     metacli.TypeRefVoid,
						},
					},
				},
			},
			// 3. Enum type.
			{
				FullName:  pkg + ".StatusCode",
				ShortName: "StatusCode",
				Namespace: pkg,
				Kind:      metacli.KindEnum,
			},
			// 4. Struct type.
			{
				FullName:  pkg + ".Point",
				ShortName: "Point",
				Namespace: pkg,
				Kind:      metacli.KindStruct,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "Distance",
						IsStatic: false,
						ReturnType: metacli.TypeRef{
							FullName: "System.Double",
							Kind:     metacli.TypeRefPrimitive,
						},
						Params: []metacli.ParamDef{
							{
								Name: "other",
								Type: metacli.TypeRef{FullName: "System.Double", Kind: metacli.TypeRefPrimitive},
							},
						},
					},
				},
			},
			// 5. Type with a generic method (should produce SkipGeneric).
			{
				FullName:  pkg + ".Repository",
				ShortName: "Repository",
				Namespace: pkg,
				Kind:      metacli.KindClass,
				IsStatic:  true,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "FindAll",
						IsStatic: true,
						ReturnType: metacli.TypeRef{
							FullName: "T",
							Kind:     metacli.TypeRefGenericParam,
						},
					},
					// One non-generic method to ensure not all are skipped.
					{
						Name:     "Count",
						IsStatic: true,
						ReturnType: metacli.TypeRef{
							FullName: "System.Int32",
							Kind:     metacli.TypeRefPrimitive,
						},
					},
				},
			},
		},
	}
}

func endToEndProvider() build.SurfaceProviderFunc {
	return func(id, version string) (*metacli.ApiSurface, error) {
		return buildEndToEndSurface(id), nil
	}
}

func TestPipeline_EndToEnd(t *testing.T) {
	const pkgID = "E2E.TestPackage"
	const pkgVersion = "2.0.0"

	dir := t.TempDir()
	d := build.NewDriver(dir)
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}

	p := &build.Pipeline{
		Driver:          d,
		Provider:        endToEndProvider(),
		TargetFramework: "net8.0",
	}

	refs := []build.ImportRef{
		{ID: pkgID, Version: pkgVersion, Alias: "e2e"},
	}

	// 1. Resolve.
	result, err := p.Resolve(refs)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}

	// 2. Assert basic structure.
	if len(result.Resolved) != 1 {
		t.Fatalf("expected 1 resolved package, got %d", len(result.Resolved))
	}
	rp := result.Resolved[0]

	// 3. Assert ref.
	if rp.Ref.ID != pkgID {
		t.Errorf("Ref.ID = %q, want %q", rp.Ref.ID, pkgID)
	}
	if rp.Ref.Version != pkgVersion {
		t.Errorf("Ref.Version = %q, want %q", rp.Ref.Version, pkgVersion)
	}

	// 4. Assert shim is non-nil.
	if rp.Shim == nil {
		t.Fatal("expected non-nil Shim")
	}

	// 5. Assert shim package and version.
	if rp.Shim.Package != pkgID {
		t.Errorf("Shim.Package = %q, want %q", rp.Shim.Package, pkgID)
	}
	if rp.Shim.PackageVersion != pkgVersion {
		t.Errorf("Shim.PackageVersion = %q, want %q", rp.Shim.PackageVersion, pkgVersion)
	}

	// 6. Assert methods count is reasonable (static utils + client methods + point distance + repo.Count).
	if len(rp.Shim.Methods) < 4 {
		t.Errorf("expected at least 4 shim methods, got %d", len(rp.Shim.Methods))
	}

	// 7. Assert skipped entries exist (Repository.FindAll is generic => SkipGeneric).
	if len(rp.Shim.Skipped) == 0 {
		t.Error("expected at least one skipped entry for generic method")
	}

	// 8. Find the specific SkipGeneric entry for Repository.FindAll.
	var foundSkipGeneric bool
	for _, skip := range rp.Shim.Skipped {
		if strings.Contains(skip.ItemPath, "FindAll") {
			foundSkipGeneric = true
		}
	}
	if !foundSkipGeneric {
		t.Errorf("expected SkipGeneric for Repository.FindAll, skipped: %v", rp.Shim.Skipped)
	}

	// 9. Assert async method is present (FetchAsync).
	var hasAsync bool
	for _, m := range rp.Shim.Methods {
		if m.IsAsync {
			hasAsync = true
		}
	}
	if !hasAsync {
		t.Error("expected at least one async method in shim")
	}

	// 10. Assert Mochi files are populated.
	if rp.Mochi.ExternMochi == "" {
		t.Error("expected non-empty ExternMochi")
	}
	if rp.Mochi.AliasMochi == "" {
		t.Error("expected non-empty AliasMochi")
	}

	// 11. Assert extern mochi contains extern type declarations.
	if !strings.Contains(rp.Mochi.ExternMochi, "extern") {
		t.Errorf("ExternMochi should contain 'extern', got: %s", rp.Mochi.ExternMochi)
	}

	// 12. Assert bridge Go file is non-empty.
	if rp.Bridge.GoFile == "" {
		t.Error("expected non-empty Bridge.GoFile")
	}

	// 13. Assert bridge Go file contains init().
	if !strings.Contains(rp.Bridge.GoFile, "func init()") {
		t.Errorf("Bridge.GoFile should contain init(), got: %s", rp.Bridge.GoFile)
	}

	// 14. Assert runtime config JSON is non-empty.
	if rp.Bridge.RuntimeConfigJSON == "" {
		t.Error("expected non-empty RuntimeConfigJSON")
	}

	// 15. Assert runtime config contains runtimeOptions.
	if !strings.Contains(rp.Bridge.RuntimeConfigJSON, "runtimeOptions") {
		t.Errorf("RuntimeConfigJSON should contain runtimeOptions, got: %s", rp.Bridge.RuntimeConfigJSON)
	}

	// 16. Assert shim dir is set.
	if rp.ShimDir == "" {
		t.Error("expected non-empty ShimDir")
	}
	if !strings.HasPrefix(rp.ShimDir, "dotnet_shim/") {
		t.Errorf("ShimDir should start with dotnet_shim/, got: %s", rp.ShimDir)
	}

	// 17. Assert target framework is set in shim.
	if rp.Shim.TargetFramework != "net8.0" {
		t.Errorf("expected net8.0, got %s", rp.Shim.TargetFramework)
	}

	// 18. Materialise workspace.
	slnPath, err := p.MaterialiseWorkspace(result)
	if err != nil {
		t.Fatalf("MaterialiseWorkspace: %v", err)
	}

	// 19. Assert .sln file exists.
	if _, err := os.Stat(slnPath); err != nil {
		t.Errorf("sln not created at %s: %v", slnPath, err)
	}

	// 20. Locate shim directory.
	shimDir := filepath.Join(dir, "dotnet_workspace", rp.ShimDir)

	// 21. Assert Bridge.cs exists.
	bridgePath := filepath.Join(shimDir, "Bridge.cs")
	if _, err := os.Stat(bridgePath); err != nil {
		t.Errorf("Bridge.cs not found at %s: %v", bridgePath, err)
	}

	// 22. Assert Bridge.cs contains the package reference.
	bridgeCS, err := os.ReadFile(bridgePath)
	if err != nil {
		t.Fatalf("read Bridge.cs: %v", err)
	}
	if !strings.Contains(string(bridgeCS), "Bridge") {
		t.Errorf("Bridge.cs missing expected content, got: %s", string(bridgeCS)[:200])
	}

	// 23. Assert .csproj exists.
	assemblyName := "MochiShim.E2ETestPackage"
	csprojPath := filepath.Join(shimDir, assemblyName+".csproj")
	if _, err := os.Stat(csprojPath); err != nil {
		t.Errorf("csproj not found at %s: %v", csprojPath, err)
	}

	// 24. Assert SKIPPED.txt exists.
	skippedPath := filepath.Join(shimDir, "SKIPPED.txt")
	if _, err := os.Stat(skippedPath); err != nil {
		t.Errorf("SKIPPED.txt not found at %s: %v", skippedPath, err)
	}

	// 25. Assert SKIPPED.txt contains SkipGeneric.
	skippedContent, err := os.ReadFile(skippedPath)
	if err != nil {
		t.Fatalf("read SKIPPED.txt: %v", err)
	}
	if !strings.Contains(string(skippedContent), "SkipGeneric") {
		t.Errorf("SKIPPED.txt should contain SkipGeneric, got: %s", string(skippedContent))
	}

	// 26. Assert .csproj contains the target framework.
	csprojContent, err := os.ReadFile(csprojPath)
	if err != nil {
		t.Fatalf("read csproj: %v", err)
	}
	if !strings.Contains(string(csprojContent), "net8.0") {
		t.Errorf("csproj should contain net8.0, got: %s", string(csprojContent))
	}

	// 27. Assert Bridge.cs contains [UnmanagedCallersOnly].
	if !strings.Contains(string(bridgeCS), "UnmanagedCallersOnly") {
		t.Error("Bridge.cs should contain UnmanagedCallersOnly")
	}

	// 28. Assert the bridge Go file contains loadShim.
	if !strings.Contains(rp.Bridge.GoFile, "loadShim") {
		t.Errorf("Bridge.GoFile should contain loadShim, got: %s", rp.Bridge.GoFile)
	}

	// 29. Assert the runtime config JSON contains net8.0.
	if !strings.Contains(rp.Bridge.RuntimeConfigJSON, "net8.0") {
		t.Errorf("RuntimeConfigJSON should contain net8.0, got: %s", rp.Bridge.RuntimeConfigJSON)
	}

	// 30. Assert mochi extern file contains the package name.
	if !strings.Contains(rp.Mochi.ExternMochi, pkgID) {
		// The header contains the package ID.
		t.Errorf("ExternMochi should reference package %s, got: %s", pkgID, rp.Mochi.ExternMochi)
	}
}

// TestPipeline_EndToEnd_multiplePackages exercises the pipeline with multiple
// packages to verify each produces distinct workspace output.
func TestPipeline_EndToEnd_multiplePackages(t *testing.T) {
	dir := t.TempDir()
	d := build.NewDriver(dir)
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}

	p := &build.Pipeline{
		Driver:   d,
		Provider: endToEndProvider(),
	}

	refs := []build.ImportRef{
		{ID: "Pkg.Alpha", Version: "1.0.0"},
		{ID: "Pkg.Beta", Version: "2.0.0"},
	}

	result, err := p.Resolve(refs)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}

	if len(result.Resolved) != 2 {
		t.Fatalf("expected 2 resolved packages, got %d", len(result.Resolved))
	}

	slnPath, err := p.MaterialiseWorkspace(result)
	if err != nil {
		t.Fatalf("MaterialiseWorkspace: %v", err)
	}

	// Assert .sln was created.
	if _, err := os.Stat(slnPath); err != nil {
		t.Errorf("sln not created: %v", err)
	}

	// Assert each shim dir exists.
	for _, rp := range result.Resolved {
		shimDir := filepath.Join(dir, "dotnet_workspace", rp.ShimDir)
		if _, err := os.Stat(shimDir); err != nil {
			t.Errorf("shim dir missing for %s: %v", rp.Ref.ID, err)
		}
	}
}
