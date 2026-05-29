package build_test

import (
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/dotnet/build"
	"mochi/package3/dotnet/metacli"
)

// mockSurface returns a minimal ApiSurface for testing.
func mockSurface(id, version string) *metacli.ApiSurface {
	return &metacli.ApiSurface{
		Assembly:        id,
		Version:         version,
		TargetFramework: "net8.0",
		Types: []metacli.SurfaceType{
			{
				FullName:  id + ".FooClass",
				ShortName: "FooClass",
				Namespace: id,
				Kind:      metacli.KindClass,
				IsStatic:  true,
				Methods: []metacli.SurfaceMethod{
					{
						Name:     "DoWork",
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
				},
			},
		},
	}
}

func mockProvider() build.SurfaceProviderFunc {
	return func(id, version string) (*metacli.ApiSurface, error) {
		return mockSurface(id, version), nil
	}
}

func TestResolve_singlePackage(t *testing.T) {
	p := &build.Pipeline{
		Provider: mockProvider(),
	}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "Newtonsoft.Json", Version: "13.0.3", Alias: "json"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(result.Resolved) != 1 {
		t.Fatalf("expected 1 resolved, got %d", len(result.Resolved))
	}
}

func TestResolve_multiplePackages(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "Pkg.A", Version: "1.0.0", Alias: "a"},
		{ID: "Pkg.B", Version: "2.0.0", Alias: "b"},
		{ID: "Pkg.C", Version: "3.0.0", Alias: "c"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(result.Resolved) != 3 {
		t.Fatalf("expected 3 resolved, got %d", len(result.Resolved))
	}
}

func TestResolve_duplicateDeduplication(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "Pkg.A", Version: "1.0.0", Alias: "a1"},
		{ID: "Pkg.A", Version: "1.0.0", Alias: "a2"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(result.Resolved) != 1 {
		t.Errorf("expected 1 after dedup, got %d", len(result.Resolved))
	}
}

func TestResolve_nilProvider(t *testing.T) {
	p := &build.Pipeline{}
	_, err := p.Resolve(nil)
	if err == nil {
		t.Error("expected error for nil provider")
	}
}

func TestResolve_nilPipeline(t *testing.T) {
	var p *build.Pipeline
	_, err := p.Resolve(nil)
	if err == nil {
		t.Error("expected error for nil pipeline")
	}
}

func TestResolve_emptyIDError(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	_, err := p.Resolve([]build.ImportRef{{ID: "", Version: "1.0.0"}})
	if err == nil {
		t.Error("expected error for empty ID")
	}
}

func TestResolve_emptyVersionError(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	_, err := p.Resolve([]build.ImportRef{{ID: "Pkg", Version: ""}})
	if err == nil {
		t.Error("expected error for empty version")
	}
}

func TestResolve_shimsHaveRef(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "MyPkg", Version: "1.0.0", Alias: "pkg"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	rp := result.Resolved[0]
	if rp.Ref.ID != "MyPkg" {
		t.Errorf("expected ID MyPkg, got %s", rp.Ref.ID)
	}
	if rp.Shim == nil {
		t.Error("expected non-nil Shim")
	}
}

func TestResolve_bridgeFilesNonEmpty(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "MyPkg", Version: "1.0.0"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	rp := result.Resolved[0]
	if rp.Bridge.GoFile == "" {
		t.Error("expected non-empty GoFile")
	}
	if rp.Bridge.RuntimeConfigJSON == "" {
		t.Error("expected non-empty RuntimeConfigJSON")
	}
}

func TestResolve_mochiFilesNonEmpty(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "MyPkg", Version: "1.0.0"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	rp := result.Resolved[0]
	if rp.Mochi.ExternMochi == "" {
		t.Error("expected non-empty ExternMochi")
	}
}

func TestResolve_customTFM(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider(), TargetFramework: "net6.0"}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "MyPkg", Version: "1.0.0"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	rp := result.Resolved[0]
	if rp.Shim.TargetFramework != "net6.0" {
		t.Errorf("expected net6.0 TFM, got %s", rp.Shim.TargetFramework)
	}
}

func TestResolve_sortedByShimDir(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "Zzz.Pkg", Version: "1.0.0"},
		{ID: "Aaa.Pkg", Version: "1.0.0"},
	})
	if err != nil {
		t.Fatalf("Resolve error: %v", err)
	}
	if len(result.Resolved) == 2 {
		if result.Resolved[0].ShimDir > result.Resolved[1].ShimDir {
			t.Errorf("results not sorted by ShimDir: %s > %s",
				result.Resolved[0].ShimDir, result.Resolved[1].ShimDir)
		}
	}
}

func TestResolve_emptyRefs(t *testing.T) {
	p := &build.Pipeline{Provider: mockProvider()}
	result, err := p.Resolve(nil)
	if err != nil {
		t.Fatalf("unexpected error for empty refs: %v", err)
	}
	if len(result.Resolved) != 0 {
		t.Errorf("expected 0 resolved, got %d", len(result.Resolved))
	}
}

func TestMaterialiseWorkspace_createsFiles(t *testing.T) {
	dir := t.TempDir()
	d := build.NewDriver(dir)
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	p := &build.Pipeline{Driver: d, Provider: mockProvider()}
	result, err := p.Resolve([]build.ImportRef{
		{ID: "Newtonsoft.Json", Version: "13.0.3"},
	})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	slnPath, err := p.MaterialiseWorkspace(result)
	if err != nil {
		t.Fatalf("MaterialiseWorkspace: %v", err)
	}
	if !fileExistsTest(slnPath) {
		t.Errorf("sln not created: %s", slnPath)
	}
}

func TestMaterialiseWorkspace_nilPipeline(t *testing.T) {
	var p *build.Pipeline
	_, err := p.MaterialiseWorkspace(&build.PipelineResult{})
	if err == nil {
		t.Error("expected error for nil pipeline")
	}
}

func TestMaterialiseWorkspace_nilResult(t *testing.T) {
	p := &build.Pipeline{Driver: build.NewDriver(t.TempDir()), Provider: mockProvider()}
	_, err := p.MaterialiseWorkspace(nil)
	if err == nil {
		t.Error("expected error for nil result")
	}
}

func TestMaterialiseWorkspace_bridgeCsPresent(t *testing.T) {
	dir := t.TempDir()
	d := build.NewDriver(dir)
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	p := &build.Pipeline{Driver: d, Provider: mockProvider()}
	result, _ := p.Resolve([]build.ImportRef{{ID: "Newtonsoft.Json", Version: "13.0.3"}})
	_, err := p.MaterialiseWorkspace(result)
	if err != nil {
		t.Fatalf("MaterialiseWorkspace: %v", err)
	}
	// Check Bridge.cs under the workspace dir.
	shimDir := filepath.Join(dir, "dotnet_workspace", "dotnet_shim", "newtonsoft.json")
	bridgePath := filepath.Join(shimDir, "Bridge.cs")
	if !fileExistsTest(bridgePath) {
		// Try to list the directory to help debug.
		entries, _ := os.ReadDir(filepath.Join(dir, "dotnet_workspace"))
		t.Logf("workspace contents: %v", entries)
		t.Errorf("Bridge.cs not created at %s", bridgePath)
	}
}

func fileExistsTest(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}
