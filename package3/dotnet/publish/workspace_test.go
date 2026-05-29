package publish_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/dotnet/publish"
	"mochi/package3/dotnet/shimgen"
)

func sampleWorkspaceConfig(workDir string) publish.WorkspaceConfig {
	return publish.WorkspaceConfig{
		WorkDir:         workDir,
		TargetFramework: "net8.0",
		Shims: []publish.ShimConfig{
			{
				Package:        "Newtonsoft.Json",
				PackageVersion: "13.0.3",
				ShimDir:        "dotnet_shim/NewtonsoftJson",
				AssemblyName:   "MochiShim.NewtonsoftJson",
			},
		},
	}
}

func TestEmitSolutionSln_header(t *testing.T) {
	cfg := sampleWorkspaceConfig("")
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "Microsoft Visual Studio Solution File") {
		t.Errorf("missing solution header: %s", got)
	}
}

func TestEmitSolutionSln_projectReference(t *testing.T) {
	cfg := sampleWorkspaceConfig("")
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "MochiShim.NewtonsoftJson") {
		t.Errorf("missing project reference: %s", got)
	}
}

func TestEmitSolutionSln_projectTypeGUID(t *testing.T) {
	cfg := sampleWorkspaceConfig("")
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "FAE04EC0") {
		t.Errorf("missing C# project type GUID: %s", got)
	}
}

func TestEmitSolutionSln_multipleProjects(t *testing.T) {
	cfg := publish.WorkspaceConfig{
		TargetFramework: "net8.0",
		Shims: []publish.ShimConfig{
			{Package: "PkgA", ShimDir: "dotnet_shim/PkgA", AssemblyName: "MochiShim.PkgA"},
			{Package: "PkgB", ShimDir: "dotnet_shim/PkgB", AssemblyName: "MochiShim.PkgB"},
		},
	}
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "MochiShim.PkgA") {
		t.Errorf("missing PkgA: %s", got)
	}
	if !strings.Contains(got, "MochiShim.PkgB") {
		t.Errorf("missing PkgB: %s", got)
	}
}

func TestEmitSolutionSln_globalSection(t *testing.T) {
	cfg := sampleWorkspaceConfig("")
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "Global") {
		t.Errorf("missing Global section: %s", got)
	}
}

func TestEmitSolutionSln_releaseConfig(t *testing.T) {
	cfg := sampleWorkspaceConfig("")
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "Release|Any CPU") {
		t.Errorf("missing Release|Any CPU: %s", got)
	}
}

func TestEmitSolutionSln_empty(t *testing.T) {
	cfg := publish.WorkspaceConfig{}
	got := publish.EmitSolutionSln(cfg)
	if !strings.Contains(got, "Microsoft Visual Studio Solution File") {
		t.Errorf("even empty config should produce solution header: %s", got)
	}
}

func TestMaterialiseWorkspace_createsSlnFile(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	slnPath, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !fileExists(slnPath) {
		t.Errorf("sln file not created at %s", slnPath)
	}
}

func TestMaterialiseWorkspace_createsBridgeCS(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	_, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	bridgePath := filepath.Join(dir, "dotnet_shim", "NewtonsoftJson", "Bridge.cs")
	if !fileExists(bridgePath) {
		t.Errorf("Bridge.cs not created at %s", bridgePath)
	}
}

func TestMaterialiseWorkspace_createsCsproj(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	_, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	csprojPath := filepath.Join(dir, "dotnet_shim", "NewtonsoftJson", "MochiShim.NewtonsoftJson.csproj")
	if !fileExists(csprojPath) {
		t.Errorf("csproj not created at %s", csprojPath)
	}
}

func TestMaterialiseWorkspace_createsSkippedTXT(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	_, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	skippedPath := filepath.Join(dir, "dotnet_shim", "NewtonsoftJson", "SKIPPED.txt")
	if !fileExists(skippedPath) {
		t.Errorf("SKIPPED.txt not created at %s", skippedPath)
	}
}

func TestMaterialiseWorkspace_withShim(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	shim := &shimgen.Shim{
		Package:         "Newtonsoft.Json",
		PackageVersion:  "13.0.3",
		TargetFramework: "net8.0",
	}
	slnPath, err := publish.MaterialiseWorkspace(cfg, []*shimgen.Shim{shim})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !fileExists(slnPath) {
		t.Errorf("sln not created: %s", slnPath)
	}
}

func TestMaterialiseWorkspace_idempotent(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	_, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("first call error: %v", err)
	}
	_, err = publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("second call error: %v", err)
	}
}

func TestMaterialiseWorkspace_slnAtWorkDir(t *testing.T) {
	dir := t.TempDir()
	cfg := sampleWorkspaceConfig(dir)
	slnPath, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// sln should be directly inside WorkDir.
	if filepath.Dir(slnPath) != dir {
		t.Errorf("sln not at workdir: %s (want parent %s)", slnPath, dir)
	}
}

func TestMaterialiseWorkspace_emptyWorkDirError(t *testing.T) {
	cfg := publish.WorkspaceConfig{}
	_, err := publish.MaterialiseWorkspace(cfg, nil)
	if err == nil {
		t.Error("expected error for empty WorkDir")
	}
}

func TestMaterialiseWorkspace_multipleShims(t *testing.T) {
	dir := t.TempDir()
	cfg := publish.WorkspaceConfig{
		WorkDir:         dir,
		TargetFramework: "net8.0",
		Shims: []publish.ShimConfig{
			{Package: "Pkg.A", PackageVersion: "1.0.0", ShimDir: "dotnet_shim/PkgA", AssemblyName: "MochiShim.PkgA"},
			{Package: "Pkg.B", PackageVersion: "2.0.0", ShimDir: "dotnet_shim/PkgB", AssemblyName: "MochiShim.PkgB"},
		},
	}
	_, err := publish.MaterialiseWorkspace(cfg, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !fileExists(filepath.Join(dir, "dotnet_shim", "PkgA", "Bridge.cs")) {
		t.Error("PkgA Bridge.cs not created")
	}
	if !fileExists(filepath.Join(dir, "dotnet_shim", "PkgB", "Bridge.cs")) {
		t.Error("PkgB Bridge.cs not created")
	}
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}
