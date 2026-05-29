package publish

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/package3/dotnet/shimgen"
)

// WorkspaceConfig describes the full workspace layout for all imported NuGet packages.
type WorkspaceConfig struct {
	// WorkDir is the root directory for the generated workspace.
	WorkDir string
	// TargetFramework is the TFM for all shim projects.
	TargetFramework string
	// Shims is the list of shim configurations.
	Shims []ShimConfig
}

// ShimConfig describes one shim project in the workspace.
type ShimConfig struct {
	// Package is the NuGet package id.
	Package string
	// PackageVersion is the resolved package version.
	PackageVersion string
	// ShimDir is the relative path from WorkDir (e.g. "dotnet_shim/NewtonsoftJson").
	ShimDir string
	// AssemblyName is the C# assembly name (e.g. "MochiShim.NewtonsoftJson").
	AssemblyName string
}

// EmitSolutionSln renders a .sln file referencing all shim projects.
// The output is a Visual Studio solution file with a project entry per shim.
func EmitSolutionSln(cfg WorkspaceConfig) string {
	var b strings.Builder
	b.WriteString("\n")
	b.WriteString("Microsoft Visual Studio Solution File, Format Version 12.00\n")
	b.WriteString("# Visual Studio Version 17\n")
	b.WriteString("VisualStudioVersion = 17.0.31903.59\n")
	b.WriteString("MinimumVisualStudioVersion = 10.0.40219.1\n")
	// Stable GUID for C# project type.
	const csprojTypeGUID = "{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"
	for _, sc := range cfg.Shims {
		projPath := filepath.Join(sc.ShimDir, sc.AssemblyName+".csproj")
		// Use the assembly name as the project GUID salt (for determinism).
		projGUID := deterministicGUID(sc.AssemblyName)
		fmt.Fprintf(&b, "Project(%q) = %q, %q, %q\n",
			csprojTypeGUID, sc.AssemblyName, filepath.ToSlash(projPath), projGUID)
		b.WriteString("EndProject\n")
	}
	b.WriteString("Global\n")
	b.WriteString("\tGlobalSection(SolutionConfigurationPlatforms) = preSolution\n")
	b.WriteString("\t\tRelease|Any CPU = Release|Any CPU\n")
	b.WriteString("\tEndGlobalSection\n")
	b.WriteString("EndGlobal\n")
	return b.String()
}

// deterministicGUID returns a deterministic GUID-shaped string for the given name.
// It is not a real UUID; it is used to produce stable .sln files.
func deterministicGUID(name string) string {
	// Simple hash-based approach: pad/truncate name to fill a GUID pattern.
	hash := uint64(14695981039346656037)
	for _, c := range name {
		hash ^= uint64(c)
		hash *= 1099511628211
	}
	return fmt.Sprintf("{%08X-%04X-%04X-%04X-%012X}",
		uint32(hash),
		uint16(hash>>32),
		uint16(hash>>16)&0x0FFF|0x4000,
		uint16(hash>>48)&0x3FFF|0x8000,
		hash&0xFFFFFFFFFFFF)
}

// MaterialiseWorkspace writes all workspace files (shim .csprojs, Bridge.cs files,
// .runtimeconfig.json, .sln) for the given shims and pipeline result.
// Returns the path to the .sln file.
func MaterialiseWorkspace(cfg WorkspaceConfig, shims []*shimgen.Shim) (string, error) {
	if cfg.WorkDir == "" {
		return "", fmt.Errorf("publish: WorkDir must be set")
	}
	if err := os.MkdirAll(cfg.WorkDir, 0o755); err != nil {
		return "", fmt.Errorf("publish: mkdir %s: %w", cfg.WorkDir, err)
	}

	for i, sc := range cfg.Shims {
		shimDir := filepath.Join(cfg.WorkDir, filepath.FromSlash(sc.ShimDir))
		if err := os.MkdirAll(shimDir, 0o755); err != nil {
			return "", fmt.Errorf("publish: mkdir %s: %w", shimDir, err)
		}

		// Write Bridge.cs.
		var s *shimgen.Shim
		if i < len(shims) {
			s = shims[i]
		}
		if s == nil {
			s = &shimgen.Shim{
				Package:         sc.Package,
				PackageVersion:  sc.PackageVersion,
				TargetFramework: cfg.TargetFramework,
			}
		}
		bridgeCS := shimgen.EmitBridgeCS(s)
		bridgePath := filepath.Join(shimDir, "Bridge.cs")
		if err := os.WriteFile(bridgePath, []byte(bridgeCS), 0o644); err != nil {
			return "", fmt.Errorf("publish: write Bridge.cs: %w", err)
		}

		// Write .csproj.
		csproj := shimgen.EmitCsproj(s)
		csprojPath := filepath.Join(shimDir, sc.AssemblyName+".csproj")
		if err := os.WriteFile(csprojPath, []byte(csproj), 0o644); err != nil {
			return "", fmt.Errorf("publish: write csproj: %w", err)
		}

		// Write SKIPPED.txt.
		skipped := shimgen.EmitSkippedTXT(s)
		skippedPath := filepath.Join(shimDir, "SKIPPED.txt")
		if err := os.WriteFile(skippedPath, []byte(skipped), 0o644); err != nil {
			return "", fmt.Errorf("publish: write SKIPPED.txt: %w", err)
		}
	}

	// Write .sln.
	sln := EmitSolutionSln(cfg)
	slnName := "MochiShims.sln"
	slnPath := filepath.Join(cfg.WorkDir, slnName)
	if err := os.WriteFile(slnPath, []byte(sln), 0o644); err != nil {
		return "", fmt.Errorf("publish: write sln: %w", err)
	}
	return slnPath, nil
}
