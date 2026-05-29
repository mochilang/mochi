package build

import (
	"errors"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"unicode"

	"mochi/package3/dotnet/clrhosting"
	"mochi/package3/dotnet/emit"
	"mochi/package3/dotnet/metacli"
	"mochi/package3/dotnet/publish"
	"mochi/package3/dotnet/shimgen"
)

// ImportRef is one resolved `import dotnet "<id>@<version>" as <alias>` statement.
type ImportRef struct {
	// ID is the NuGet package id.
	ID string
	// Version is the resolved version.
	Version string
	// Alias is the Mochi-side alias the user introduced.
	Alias string
}

// SurfaceProvider resolves an (id, version) pair to a metacli.ApiSurface.
type SurfaceProvider interface {
	Surface(id, version string) (*metacli.ApiSurface, error)
}

// SurfaceProviderFunc adapts a plain function to SurfaceProvider.
type SurfaceProviderFunc func(id, version string) (*metacli.ApiSurface, error)

// Surface dispatches to the underlying function.
func (f SurfaceProviderFunc) Surface(id, version string) (*metacli.ApiSurface, error) {
	return f(id, version)
}

// EmittedBridge is the bundle of generated files for one CLR hosting bridge.
type EmittedBridge = clrhosting.EmittedBridge

// ResolvedPackage is the result of running one ImportRef through the pipeline.
type ResolvedPackage struct {
	// Ref is the original import ref.
	Ref ImportRef
	// Shim is the synthesised C# shim.
	Shim *shimgen.Shim
	// Mochi holds the generated Mochi source files.
	Mochi emit.Files
	// Bridge holds the generated Go hosting bridge files.
	Bridge EmittedBridge
	// ShimDir is the relative path to the shim directory (e.g. "dotnet_shim/NewtonsoftJson").
	ShimDir string
}

// PipelineResult is the bundle returned by Pipeline.Resolve.
type PipelineResult struct {
	// Resolved contains one entry per resolved import ref.
	Resolved []ResolvedPackage
}

// Pipeline drives the end-to-end MEP-68 bridge synthesis.
type Pipeline struct {
	// Driver holds filesystem configuration.
	Driver *Driver
	// Provider resolves import refs to API surfaces.
	Provider SurfaceProvider
	// TargetFramework is the TFM for all generated projects (default "net8.0").
	TargetFramework string
}

// Resolve runs each ImportRef through the synthesis pipeline.
// Does NOT touch the filesystem.
func (p *Pipeline) Resolve(refs []ImportRef) (*PipelineResult, error) {
	if p == nil {
		return nil, errors.New("pipeline: nil receiver")
	}
	if p.Provider == nil {
		return nil, errors.New("pipeline: no SurfaceProvider configured")
	}
	tfm := p.TargetFramework
	if tfm == "" {
		tfm = "net8.0"
	}
	seen := map[string]struct{}{}
	out := make([]ResolvedPackage, 0, len(refs))
	for _, ref := range refs {
		if err := validateRef(ref); err != nil {
			return nil, err
		}
		key := ref.ID + "@" + ref.Version
		if _, dup := seen[key]; dup {
			continue
		}
		seen[key] = struct{}{}

		surface, err := p.Provider.Surface(ref.ID, ref.Version)
		if err != nil {
			return nil, fmt.Errorf("pipeline: surface for %s: %w", key, err)
		}
		if surface == nil {
			return nil, fmt.Errorf("pipeline: surface for %s: provider returned nil surface", key)
		}

		shim := shimgen.Synth(ref.ID, ref.Version, tfm, surface)
		files := emit.Emit(shim)

		shimDir := "dotnet_shim/" + safePathSegment(ref.ID)
		assemblyName := "MochiShim." + safeNamespace(ref.ID)

		cfg := clrhosting.Config{
			Package:          ref.ID,
			PackageVersion:   ref.Version,
			TargetFramework:  tfm,
			ShimAssemblyName: assemblyName,
			ShimDir:          shimDir,
			MarshalFreeEntry: "mochi_marshal_free",
		}
		methods := clrhosting.BindingsFromShim(shim)
		goFile := clrhosting.EmitGoFile(safePackageName(ref.ID), cfg, methods)
		runtimeCfg := clrhosting.RuntimeConfigJSON(cfg)

		out = append(out, ResolvedPackage{
			Ref:   ref,
			Shim:  shim,
			Mochi: files,
			Bridge: EmittedBridge{
				GoFile:            goFile,
				RuntimeConfigJSON: runtimeCfg,
			},
			ShimDir: shimDir,
		})
	}
	sort.Slice(out, func(i, j int) bool {
		return out[i].ShimDir < out[j].ShimDir
	})
	return &PipelineResult{Resolved: out}, nil
}

// MaterialiseWorkspace writes all workspace files and returns the workspace root path.
func (p *Pipeline) MaterialiseWorkspace(result *PipelineResult) (string, error) {
	if p == nil || p.Driver == nil {
		return "", errors.New("pipeline: nil pipeline or driver")
	}
	if result == nil {
		return "", errors.New("pipeline: nil PipelineResult")
	}
	tfm := p.TargetFramework
	if tfm == "" {
		tfm = "net8.0"
	}
	shimCfgs := make([]publish.ShimConfig, 0, len(result.Resolved))
	shims := make([]*shimgen.Shim, 0, len(result.Resolved))
	for _, rp := range result.Resolved {
		assemblyName := "MochiShim." + safeNamespace(rp.Ref.ID)
		shimCfgs = append(shimCfgs, publish.ShimConfig{
			Package:        rp.Ref.ID,
			PackageVersion: rp.Ref.Version,
			ShimDir:        rp.ShimDir,
			AssemblyName:   assemblyName,
		})
		shims = append(shims, rp.Shim)
	}
	cfg := publish.WorkspaceConfig{
		TargetFramework: tfm,
		Shims:           shimCfgs,
	}
	return p.Driver.WriteWorkspaceRoot(cfg, shims)
}

// validateRef checks for empty ID or version.
func validateRef(ref ImportRef) error {
	if ref.ID == "" {
		return fmt.Errorf("pipeline: import ref has empty ID")
	}
	if ref.Version == "" {
		return fmt.Errorf("pipeline: import ref %q has empty version", ref.ID)
	}
	return nil
}

// safePathSegment converts a NuGet package id to a safe filesystem path segment.
// "Newtonsoft.Json" -> "newtonsoft.json"
func safePathSegment(id string) string {
	return strings.ToLower(id)
}

// safeNamespace converts a NuGet package id to a C# namespace-safe string.
// "Newtonsoft.Json" -> "NewtonsoftJson"
func safeNamespace(id string) string {
	var b strings.Builder
	for _, r := range id {
		switch {
		case r == '.', r == '-':
			// drop separators
		case unicode.IsLetter(r) || unicode.IsDigit(r):
			b.WriteRune(r)
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}

// safePackageName converts a NuGet package id to a Go package name.
// "Newtonsoft.Json" -> "dotnet_bridge_newtonsoftjson"
func safePackageName(id string) string {
	var b strings.Builder
	b.WriteString("dotnet_bridge_")
	for _, r := range id {
		switch {
		case r == '.', r == '-':
			// drop separators
		case unicode.IsLetter(r) || unicode.IsDigit(r):
			b.WriteRune(unicode.ToLower(r))
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}

// shimDirPath builds the relative shim directory path.
func shimDirPath(id string) string {
	return filepath.Join("dotnet_shim", safePathSegment(id))
}
