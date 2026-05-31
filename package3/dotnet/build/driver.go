// Package build orchestrates the NativeAOT compilation and linking phase for
// MEP-68. It drives `dotnet publish /p:PublishAot=true /p:NativeLib=Static`
// for each imported NuGet package's wrapper project, manages the NuGet.Config
// that points dotnet restore at the content-addressed cache, and emits the
// linker flags that the MEP-53 build driver uses to link the resulting static
// archives into the final Mochi binary.
package build

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// WorkspaceConfig holds the paths and settings for the wrapper workspace.
type WorkspaceConfig struct {
	// WorkDir is the Mochi build working directory.
	WorkDir string
	// CacheDir is the content-addressed package cache (e.g. ~/.cache/mochi/dotnet-deps/).
	CacheDir string
	// RuntimeIdentifiers is the list of RIDs to compile for.
	RuntimeIdentifiers []string
	// Verbose enables verbose dotnet publish output.
	Verbose bool
}

// PackageLink holds the per-RID static library paths and linker flags
// produced by compiling a wrapper project.
type PackageLink struct {
	PackageID string
	// StaticLibs maps RID to the path of the compiled .a / .lib static archive.
	StaticLibs map[string]string
	// LinkerFlags are the flags to pass to the Rust/Mochi linker (via
	// cargo:rustc-link-lib and cargo:rustc-link-search build script directives).
	LinkerFlags []string
}

// PrepareWorkspace materialises the wrapper projects from the content-addressed
// cache into the working directory and writes a NuGet.Config so `dotnet restore`
// resolves packages from the local cache. It must be called before Build.
func PrepareWorkspace(cfg WorkspaceConfig, packages []PackageSpec) error {
	wrapDir := filepath.Join(cfg.WorkDir, "dotnet_wrap")
	depsDir := filepath.Join(cfg.WorkDir, "dotnet_deps")

	if err := os.MkdirAll(wrapDir, 0o755); err != nil {
		return fmt.Errorf("build: mkdir dotnet_wrap: %w", err)
	}
	if err := os.MkdirAll(depsDir, 0o755); err != nil {
		return fmt.Errorf("build: mkdir dotnet_deps: %w", err)
	}

	// Write NuGet.Config pointing to the local cache.
	nugetConfig := filepath.Join(cfg.WorkDir, "NuGet.Config")
	if err := writeNuGetConfig(nugetConfig, depsDir, cfg.CacheDir); err != nil {
		return fmt.Errorf("build: write NuGet.Config: %w", err)
	}

	// Materialise each package's wrapper project.
	for _, pkg := range packages {
		pkgWrapDir := filepath.Join(wrapDir, pkg.ID)
		if err := os.MkdirAll(filepath.Join(pkgWrapDir, "Src"), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(filepath.Join(pkgWrapDir, pkg.ID+".csproj"), []byte(pkg.CsprojBody), 0o644); err != nil {
			return fmt.Errorf("build: write %s.csproj: %w", pkg.ID, err)
		}
		if err := os.WriteFile(filepath.Join(pkgWrapDir, "Src", "Wrapper.cs"), []byte(pkg.WrapperBody), 0o644); err != nil {
			return fmt.Errorf("build: write Wrapper.cs for %s: %w", pkg.ID, err)
		}
	}
	return nil
}

// PackageSpec carries the generated wrapper files for one NuGet package.
type PackageSpec struct {
	ID          string
	Version     string
	CsprojBody  string
	WrapperBody string
}

// Build compiles the NativeAOT wrapper for a single package for all configured
// runtime identifiers. It returns a PackageLink with the paths to the compiled
// static archives.
func Build(ctx context.Context, cfg WorkspaceConfig, pkgID string) (*PackageLink, error) {
	wrapDir := filepath.Join(cfg.WorkDir, "dotnet_wrap", pkgID)
	link := &PackageLink{
		PackageID:  pkgID,
		StaticLibs: make(map[string]string),
	}

	for _, rid := range cfg.RuntimeIdentifiers {
		outDir := filepath.Join(cfg.WorkDir, "dotnet_native", rid, pkgID)
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			return nil, err
		}

		args := []string{
			"publish",
			filepath.Join(wrapDir, pkgID+".csproj"),
			"-r", rid,
			"-c", "Release",
			"--output", outDir,
			"/p:PublishAot=true",
			"/p:NativeLib=Static",
			"/p:StripSymbols=true",
			"/p:InvariantGlobalization=true",
			"--nologo",
		}
		if !cfg.Verbose {
			args = append(args, "-v", "quiet")
		}

		cmd := exec.CommandContext(ctx, "dotnet", args...)
		cmd.Dir = wrapDir
		if cfg.Verbose {
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
		}
		if out, err := cmd.CombinedOutput(); err != nil {
			return nil, fmt.Errorf("build: dotnet publish %s/%s: %w\n%s", pkgID, rid, err, out)
		}

		libName := staticLibName(pkgID, rid)
		libPath := filepath.Join(outDir, libName)
		if _, err := os.Stat(libPath); err != nil {
			return nil, fmt.Errorf("build: expected static lib not found: %s", libPath)
		}
		link.StaticLibs[rid] = libPath
	}

	// Emit cargo:rustc-link-lib and cargo:rustc-link-search directives.
	if len(cfg.RuntimeIdentifiers) > 0 {
		hostRID := cfg.RuntimeIdentifiers[0] // used for the host build
		hostOutDir := filepath.Join(cfg.WorkDir, "dotnet_native", hostRID, pkgID)
		link.LinkerFlags = []string{
			"cargo:rustc-link-search=native=" + hostOutDir,
			"cargo:rustc-link-lib=static=" + sanitizeLibName(pkgID) + "_dotnet_wrap",
		}
		// NativeAOT static libs require system libraries.
		link.LinkerFlags = append(link.LinkerFlags, nativeAOTSystemLibs(hostRID)...)
	}

	return link, nil
}

func staticLibName(pkgID, rid string) string {
	base := sanitizeLibName(pkgID) + "_dotnet_wrap"
	if strings.HasPrefix(rid, "win-") {
		return base + ".lib"
	}
	return "lib" + base + ".a"
}

func sanitizeLibName(pkgID string) string {
	var b strings.Builder
	for _, c := range pkgID {
		if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') {
			b.WriteRune(c)
		} else {
			b.WriteRune('_')
		}
	}
	return strings.ToLower(b.String())
}

func nativeAOTSystemLibs(rid string) []string {
	switch {
	case strings.HasPrefix(rid, "linux-"):
		return []string{
			"cargo:rustc-link-lib=stdc++",
			"cargo:rustc-link-lib=pthread",
			"cargo:rustc-link-lib=dl",
			"cargo:rustc-link-lib=m",
		}
	case strings.HasPrefix(rid, "osx-"):
		return []string{
			"cargo:rustc-link-lib=c++",
			"cargo:rustc-link-lib=framework=Foundation",
			"cargo:rustc-link-lib=framework=Security",
		}
	case strings.HasPrefix(rid, "win-"):
		return []string{
			"cargo:rustc-link-lib=bcrypt",
			"cargo:rustc-link-lib=kernel32",
			"cargo:rustc-link-lib=advapi32",
		}
	default:
		return nil
	}
}

func writeNuGetConfig(path, localFeed, globalCache string) error {
	body := fmt.Sprintf(`<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="mochi-local" value="%s" />
  </packageSources>
  <fallbackPackageFolders>
    <add key="mochi-cache" value="%s" />
  </fallbackPackageFolders>
</configuration>
`, localFeed, globalCache)
	return os.WriteFile(path, []byte(body), 0o644)
}
