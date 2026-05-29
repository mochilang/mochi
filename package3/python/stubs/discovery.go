package stubs

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Tier is the PEP 561 stub source tier.
type Tier int

const (
	TierUnknown Tier = iota
	// TierInline: package ships py.typed marker + bundled .pyi or annotated .py.
	TierInline
	// TierSiblingStubs: separate `<name>-stubs` distribution.
	TierSiblingStubs
	// TierTypeshed: centralised typeshed lookup.
	TierTypeshed
	// TierStubgen: fallback generated from .py source.
	TierStubgen
)

// String renders the tier token.
func (t Tier) String() string {
	switch t {
	case TierInline:
		return "inline"
	case TierSiblingStubs:
		return "sibling-stubs"
	case TierTypeshed:
		return "typeshed"
	case TierStubgen:
		return "stubgen"
	default:
		return "unknown"
	}
}

// StubSource is the discovered set of stub files for a package, plus the tier
// that produced them.
type StubSource struct {
	// Package is the PEP 503 normalised name.
	Package string
	// Tier indicates which PEP 561 source the stubs came from.
	Tier Tier
	// RootDir is the directory at which the package's stubs are rooted. For
	// inline stubs this is the package itself; for sibling stubs it is the
	// `<name>-stubs` directory; for typeshed it is the corresponding subtree
	// of the pinned typeshed checkout; for stubgen it is the cache directory
	// where stubgen wrote its output.
	RootDir string
	// Files is the relative paths of every .pyi (and .py with explicit
	// annotations under TierInline) under RootDir, sorted alphabetically.
	Files []string
	// Partial is true when the source carries no guarantee of completeness
	// (PEP 561 `py.typed` files contain the literal token "partial" or this
	// is a stubgen fallback). Phase 4 refuses to emit wrappers for partial
	// sources unless --allow-partial is set on the build.
	Partial bool
}

// Discovery resolves a package name into a StubSource by walking the four
// PEP 561 tiers in priority order.
type Discovery struct {
	// SitePackages is the venv `site-packages` directory containing the
	// installed package and any sibling stub distributions.
	SitePackages string
	// Typeshed is the optional pinned typeshed repository.
	Typeshed *Typeshed
	// Stubgen is the optional stubgen fallback runner.
	Stubgen Stubgen
	// AllowStubgen, when true, lets Resolve drop to the stubgen tier instead
	// of returning a not-found error.
	AllowStubgen bool
}

// Resolve runs the 4-tier lookup for `name` and returns the highest-tier hit.
// Returns (nil, err) when no source is available and stubgen is disabled.
// The returned StubSource's RootDir / Files are populated.
func (d *Discovery) Resolve(name string) (*StubSource, error) {
	if name == "" {
		return nil, fmt.Errorf("stubs: empty package name")
	}
	moduleName := strings.ReplaceAll(name, "-", "_")

	// Tier 1: inline (py.typed in the package).
	if d.SitePackages != "" {
		pkgDir := filepath.Join(d.SitePackages, moduleName)
		if info, err := os.Stat(filepath.Join(pkgDir, "py.typed")); err == nil && !info.IsDir() {
			files, _ := walkStubFiles(pkgDir)
			partial, _ := readPartialMarker(filepath.Join(pkgDir, "py.typed"))
			return &StubSource{
				Package: name,
				Tier:    TierInline,
				RootDir: pkgDir,
				Files:   files,
				Partial: partial,
			}, nil
		}
	}

	// Tier 2: sibling `<name>-stubs` distribution.
	if d.SitePackages != "" {
		stubDir := filepath.Join(d.SitePackages, moduleName+"-stubs")
		if info, err := os.Stat(stubDir); err == nil && info.IsDir() {
			files, _ := walkStubFiles(stubDir)
			return &StubSource{
				Package: name,
				Tier:    TierSiblingStubs,
				RootDir: stubDir,
				Files:   files,
				Partial: false,
			}, nil
		}
	}

	// Tier 3: typeshed.
	if d.Typeshed != nil {
		if src, ok := d.Typeshed.Lookup(name); ok {
			return src, nil
		}
	}

	// Tier 4: stubgen fallback.
	if d.AllowStubgen && d.Stubgen != nil {
		src, err := d.Stubgen.Generate(name, moduleName, d.SitePackages)
		if err != nil {
			return nil, fmt.Errorf("stubs: stubgen fallback for %q: %w", name, err)
		}
		return src, nil
	}

	return nil, fmt.Errorf("stubs: no PEP 561 stubs found for package %q (tiers: inline / sibling-stubs / typeshed / stubgen)", name)
}

// walkStubFiles returns the relative paths of every .pyi file under root,
// sorted alphabetically. .py files are included only when no .pyi shadows them.
func walkStubFiles(root string) ([]string, error) {
	var files []string
	seen := map[string]bool{}
	err := filepath.WalkDir(root, func(path string, _ os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		ext := filepath.Ext(path)
		if ext != ".pyi" && ext != ".py" {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		if ext == ".pyi" {
			files = append(files, rel)
			noExt := strings.TrimSuffix(rel, ".pyi")
			seen[noExt] = true
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	// Pass 2: include .py whose .pyi twin is absent.
	err = filepath.WalkDir(root, func(path string, _ os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if filepath.Ext(path) != ".py" {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		noExt := strings.TrimSuffix(rel, ".py")
		if seen[noExt] {
			return nil
		}
		files = append(files, rel)
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Strings(files)
	return files, nil
}

// readPartialMarker reads a PEP 561 `py.typed` file. PEP 561 §"Stub-Only
// Packages" allows the contents to be either empty (full type information) or
// the literal token "partial" (incomplete stubs). Any other content is
// tolerated but treated as full.
func readPartialMarker(path string) (bool, error) {
	body, err := os.ReadFile(path)
	if err != nil {
		return false, err
	}
	return strings.TrimSpace(string(body)) == "partial", nil
}
