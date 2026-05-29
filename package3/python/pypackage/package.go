package pypackage

import (
	"fmt"
	"strings"

	"mochi/package3/python/typemap"
)

// ExportKind classifies an entry in the public surface of the package.
type ExportKind int

const (
	// ExportFunc is a top-level callable surfaced to Python.
	ExportFunc ExportKind = iota
	// ExportRecord is a Mochi record exported as a Python dataclass.
	ExportRecord
	// ExportInterface is a Mochi interface exported as a Python Protocol.
	ExportInterface
	// ExportConstant is a module-level value with a fixed type.
	ExportConstant
)

func (k ExportKind) String() string {
	switch k {
	case ExportFunc:
		return "fun"
	case ExportRecord:
		return "record"
	case ExportInterface:
		return "interface"
	case ExportConstant:
		return "const"
	}
	return "unknown"
}

// Export is one entry in the package's public surface.
type Export struct {
	Name string
	Kind ExportKind
	Type typemap.MochiType
}

// Package is the in-memory shape of a Mochi-side Python distribution. Every
// field is required unless documented otherwise.
type Package struct {
	// Distribution is the PEP 503-normalised distribution name.
	Distribution string
	// Version is the PEP 440 release identifier (e.g. "0.6.0").
	Version string
	// Summary is the single-line description that lands in METADATA's
	// Summary header.
	Summary string
	// License is an SPDX expression (e.g. "Apache-2.0").
	License string
	// Author is optional and surfaces in the METADATA Author field.
	Author string
	// HomePage is optional and surfaces in METADATA's Home-page field.
	HomePage string
	// Module is the top-level Python module name. Defaults to
	// Distribution with `-` -> `_` when empty.
	Module string
	// RequiresPython is the PEP 440 python version specifier
	// (e.g. ">=3.12,<3.15"). Optional; omitted when empty.
	RequiresPython string
	// Dependencies are PEP 508 requirement strings (one entry per line in
	// METADATA's `Requires-Dist`).
	Dependencies []string
	// Exports is the public surface. Order is preserved in the .pyi.
	Exports []Export
}

// Validate ensures the package is well-formed enough to render.
func (p Package) Validate() error {
	if p.Distribution == "" {
		return fmt.Errorf("pypackage: empty Distribution")
	}
	if !isPEP503Name(p.Distribution) {
		return fmt.Errorf("pypackage: invalid Distribution %q", p.Distribution)
	}
	if p.Version == "" {
		return fmt.Errorf("pypackage: empty Version")
	}
	if p.Summary == "" {
		return fmt.Errorf("pypackage: empty Summary")
	}
	if p.License == "" {
		return fmt.Errorf("pypackage: empty License")
	}
	for i, e := range p.Exports {
		if e.Name == "" {
			return fmt.Errorf("pypackage: export %d has empty Name", i)
		}
		if !isPyIdent(e.Name) {
			return fmt.Errorf("pypackage: export %q is not a Python identifier", e.Name)
		}
	}
	return nil
}

// ModuleName returns the Python module name, defaulting to Distribution with
// `-` mapped to `_` so the wheel + sdist file layout names a valid Python
// import.
func (p Package) ModuleName() string {
	if p.Module != "" {
		return p.Module
	}
	return strings.ReplaceAll(p.Distribution, "-", "_")
}

func isPEP503Name(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		switch {
		case r >= 'a' && r <= 'z':
		case r >= '0' && r <= '9':
		case r == '-' && i != 0 && i != len(s)-1:
		default:
			return false
		}
	}
	return true
}

func isPyIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		switch {
		case r >= 'a' && r <= 'z':
		case r >= 'A' && r <= 'Z':
		case r == '_':
		case r >= '0' && r <= '9' && i > 0:
		default:
			return false
		}
	}
	return true
}
