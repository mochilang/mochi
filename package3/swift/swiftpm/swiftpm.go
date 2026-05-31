// Package swiftpm parses Swift Package Manager manifests. It invokes
// `swift package dump-package` to obtain a JSON description of the
// package and decodes it into a structured PackageManifest.
package swiftpm

import (
	"encoding/json"
	"fmt"
	"os/exec"
)

// Platform describes a supported platform and its minimum version.
type Platform struct {
	Name    string `json:"platformName"`
	Version string `json:"version"`
}

// Product describes a library or executable product a package vends.
type Product struct {
	Name    string   `json:"name"`
	Type    string   `json:"type"`
	Targets []string `json:"targets"`
}

// Target is one compilation unit in the package.
type Target struct {
	Name         string   `json:"name"`
	Type         string   `json:"type"`
	Path         string   `json:"path"`
	Dependencies []string `json:"dependencies"`
}

// Dependency is an external package dependency.
type Dependency struct {
	URL         string `json:"url"`
	Requirement string `json:"requirement"`
}

// PackageManifest is the decoded output of `swift package dump-package`.
type PackageManifest struct {
	Name         string       `json:"name"`
	Platforms    []Platform   `json:"platforms"`
	Products     []Product    `json:"products"`
	Targets      []Target     `json:"targets"`
	Dependencies []Dependency `json:"dependencies"`
}

// dumpJSON is the raw shape emitted by swift package dump-package.
// We do a two-pass decode: first into this shape, then normalise.
type dumpJSON struct {
	Name      string `json:"name"`
	Platforms []struct {
		PlatformName string `json:"platformName"`
		Version      string `json:"version"`
	} `json:"platforms"`
	Products []struct {
		Name    string   `json:"name"`
		Targets []string `json:"targets"`
		Type    struct {
			Library    *[]string `json:"library"`
			Executable *struct{} `json:"executable"`
		} `json:"type"`
	} `json:"products"`
	Targets []struct {
		Name         string `json:"name"`
		Type         string `json:"type"`
		Path         string `json:"path"`
		Dependencies []struct {
			Product *struct {
				Name string `json:"name"`
			} `json:"product"`
			Target *struct {
				Name string `json:"name"`
			} `json:"target"`
			ByName *struct {
				Name string `json:"name"`
			} `json:"byName"`
		} `json:"dependencies"`
	} `json:"targets"`
	Dependencies []struct {
		SourceControl []struct {
			Location struct {
				Remote []struct {
					URLString string `json:"urlString"`
				} `json:"remote"`
			} `json:"location"`
			Requirement struct {
				Range []struct {
					LowerBound string `json:"lowerBound"`
					UpperBound string `json:"upperBound"`
				} `json:"range"`
				Exact  []string `json:"exact"`
				Branch []string `json:"branch"`
			} `json:"requirement"`
		} `json:"sourceControl"`
	} `json:"dependencies"`
}

// DumpPackage runs `swift package dump-package` in dir and returns the parsed
// manifest. Returns an error if swift is not on PATH or the command fails.
func DumpPackage(dir string) (*PackageManifest, error) {
	swiftBin, err := exec.LookPath("swift")
	if err != nil {
		return nil, fmt.Errorf("swiftpm: swift not found on PATH: %w", err)
	}
	cmd := exec.Command(swiftBin, "package", "dump-package")
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("swiftpm: swift package dump-package failed: %w", err)
	}
	return ParseDumpJSON(out)
}

// ParseDumpJSON parses the JSON output of `swift package dump-package`.
func ParseDumpJSON(data []byte) (*PackageManifest, error) {
	var raw dumpJSON
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, fmt.Errorf("swiftpm: json decode: %w", err)
	}

	m := &PackageManifest{Name: raw.Name}

	for _, p := range raw.Platforms {
		m.Platforms = append(m.Platforms, Platform{
			Name:    p.PlatformName,
			Version: p.Version,
		})
	}

	for _, p := range raw.Products {
		typ := "library"
		if p.Type.Executable != nil {
			typ = "executable"
		}
		m.Products = append(m.Products, Product{
			Name:    p.Name,
			Type:    typ,
			Targets: p.Targets,
		})
	}

	for _, t := range raw.Targets {
		var deps []string
		for _, d := range t.Dependencies {
			switch {
			case d.Product != nil:
				deps = append(deps, d.Product.Name)
			case d.Target != nil:
				deps = append(deps, d.Target.Name)
			case d.ByName != nil:
				deps = append(deps, d.ByName.Name)
			}
		}
		m.Targets = append(m.Targets, Target{
			Name:         t.Name,
			Type:         t.Type,
			Path:         t.Path,
			Dependencies: deps,
		})
	}

	for _, d := range raw.Dependencies {
		for _, sc := range d.SourceControl {
			url := ""
			if len(sc.Location.Remote) > 0 {
				url = sc.Location.Remote[0].URLString
			}
			req := ""
			if len(sc.Requirement.Range) > 0 {
				r := sc.Requirement.Range[0]
				req = fmt.Sprintf(`"%s"..<"%s"`, r.LowerBound, r.UpperBound)
			} else if len(sc.Requirement.Exact) > 0 {
				req = fmt.Sprintf(`exact: "%s"`, sc.Requirement.Exact[0])
			} else if len(sc.Requirement.Branch) > 0 {
				req = fmt.Sprintf(`branch: "%s"`, sc.Requirement.Branch[0])
			}
			m.Dependencies = append(m.Dependencies, Dependency{URL: url, Requirement: req})
		}
	}

	return m, nil
}
