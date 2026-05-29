package uv

import (
	"fmt"
	"sort"
	"strings"

	"mochi/package3/python/toml"
)

// Lockfile is the parsed contents of a uv.lock file. It is the
// canonical input to phase 8 (build orchestration) and phase 9
// (mochi.lock integration).
type Lockfile struct {
	// Version is the uv.lock schema version. uv 0.4 emits version 1.
	Version int64
	// RequiresPython is the project-level PEP 440 specifier.
	RequiresPython string
	// Packages is every package the resolver included, in the order the
	// lockfile lists them.
	Packages []LockedPackage
}

// LockedPackage is one resolved entry in uv.lock.
type LockedPackage struct {
	// Name is the PEP 503 normalised project name as written by uv.
	Name string
	// Version is the resolved PEP 440 version string.
	Version string
	// Source describes where the package came from.
	Source LockedSource
	// Dependencies are the names of other packages this package depends on
	// at the resolved version. uv writes these as inline tables with a
	// `name` key (and optional extras / markers we expose verbatim).
	Dependencies []LockedDep
	// Wheels are every wheel file the resolver chose to record for this
	// package across the target platforms.
	Wheels []LockedFile
	// Sdist is the optional source distribution.
	Sdist *LockedFile
}

// LockedSource is the `source = { ... }` inline table from uv.lock. uv writes
// one of `registry = "<url>"`, `git = "<url>"`, `path = "<path>"`, or
// `editable = "<path>"`.
type LockedSource struct {
	// Kind is one of "registry", "git", "path", "editable", or "" when uv
	// did not record a source (e.g., workspace virtual root).
	Kind string
	// URL is the registry URL or git URL.
	URL string
	// Path is the filesystem path for path / editable sources.
	Path string
	// Reference is the git commit / tag for git sources.
	Reference string
}

// LockedDep is one entry from a `dependencies = [{...}, ...]` array.
type LockedDep struct {
	// Name is the dependency's project name.
	Name string
	// Extras is the list of extras requested for this dependency.
	Extras []string
	// Marker is the PEP 508 environment marker, if uv recorded one.
	Marker string
}

// LockedFile is one wheel or sdist entry, e.g.
// `{url = "...", hash = "sha256:..."}`.
type LockedFile struct {
	URL      string
	Hash     string // "<algo>:<hex>"
	Filename string // optional; uv sometimes omits and the bridge derives it
	Size     int64  // optional
}

// ParseLockfile decodes the uv.lock TOML body.
func ParseLockfile(src []byte) (*Lockfile, error) {
	tree, err := toml.Parse(string(src))
	if err != nil {
		return nil, fmt.Errorf("uv: parse uv.lock: %w", err)
	}
	d := toml.NewDecoder(tree)
	lf := &Lockfile{}
	if v, present, err := d.Int("version"); err != nil {
		return nil, err
	} else if present {
		lf.Version = v
	}
	if s, present, err := d.String("requires-python"); err != nil {
		return nil, err
	} else if present {
		lf.RequiresPython = s
	}
	pkgs, present, err := d.TableArray("package")
	if err != nil {
		return nil, err
	}
	if !present {
		return lf, nil
	}
	for i, p := range pkgs {
		lp, err := parseLockedPackage(p)
		if err != nil {
			return nil, fmt.Errorf("uv: package %d: %w", i, err)
		}
		lf.Packages = append(lf.Packages, lp)
	}
	return lf, nil
}

func parseLockedPackage(d *toml.Decoder) (LockedPackage, error) {
	var lp LockedPackage
	name, err := d.StringRequired("name")
	if err != nil {
		return lp, err
	}
	lp.Name = name
	version, err := d.StringRequired("version")
	if err != nil {
		return lp, err
	}
	lp.Version = version

	srcTbl, srcPresent, err := d.Table("source")
	if err != nil {
		return lp, err
	}
	if srcPresent {
		lp.Source = parseLockedSource(srcTbl)
	}

	deps, depsPresent, err := d.TableArray("dependencies")
	if err != nil {
		return lp, err
	}
	if depsPresent {
		for j, depD := range deps {
			ld, err := parseLockedDep(depD)
			if err != nil {
				return lp, fmt.Errorf("dependency %d: %w", j, err)
			}
			lp.Dependencies = append(lp.Dependencies, ld)
		}
	}

	wheels, wheelsPresent, err := d.TableArray("wheels")
	if err != nil {
		return lp, err
	}
	if wheelsPresent {
		for j, w := range wheels {
			lf, err := parseLockedFile(w)
			if err != nil {
				return lp, fmt.Errorf("wheel %d: %w", j, err)
			}
			lp.Wheels = append(lp.Wheels, lf)
		}
	}

	if sdistTbl, present, err := d.Table("sdist"); err != nil {
		return lp, err
	} else if present {
		lf, err := parseLockedFile(sdistTbl)
		if err != nil {
			return lp, fmt.Errorf("sdist: %w", err)
		}
		lp.Sdist = &lf
	}
	return lp, nil
}

func parseLockedSource(d *toml.Decoder) LockedSource {
	var s LockedSource
	if u, present, _ := d.String("registry"); present {
		s.Kind = "registry"
		s.URL = u
		return s
	}
	if u, present, _ := d.String("git"); present {
		s.Kind = "git"
		s.URL = u
		if ref, present, _ := d.String("rev"); present {
			s.Reference = ref
		} else if ref, present, _ := d.String("tag"); present {
			s.Reference = ref
		} else if ref, present, _ := d.String("branch"); present {
			s.Reference = ref
		}
		return s
	}
	if p, present, _ := d.String("path"); present {
		s.Kind = "path"
		s.Path = p
		return s
	}
	if p, present, _ := d.String("editable"); present {
		s.Kind = "editable"
		s.Path = p
		return s
	}
	return s
}

func parseLockedDep(d *toml.Decoder) (LockedDep, error) {
	var ld LockedDep
	name, err := d.StringRequired("name")
	if err != nil {
		return ld, err
	}
	ld.Name = name
	if xs, present, err := d.StringArray("extras"); err != nil {
		return ld, err
	} else if present {
		ld.Extras = xs
	}
	if m, present, err := d.String("marker"); err != nil {
		return ld, err
	} else if present {
		ld.Marker = m
	}
	return ld, nil
}

func parseLockedFile(d *toml.Decoder) (LockedFile, error) {
	var f LockedFile
	url, err := d.StringRequired("url")
	if err != nil {
		return f, err
	}
	f.URL = url
	if h, present, err := d.String("hash"); err != nil {
		return f, err
	} else if present {
		f.Hash = h
	}
	if fn, present, err := d.String("filename"); err != nil {
		return f, err
	} else if present {
		f.Filename = fn
	}
	if sz, present, err := d.Int("size"); err != nil {
		return f, err
	} else if present {
		f.Size = sz
	}
	return f, nil
}

// PackagesByName returns a map keyed by Name for fast lookup.
func (l *Lockfile) PackagesByName() map[string]LockedPackage {
	out := make(map[string]LockedPackage, len(l.Packages))
	for _, p := range l.Packages {
		out[p.Name] = p
	}
	return out
}

// SortedPackageNames returns every package name sorted alphabetically.
func (l *Lockfile) SortedPackageNames() []string {
	names := make([]string, len(l.Packages))
	for i, p := range l.Packages {
		names[i] = p.Name
	}
	sort.Strings(names)
	return names
}

// SplitHash returns (algo, hex, ok) from a "<algo>:<hex>" string.
func SplitHash(h string) (string, string, bool) {
	i := strings.IndexByte(h, ':')
	if i <= 0 || i == len(h)-1 {
		return "", "", false
	}
	return strings.ToLower(h[:i]), strings.ToLower(h[i+1:]), true
}
