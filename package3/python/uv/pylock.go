package uv

import (
	"fmt"
	"sort"
	"strings"

	"mochi/package3/python/toml"
)

// PyLock is the PEP 751 `pylock.toml` representation. PEP 751 is the cross-
// tool lockfile format championed by Brett Cannon and adopted by pip, uv,
// poetry, and pdm. The bridge round-trips this format so external tooling can
// hand a pylock.toml to Mochi and Mochi can hand one to external tooling.
//
// The PEP 751 envelope is:
//
//	lock-version = "1.0"
//	created-by = "<tool>"
//	requires-python = ">=3.10"
//
//	[[packages]]
//	name = "httpx"
//	version = "0.27.0"
//	marker = "..."
//	requires-python = ">=3.8"
//	dependencies = ["anyio", "certifi"]
//
//	[[packages.wheels]]
//	name = "httpx-0.27.0-py3-none-any.whl"
//	url = "https://files.pythonhosted.org/.../httpx-0.27.0-py3-none-any.whl"
//	hashes = {sha256 = "<hex>"}
//
//	[packages.sdist]
//	name = "httpx-0.27.0.tar.gz"
//	url = "..."
//	hashes = {sha256 = "<hex>"}
//
// The bridge exposes the same struct shape on read and write so phase 9 can
// take the parsed Lockfile (uv.lock) and re-emit it as a pylock.toml for
// downstream consumers without an intermediate intermediate representation.
type PyLock struct {
	LockVersion    string
	CreatedBy      string
	RequiresPython string
	Packages       []PyLockPackage
}

// PyLockPackage is one resolved package in the PEP 751 lockfile.
type PyLockPackage struct {
	Name           string
	Version        string
	Marker         string
	RequiresPython string
	Dependencies   []string
	Wheels         []PyLockFile
	Sdist          *PyLockFile
}

// PyLockFile is one wheel or sdist file in PEP 751.
type PyLockFile struct {
	// Name is the file basename, e.g. "httpx-0.27.0-py3-none-any.whl".
	Name string
	// URL is the absolute download URL.
	URL string
	// Hashes maps algorithm -> hex digest. PEP 751 requires at least one
	// hash and prefers sha256; pip-2026 will adopt blake3 alongside.
	Hashes map[string]string
}

// ParsePyLock decodes a pylock.toml document.
func ParsePyLock(src []byte) (*PyLock, error) {
	tree, err := toml.Parse(string(src))
	if err != nil {
		return nil, fmt.Errorf("pylock: parse: %w", err)
	}
	d := toml.NewDecoder(tree)
	lf := &PyLock{}
	if s, present, err := d.String("lock-version"); err != nil {
		return nil, err
	} else if present {
		lf.LockVersion = s
	}
	if s, present, err := d.String("created-by"); err != nil {
		return nil, err
	} else if present {
		lf.CreatedBy = s
	}
	if s, present, err := d.String("requires-python"); err != nil {
		return nil, err
	} else if present {
		lf.RequiresPython = s
	}
	pkgs, present, err := d.TableArray("packages")
	if err != nil {
		return nil, err
	}
	if !present {
		return lf, nil
	}
	for i, pd := range pkgs {
		pp, err := parsePyLockPackage(pd)
		if err != nil {
			return nil, fmt.Errorf("pylock: package %d: %w", i, err)
		}
		lf.Packages = append(lf.Packages, pp)
	}
	return lf, nil
}

func parsePyLockPackage(d *toml.Decoder) (PyLockPackage, error) {
	var pp PyLockPackage
	name, err := d.StringRequired("name")
	if err != nil {
		return pp, err
	}
	pp.Name = name
	version, err := d.StringRequired("version")
	if err != nil {
		return pp, err
	}
	pp.Version = version
	if s, present, err := d.String("marker"); err != nil {
		return pp, err
	} else if present {
		pp.Marker = s
	}
	if s, present, err := d.String("requires-python"); err != nil {
		return pp, err
	} else if present {
		pp.RequiresPython = s
	}
	if xs, present, err := d.StringArray("dependencies"); err != nil {
		return pp, err
	} else if present {
		pp.Dependencies = xs
	}
	wheels, wheelsPresent, err := d.TableArray("wheels")
	if err != nil {
		return pp, err
	}
	if wheelsPresent {
		for j, wd := range wheels {
			f, err := parsePyLockFile(wd)
			if err != nil {
				return pp, fmt.Errorf("wheel %d: %w", j, err)
			}
			pp.Wheels = append(pp.Wheels, f)
		}
	}
	if sdistD, present, err := d.Table("sdist"); err != nil {
		return pp, err
	} else if present {
		f, err := parsePyLockFile(sdistD)
		if err != nil {
			return pp, fmt.Errorf("sdist: %w", err)
		}
		pp.Sdist = &f
	}
	return pp, nil
}

func parsePyLockFile(d *toml.Decoder) (PyLockFile, error) {
	var f PyLockFile
	name, err := d.StringRequired("name")
	if err != nil {
		return f, err
	}
	f.Name = name
	url, err := d.StringRequired("url")
	if err != nil {
		return f, err
	}
	f.URL = url
	hashesTbl, present, err := d.Table("hashes")
	if err != nil {
		return f, err
	}
	if !present {
		return f, fmt.Errorf("file %q missing hashes", name)
	}
	f.Hashes = map[string]string{}
	for _, k := range hashesTbl.Keys() {
		v, _, err := hashesTbl.String(k)
		if err != nil {
			return f, err
		}
		f.Hashes[strings.ToLower(k)] = strings.ToLower(v)
	}
	if len(f.Hashes) == 0 {
		return f, fmt.Errorf("file %q hashes table empty", name)
	}
	return f, nil
}

// RenderPyLock emits a canonical PEP 751 pylock.toml. Output is deterministic:
// packages are sorted by name then version, wheels by name, hashes by algo.
func (l *PyLock) Render() string {
	var b strings.Builder
	b.WriteString("# Generated by mochi-python-bridge. PEP 751 lockfile.\n")
	if l.LockVersion != "" {
		fmt.Fprintf(&b, "lock-version = %q\n", l.LockVersion)
	} else {
		b.WriteString("lock-version = \"1.0\"\n")
	}
	if l.CreatedBy != "" {
		fmt.Fprintf(&b, "created-by = %q\n", l.CreatedBy)
	} else {
		b.WriteString("created-by = \"mochi\"\n")
	}
	if l.RequiresPython != "" {
		fmt.Fprintf(&b, "requires-python = %q\n", l.RequiresPython)
	}
	b.WriteString("\n")

	pkgs := append([]PyLockPackage(nil), l.Packages...)
	sort.SliceStable(pkgs, func(i, j int) bool {
		if pkgs[i].Name != pkgs[j].Name {
			return pkgs[i].Name < pkgs[j].Name
		}
		return pkgs[i].Version < pkgs[j].Version
	})
	for _, p := range pkgs {
		b.WriteString("[[packages]]\n")
		fmt.Fprintf(&b, "name = %q\n", p.Name)
		fmt.Fprintf(&b, "version = %q\n", p.Version)
		if p.RequiresPython != "" {
			fmt.Fprintf(&b, "requires-python = %q\n", p.RequiresPython)
		}
		if p.Marker != "" {
			fmt.Fprintf(&b, "marker = %q\n", p.Marker)
		}
		if len(p.Dependencies) > 0 {
			deps := append([]string(nil), p.Dependencies...)
			sort.Strings(deps)
			b.WriteString("dependencies = [")
			for i, d := range deps {
				if i > 0 {
					b.WriteString(", ")
				}
				fmt.Fprintf(&b, "%q", d)
			}
			b.WriteString("]\n")
		}
		wheels := append([]PyLockFile(nil), p.Wheels...)
		sort.SliceStable(wheels, func(i, j int) bool { return wheels[i].Name < wheels[j].Name })
		for _, w := range wheels {
			b.WriteString("\n[[packages.wheels]]\n")
			renderFile(&b, w)
		}
		if p.Sdist != nil {
			b.WriteString("\n[packages.sdist]\n")
			renderFile(&b, *p.Sdist)
		}
		b.WriteString("\n")
	}
	return b.String()
}

func renderFile(b *strings.Builder, f PyLockFile) {
	fmt.Fprintf(b, "name = %q\n", f.Name)
	fmt.Fprintf(b, "url = %q\n", f.URL)
	algos := make([]string, 0, len(f.Hashes))
	for k := range f.Hashes {
		algos = append(algos, k)
	}
	sort.Strings(algos)
	b.WriteString("hashes = {")
	for i, k := range algos {
		if i > 0 {
			b.WriteString(", ")
		}
		fmt.Fprintf(b, "%s = %q", k, f.Hashes[k])
	}
	b.WriteString("}\n")
}

// FromLockfile converts a uv.lock representation into a PyLock for PEP 751
// emission. The transformation is mostly mechanical; the bridge sets
// CreatedBy = "mochi-python-bridge".
func FromLockfile(l *Lockfile) *PyLock {
	out := &PyLock{
		LockVersion:    "1.0",
		CreatedBy:      "mochi-python-bridge",
		RequiresPython: l.RequiresPython,
	}
	for _, p := range l.Packages {
		pp := PyLockPackage{
			Name:    p.Name,
			Version: p.Version,
		}
		for _, d := range p.Dependencies {
			pp.Dependencies = append(pp.Dependencies, d.Name)
		}
		for _, w := range p.Wheels {
			pp.Wheels = append(pp.Wheels, hydrateLockedFile(w))
		}
		if p.Sdist != nil {
			f := hydrateLockedFile(*p.Sdist)
			pp.Sdist = &f
		}
		out.Packages = append(out.Packages, pp)
	}
	return out
}

func hydrateLockedFile(f LockedFile) PyLockFile {
	name := f.Filename
	if name == "" {
		// Derive from the URL's last path segment.
		if i := strings.LastIndexByte(f.URL, '/'); i >= 0 && i+1 < len(f.URL) {
			name = f.URL[i+1:]
		}
	}
	out := PyLockFile{
		Name:   name,
		URL:    f.URL,
		Hashes: map[string]string{},
	}
	if algo, hex, ok := SplitHash(f.Hash); ok {
		out.Hashes[algo] = hex
	}
	return out
}
