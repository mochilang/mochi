package stubs

import (
	"fmt"
	"os"
	"path/filepath"
)

// Typeshed is a pinned local checkout of github.com/python/typeshed used as
// the tier-3 stub source. The bridge does not fetch typeshed itself; the
// workspace orchestrator (phase 8) ensures a checkout exists at the pinned
// commit before discovery runs.
type Typeshed struct {
	// Root is the local typeshed checkout directory.
	Root string
	// Commit is the pinned commit SHA. It is recorded in the lockfile but
	// otherwise unused at lookup time; the bridge trusts the Root to be at
	// that commit.
	Commit string
}

// NewTypeshed wraps a local checkout. Root must exist and contain `stdlib/` or
// `stubs/` subdirectories (the typeshed layout).
func NewTypeshed(root, commit string) (*Typeshed, error) {
	if root == "" {
		return nil, fmt.Errorf("typeshed: empty root")
	}
	info, err := os.Stat(root)
	if err != nil {
		return nil, fmt.Errorf("typeshed: stat %q: %w", root, err)
	}
	if !info.IsDir() {
		return nil, fmt.Errorf("typeshed: %q is not a directory", root)
	}
	// Soft check: at least one of the two canonical subtrees should exist.
	hasStdlib := dirExists(filepath.Join(root, "stdlib"))
	hasStubs := dirExists(filepath.Join(root, "stubs"))
	if !hasStdlib && !hasStubs {
		return nil, fmt.Errorf("typeshed: %q does not look like a typeshed checkout (no stdlib/ or stubs/)", root)
	}
	return &Typeshed{Root: root, Commit: commit}, nil
}

// Lookup searches typeshed for `name` (PEP 503 normalised) and returns a
// StubSource on hit. Returns (nil, false) when not found.
//
// Search order within typeshed:
//   1. stubs/<name>/<module>/ for third-party stubs.
//   2. stdlib/<module>/ for stdlib modules where name == module.
func (ts *Typeshed) Lookup(name string) (*StubSource, bool) {
	module := moduleFromName(name)
	// Third-party: `stubs/<name>/<module>` with a top-level METADATA.toml.
	thirdParty := filepath.Join(ts.Root, "stubs", name, module)
	if dirExists(thirdParty) {
		files, _ := walkStubFiles(thirdParty)
		return &StubSource{
			Package: name,
			Tier:    TierTypeshed,
			RootDir: thirdParty,
			Files:   files,
		}, true
	}
	// Some typeshed entries place the stubs at `stubs/<name>` directly.
	thirdParty2 := filepath.Join(ts.Root, "stubs", name)
	if dirExists(thirdParty2) {
		files, _ := walkStubFiles(thirdParty2)
		return &StubSource{
			Package: name,
			Tier:    TierTypeshed,
			RootDir: thirdParty2,
			Files:   files,
		}, true
	}
	// Stdlib: `stdlib/<module>.pyi` or `stdlib/<module>/`.
	stdlibDir := filepath.Join(ts.Root, "stdlib", module)
	if dirExists(stdlibDir) {
		files, _ := walkStubFiles(stdlibDir)
		return &StubSource{
			Package: name,
			Tier:    TierTypeshed,
			RootDir: stdlibDir,
			Files:   files,
		}, true
	}
	stdlibFile := filepath.Join(ts.Root, "stdlib", module+".pyi")
	if _, err := os.Stat(stdlibFile); err == nil {
		return &StubSource{
			Package: name,
			Tier:    TierTypeshed,
			RootDir: filepath.Join(ts.Root, "stdlib"),
			Files:   []string{module + ".pyi"},
		}, true
	}
	return nil, false
}

// moduleFromName converts a PEP 503 distribution name into the most likely
// import module name. `Flask-SQLAlchemy` -> `flask_sqlalchemy`.
func moduleFromName(name string) string {
	out := make([]byte, len(name))
	for i := 0; i < len(name); i++ {
		c := name[i]
		switch {
		case c >= 'A' && c <= 'Z':
			out[i] = c + ('a' - 'A')
		case c == '-':
			out[i] = '_'
		default:
			out[i] = c
		}
	}
	return string(out)
}

func dirExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
}
