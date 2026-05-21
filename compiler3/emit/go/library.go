package gogen

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/compiler3/ir"
)

// Library is the multi-file output unit Phase 8 of MEP-43 (mochi build
// --emit=go-library) consumes. The output is a normal Go package; the
// consumer points its `import` at ModulePath and calls the Go-exported
// (capitalized) functions directly. There are no per-symbol stubs;
// every Mochi `export fn` is emitted as a Go top-level function in
// pkg.go.
type Library struct {
	// ModulePath is the importable module path the produced go.mod
	// declares (e.g. "example.com/mypkg"). The consumer's go.mod uses
	// a `replace` directive to point at the on-disk produced package
	// during local development; published packages omit the replace.
	ModulePath string
	// PkgName is the Go package name as it appears in `package ...`.
	// Defaults to the last path segment of ModulePath.
	PkgName string
	// Funcs is the IR function table. Functions whose Name starts with
	// an uppercase letter are emitted as Go-public symbols. The
	// emitter does not rename; the IR builder (or hand-built fixture)
	// is responsible for capitalising exports.
	Funcs []*ir.Function
	// GoVersion is the `go 1.X` directive in the produced go.mod.
	// Defaults to "1.22".
	GoVersion string
	// RuntimeReplace, when non-empty, adds a `replace mochi => <path>`
	// directive in go.mod. Used by tests so the produced package can
	// resolve `mochi/runtime/mochi/query` without publishing.
	RuntimeReplace string
}

// EmitLibrary lowers lib to a normal Go package. The return value is
// a map from relative file path to file contents, ready for the
// caller to write to disk.
//
// Layout:
//   - <PkgName>.go        public surface; every function (including
//                         lowercase helpers) lives here. No stubs.
//   - go.mod              module + go directive + optional replace.
//
// Lowercase helper functions remain in the same file because keeping
// them in a sibling internal/ would force a wrapper-emit pattern
// (every public fn delegates to internal/), which the MEP-43 gate
// explicitly forbids ("no per-symbol stubs").
func EmitLibrary(lib *Library) (map[string][]byte, error) {
	if lib.ModulePath == "" {
		return nil, fmt.Errorf("EmitLibrary: ModulePath is required")
	}
	if lib.PkgName == "" {
		lib.PkgName = defaultPkgName(lib.ModulePath)
	}
	if lib.GoVersion == "" {
		lib.GoVersion = "1.22"
	}

	src, err := Emit(&Program{
		PkgName: lib.PkgName,
		Funcs:   lib.Funcs,
	})
	if err != nil {
		return nil, fmt.Errorf("EmitLibrary: %w", err)
	}

	var mod bytes.Buffer
	fmt.Fprintf(&mod, "module %s\n\n", lib.ModulePath)
	fmt.Fprintf(&mod, "go %s\n", lib.GoVersion)

	// If any emitted function references `mochi/runtime/mochi/...`,
	// add the require + optional replace so the package builds.
	if bytes.Contains(src, []byte("\"mochi/runtime/mochi/")) {
		fmt.Fprintf(&mod, "\nrequire mochi v0.0.0\n")
		if lib.RuntimeReplace != "" {
			fmt.Fprintf(&mod, "\nreplace mochi => %s\n", lib.RuntimeReplace)
		}
	} else if lib.RuntimeReplace != "" {
		// Still permit a replace for downstream tests that import
		// mochi without using query helpers (rare; future proof).
		fmt.Fprintf(&mod, "\nrequire mochi v0.0.0\n")
		fmt.Fprintf(&mod, "\nreplace mochi => %s\n", lib.RuntimeReplace)
	}

	files := map[string][]byte{
		lib.PkgName + ".go": src,
		"go.mod":            mod.Bytes(),
	}
	return files, nil
}

// defaultPkgName extracts a Go-valid package name from a module path.
// It returns the last path segment and replaces any hyphens with
// underscores so the result is a valid Go identifier.
func defaultPkgName(modulePath string) string {
	parts := strings.Split(modulePath, "/")
	last := parts[len(parts)-1]
	last = strings.ReplaceAll(last, "-", "_")
	return last
}

// SortedFilenames returns the file names of an EmitLibrary result in
// deterministic order. Useful for tests and for callers that want a
// stable iteration order when writing to disk.
func SortedFilenames(files map[string][]byte) []string {
	out := make([]string, 0, len(files))
	for k := range files {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}
