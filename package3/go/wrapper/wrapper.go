// Package wrapper is the MEP-74 phase 6 cgo wrapper synthesiser. It
// takes an apisurface.Surface plus a typemap.Mapper and emits a Go
// source tree implementing the cgo //export bridge from C-ABI to
// the source Go module. The output is consumed by phase 9's build
// orchestration: `go build -buildmode=c-archive` over the emitted
// tree produces libwrap.a, which the MEP-54 link step folds into
// the final binary.
//
// Phase 6 handles the *baseline* lowering rules: scalar in/out,
// string out (with module-scoped _string_free), []byte out (with
// _bytes_free), error out (status code + out_err pointer), and
// multi-result tuples. Channels, function values, maps, generic
// instantiations, and full struct records emit SkipNotes and are
// completed in follow-up sub-phases (6.1+).
package wrapper

import (
	"errors"
	"fmt"
	"go/parser"
	"go/token"
	"sort"
	"strings"

	"mochi/package3/go/apisurface"
	"mochi/package3/go/typemap"
)

// ErrEmit is returned when the emitter cannot produce a wrapper for
// the whole surface. Skip-individual-item failures do not surface
// here; they are returned as SkipNote entries on the Result.
var ErrEmit = errors.New("wrapper: emit")

// Result is what one call to Emitter.Emit returns.
type Result struct {
	// ModuleName is the synthesised Go package name (e.g.
	// "mochi_go_github_com_spf13_cobra"). It is also the directory
	// name and the //export-symbol prefix.
	ModuleName string
	// Files maps relative file name -> Go source. The caller writes
	// them under the wrapper directory chosen by phase 9.
	Files map[string]string
	// Funcs is the index of every wrapper function that the emitter
	// produced. Phase 7 (extern emitter) consumes this directly to
	// build the matching Mochi extern fn declarations.
	Funcs []EmittedFunc
	// Skipped is the closed-reason list of items the emitter could
	// not lower in phase 6. Each entry can be picked up by a follow-
	// up sub-phase.
	Skipped []SkipNote
}

// EmittedFunc captures a single //export wrapper function the
// emitter produced. It is exposed so phase 7 can reuse the same
// argument-name conventions.
type EmittedFunc struct {
	// Symbol is the C-side //export name.
	Symbol string
	// Package is the source Go package's import path.
	Package string
	// GoName is the original Go function name (no package prefix).
	GoName string
	// Params is the wrapper's C-ABI parameter list (in order).
	Params []EmittedParam
	// Results is the wrapper's C-ABI result list (in order). For
	// error-returning Go funcs, the last result is replaced by a
	// MochiStatus int return code and the error string is delivered
	// via an out-pointer in Params.
	Results []EmittedParam
	// HasError reports whether the wrapped function returned a
	// trailing error value lowered to a status code.
	HasError bool
}

// EmittedParam is one wrapper-function parameter or result.
type EmittedParam struct {
	Name  string
	CType string
	// Mochi is the typemap.MochiType this slot exposes. It is
	// recorded so phase 7 can emit the matching Mochi extern type
	// without re-mapping from scratch.
	Mochi typemap.MochiType
}

// SkipNote records one item the emitter deliberately omitted from
// the wrapper. Reason values are stable strings.
type SkipNote struct {
	Kind     string // "func", "method", "type"
	Package  string
	Name     string
	Position string
	Reason   string
}

// Emitter synthesises a cgo wrapper for one Go module.
type Emitter struct {
	surface *apisurface.Surface
	mapper  *typemap.Mapper
	module  string
	version string
	// flatModule is the module path with all non-identifier characters
	// replaced by underscore (e.g. "github.com/spf13/cobra" ->
	// "github_com_spf13_cobra"). Used as the //export symbol prefix.
	flatModule string
}

// NewEmitter constructs an Emitter for the given module/version
// pair. surface and mapper must not be nil.
func NewEmitter(surface *apisurface.Surface, mapper *typemap.Mapper, module, version string) (*Emitter, error) {
	if surface == nil {
		return nil, fmt.Errorf("%w: nil surface", ErrEmit)
	}
	if mapper == nil {
		return nil, fmt.Errorf("%w: nil mapper", ErrEmit)
	}
	if module == "" {
		return nil, fmt.Errorf("%w: empty module path", ErrEmit)
	}
	return &Emitter{
		surface:    surface,
		mapper:     mapper,
		module:     module,
		version:    version,
		flatModule: flattenModule(module),
	}, nil
}

// Emit produces the wrapper source files. The returned Result is
// non-nil even when individual items were skipped; check
// Result.Skipped to see what was omitted.
func (e *Emitter) Emit() (*Result, error) {
	pkgName := "mochi_go_" + e.flatModule
	r := &Result{
		ModuleName: pkgName,
		Files:      make(map[string]string),
	}

	// Walk packages in sorted order so the output is byte-stable.
	pkgPaths := e.surface.PackagePaths()
	sort.Strings(pkgPaths)
	for _, ip := range pkgPaths {
		pv := e.surface.Packages[ip]
		if pv.Pkg.IsMain {
			r.Skipped = append(r.Skipped, SkipNote{
				Kind:    "package",
				Package: ip,
				Name:    pv.Pkg.Name,
				Reason:  "main package has no library API",
			})
			continue
		}
		for _, fname := range sortedFuncNames(pv) {
			fd := pv.Funcs[fname]
			ef, sk, err := e.emitFunc(ip, pv.Pkg.Name, fd)
			if err != nil {
				return nil, err
			}
			if sk != nil {
				r.Skipped = append(r.Skipped, *sk)
				continue
			}
			r.Funcs = append(r.Funcs, *ef)
		}
	}

	src, err := e.renderSource(pkgName, r.Funcs)
	if err != nil {
		return nil, err
	}
	r.Files["wrap.go"] = src
	r.Files["wrap_handles.go"] = e.renderHandles(pkgName)
	return r, nil
}

func sortedFuncNames(pv *apisurface.PackageView) []string {
	names := make([]string, 0, len(pv.Funcs))
	for n := range pv.Funcs {
		names = append(names, n)
	}
	sort.Strings(names)
	return names
}

// flattenModule converts a Go module path to a valid Go identifier
// fragment: every non-letter/digit becomes underscore.
func flattenModule(m string) string {
	var sb strings.Builder
	for _, r := range m {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
			sb.WriteRune(r)
		default:
			sb.WriteByte('_')
		}
	}
	out := sb.String()
	// Trim leading digits so the result is a valid Go identifier.
	for len(out) > 0 && out[0] >= '0' && out[0] <= '9' {
		out = "_" + out
		break
	}
	return out
}

// ParseGenerated parses src under the strict cgo-compatible
// parser settings. Phase 6's gate runs this on every emitted file
// to confirm syntactic validity.
func ParseGenerated(name, src string) error {
	_, err := parser.ParseFile(token.NewFileSet(), name, src, parser.AllErrors|parser.ParseComments)
	if err != nil {
		return fmt.Errorf("%w: %s: %v", ErrEmit, name, err)
	}
	return nil
}
