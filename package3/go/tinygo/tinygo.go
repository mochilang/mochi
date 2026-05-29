// Package tinygo is the MEP-74 phase 16 embedded-subset bridge.
// It defines the `profile = "embedded"` opt-in: a closed subset of
// stdlib imports and surface idioms that TinyGo can compile, plus
// a `//go:linkname` wrapper renderer for the cases where the
// regular `//export` cgo path is unavailable (TinyGo on
// wasm-js / wasi-libc / baremetal targets has no working cgo, so
// the wrapper has to bind to source-module symbols via the
// linkname compiler directive instead).
//
// The package has three layers:
//
//  1. A profile descriptor + banned-import / banned-type sets that
//     define which surfaces are allowed under
//     `profile = "embedded"`. The MEP-74 spec's §"TinyGo subset"
//     pins the canonical set; this file is the executable form.
//  2. A `CheckPackage` walker that takes an `apisurface.Package`
//     and reports every export that violates the subset. Each
//     violation is one line in a deterministically-sorted
//     `[]Violation` slice so the wrapper synthesiser can surface
//     them as SkipReport entries.
//  3. A `RenderLinkname` renderer that emits a single
//     `//go:linkname`-decorated wrapper file. The wrapper has no
//     body of its own: the linkname directive aliases the local
//     symbol to the source-module's exported function so the
//     downstream TinyGo build links the two together directly.
//
// Out of scope for v1 (deferred to 16.1+): a real `tinygo build`
// gate (the sentinel uses the regular `go` toolchain as a
// stand-in), the wasm-js / wasi-libc target-specific link-helper
// emission, and the per-symbol `//go:wasmexport` directive for the
// wasm-js side.
package tinygo

import (
	"errors"
	"fmt"
	"sort"
	"strings"

	"mochi/package3/go/apisurface"
)

// ErrTinygo is the package-wide error sentinel.
var ErrTinygo = errors.New("tinygo")

// Profile names the build profile a wrapper is being generated for.
// `Standard` is the default cgo-c-archive path; `Embedded` is the
// TinyGo subset gated by this package.
type Profile string

const (
	// ProfileStandard is the default cgo + c-archive build path.
	ProfileStandard Profile = "standard"
	// ProfileEmbedded is the TinyGo subset profile.
	ProfileEmbedded Profile = "embedded"
)

// IsValid reports whether p is one of the recognised profiles.
func (p Profile) IsValid() bool {
	switch p {
	case ProfileStandard, ProfileEmbedded:
		return true
	}
	return false
}

// BannedImports is the closed list of stdlib import paths the
// embedded subset refuses. The list is intentionally narrow: it
// covers packages that pull in cgo, runtime reflection, or
// network/process surfaces TinyGo cannot link on baremetal /
// wasm-js targets.
//
// Sorted lexicographically; the order is load-bearing for the
// deterministic Violation reporting.
var BannedImports = []string{
	"debug/buildinfo",
	"debug/dwarf",
	"debug/elf",
	"debug/gosym",
	"debug/macho",
	"debug/pe",
	"debug/plan9obj",
	"encoding/gob",
	"go/ast",
	"go/parser",
	"go/types",
	"net/http",
	"net/rpc",
	"net/smtp",
	"os/exec",
	"plugin",
	"reflect",
	"runtime/cgo",
	"runtime/debug",
	"runtime/pprof",
	"runtime/trace",
	"syscall/js",
	"text/template",
	"unsafe",
}

// BannedTypePrefixes lists qualified-type prefixes the embedded
// subset refuses in any exported position (parameter, result,
// field, method signature). A type qualifies if its rendered
// form starts with any of these prefixes (allowing for a leading
// `*`, `[]`, `[N]`, or `map[...]` wrapper).
var BannedTypePrefixes = []string{
	"reflect.",
	"runtime/cgo.Handle",
	"runtime/debug.",
	"unsafe.Pointer",
}

// Violation is one embedded-subset incompatibility. Kind names the
// category; Where is the surface element (import path or
// `<pkg>.<Ident>`); Reason is a short human-readable explanation.
type Violation struct {
	Kind   string
	Where  string
	Reason string
}

// String renders the violation as `<kind>: <where>: <reason>`.
func (v Violation) String() string {
	return v.Kind + ": " + v.Where + ": " + v.Reason
}

// CheckPackage walks an apisurface.Package and returns every
// embedded-subset violation it finds. The returned slice is
// sorted by (Kind, Where) so the output is byte-deterministic for
// the phase 10 lockfile-pin path.
//
// CheckPackage returns nil for any profile other than
// ProfileEmbedded (a no-op for the standard cgo path).
func CheckPackage(profile Profile, pkg apisurface.Package) []Violation {
	if profile != ProfileEmbedded {
		return nil
	}
	var out []Violation

	bannedImp := map[string]struct{}{}
	for _, b := range BannedImports {
		bannedImp[b] = struct{}{}
	}

	for _, imp := range pkg.Imports {
		if _, hit := bannedImp[imp]; hit {
			out = append(out, Violation{
				Kind:   "import",
				Where:  imp,
				Reason: "stdlib import is outside the embedded subset",
			})
		}
	}

	for _, f := range pkg.Funcs {
		out = append(out, checkFunc(pkg.ImportPath, "", f)...)
	}
	for _, t := range pkg.Types {
		for _, m := range t.Methods {
			out = append(out, checkFunc(pkg.ImportPath, t.Name, m)...)
		}
		for _, m := range t.InterfaceMethods {
			out = append(out, checkFunc(pkg.ImportPath, t.Name, m)...)
		}
	}

	sort.SliceStable(out, func(i, j int) bool {
		if out[i].Kind != out[j].Kind {
			return out[i].Kind < out[j].Kind
		}
		return out[i].Where < out[j].Where
	})
	return out
}

func checkFunc(pkgPath, receiver string, f apisurface.Func) []Violation {
	var out []Violation
	where := pkgPath + "."
	if receiver != "" {
		where += receiver + "."
	}
	where += f.Name
	for _, p := range f.Params {
		if r := bannedTypeReason(p.Type); r != "" {
			out = append(out, Violation{Kind: "param-type", Where: where, Reason: r})
		}
	}
	for _, r := range f.Results {
		if reason := bannedTypeReason(r.Type); reason != "" {
			out = append(out, Violation{Kind: "result-type", Where: where, Reason: reason})
		}
	}
	return out
}

// bannedTypeReason returns the offending reason if t (after
// peeling slice/array/map/pointer wrappers) matches a banned
// prefix, else "".
func bannedTypeReason(t string) string {
	stripped := stripWrappers(t)
	for _, pre := range BannedTypePrefixes {
		if strings.HasPrefix(stripped, pre) {
			return stripped + " is outside the embedded subset"
		}
	}
	return ""
}

func stripWrappers(t string) string {
	for {
		switch {
		case strings.HasPrefix(t, "*"):
			t = strings.TrimPrefix(t, "*")
		case strings.HasPrefix(t, "[]"):
			t = strings.TrimPrefix(t, "[]")
		case strings.HasPrefix(t, "..."):
			t = strings.TrimPrefix(t, "...")
		case strings.HasPrefix(t, "map["):
			// Drop the key half and the closing bracket; the
			// value half is what we want to examine.
			idx := strings.Index(t, "]")
			if idx < 0 {
				return t
			}
			t = t[idx+1:]
		case strings.HasPrefix(t, "[") && !strings.HasPrefix(t, "[]"):
			// Fixed-size array `[N]T`: drop the `[N]`.
			idx := strings.Index(t, "]")
			if idx < 0 {
				return t
			}
			t = t[idx+1:]
		default:
			return t
		}
	}
}

// IsCompatible reports whether pkg has zero embedded-subset
// violations under the given profile. Always true for the
// standard profile.
func IsCompatible(profile Profile, pkg apisurface.Package) bool {
	return len(CheckPackage(profile, pkg)) == 0
}

// LinknameSpec is the input for RenderLinkname: one wrapper
// binding a local symbol to a source-module function via
// `//go:linkname`.
type LinknameSpec struct {
	// LocalName is the wrapper symbol's local name, e.g.
	// `mochi_<module>_<Ident>`. Becomes the function name in the
	// rendered output.
	LocalName string
	// TargetSymbol is the fully-qualified source symbol the
	// linkname directive aliases to. Format:
	// `<package-import-path>.<Ident>` (e.g.
	// `example.com/src.Sort`).
	TargetSymbol string
	// Params lists the wrapper's input parameters in declaration
	// order. Empty for parameterless wrappers.
	Params []apisurface.Param
	// Results lists the wrapper's return values in declaration
	// order. Empty for void wrappers.
	Results []apisurface.Param
	// Variadic marks the last param as `...T`-shaped (the
	// directive itself doesn't change, but the rendered signature
	// matches the source's variadic suffix).
	Variadic bool
}

// Validate rejects structurally-empty specs.
func (s LinknameSpec) Validate() error {
	if strings.TrimSpace(s.LocalName) == "" {
		return fmt.Errorf("%w: LocalName is required", ErrTinygo)
	}
	if strings.TrimSpace(s.TargetSymbol) == "" {
		return fmt.Errorf("%w: TargetSymbol is required", ErrTinygo)
	}
	if dot := strings.LastIndex(s.TargetSymbol, "."); dot <= 0 {
		return fmt.Errorf("%w: TargetSymbol %q must be 'package/path.Ident'", ErrTinygo, s.TargetSymbol)
	}
	return nil
}

// RenderLinkname emits the Go source for one linkname wrapper.
// The output is the wrapper function declaration (with an
// `//go:linkname` directive immediately above) plus the magic
// `import _ "unsafe"` line; the caller is responsible for the
// package clause.
//
// The wrapper has no body: the linkname directive aliases the
// local symbol to the target so the linker resolves the two as
// the same address. This is the TinyGo-compatible analogue of the
// phase 6 cgo `//export` path.
//
// The output is byte-deterministic across calls.
func RenderLinkname(spec LinknameSpec) (string, error) {
	if err := spec.Validate(); err != nil {
		return "", err
	}
	var sb strings.Builder
	sb.WriteString("// " + spec.LocalName + " is the //go:linkname wrapper for\n")
	sb.WriteString("// " + spec.TargetSymbol + ".\n")
	sb.WriteString("// Generated by mochi MEP-74 phase 16 (embedded profile). DO NOT EDIT.\n")
	sb.WriteString("\n")
	sb.WriteString("//go:linkname " + spec.LocalName + " " + spec.TargetSymbol + "\n")
	sb.WriteString("func " + spec.LocalName + "(")
	for i, p := range spec.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		name := p.Name
		if name == "" {
			name = fmt.Sprintf("p%d", i)
		}
		typ := p.Type
		if spec.Variadic && i == len(spec.Params)-1 {
			typ = "..." + strings.TrimPrefix(typ, "...")
		}
		sb.WriteString(name + " " + typ)
	}
	sb.WriteString(")")
	switch len(spec.Results) {
	case 0:
	case 1:
		sb.WriteString(" " + spec.Results[0].Type)
	default:
		sb.WriteString(" (")
		for i, r := range spec.Results {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(r.Type)
		}
		sb.WriteString(")")
	}
	sb.WriteString("\n")
	return sb.String(), nil
}

// RenderFile combines the package clause, the unsafe import
// required by the //go:linkname directive, and the concatenated
// rendered specs into one Go source file. The output begins with
// a `//go:build !mochi_no_linkname` build tag so the file
// disappears when the consumer explicitly opts out (e.g. a host
// `go vet` run that doesn't want to follow the directive).
//
// pkgName must be a valid Go identifier; specs is the list of
// linkname wrappers to render in declaration order.
func RenderFile(pkgName string, specs []LinknameSpec) (string, error) {
	if strings.TrimSpace(pkgName) == "" {
		return "", fmt.Errorf("%w: pkgName is required", ErrTinygo)
	}
	var sb strings.Builder
	sb.WriteString("// Code generated by mochi MEP-74 phase 16 (embedded profile). DO NOT EDIT.\n")
	sb.WriteString("\n")
	sb.WriteString("//go:build !mochi_no_linkname\n")
	sb.WriteString("\n")
	sb.WriteString("package " + pkgName + "\n")
	sb.WriteString("\n")
	sb.WriteString("import _ \"unsafe\" // for //go:linkname\n")
	sb.WriteString("\n")
	for i, s := range specs {
		out, err := RenderLinkname(s)
		if err != nil {
			return "", fmt.Errorf("%w: spec %d (%s): %v", ErrTinygo, i, s.LocalName, err)
		}
		sb.WriteString(out)
		sb.WriteString("\n")
	}
	return sb.String(), nil
}
