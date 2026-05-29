// Package emit is the MEP-74 phase 7 Mochi extern emitter. It
// consumes a wrapper.Result (the cgo //export wrapper set from
// phase 6) and produces a Mochi source file with matching
// `extern fun` declarations under a single user-chosen alias
// (the `as <alias>` token in `import go "<module>@<semver>" as
// <alias>`).
//
// The emitter is closed-grammar: every wrapper.EmittedFunc lowers
// to exactly one Mochi extern; the typemap.MochiType.String form
// is the source of truth for type rendering. Error-bearing
// wrappers lower to a Mochi `Result<T, string>` (MEP-13 sum type)
// so the caller can `match` over success/failure without a
// non-local control-flow side channel; this mirrors MEP-73's
// Rust-bridge convention so phase 7's emitter has a single
// audit-output template per language.
//
// The output is byte-deterministic: wrappers are sorted by their
// C symbol name, params keep their wrapper order, the file
// banner is the same across runs. Phase 10's lockfile records a
// SHA-256 of this file so a drift in wrapper input would cause a
// noticed lockfile churn rather than silent re-resolution.
package emit

import (
	"errors"
	"fmt"
	"sort"
	"strings"

	"mochi/package3/go/typemap"
	"mochi/package3/go/wrapper"
)

// ErrEmit is the package-wide error type for unrecoverable emit
// failures. Per-item failures land in Result.Skipped instead.
var ErrEmit = errors.New("emit: extern")

// Result is the output of one Emitter.Emit call. Files contains
// one entry, "externs.mochi", that the user includes via the
// MEP-74 phase 8 grammar extension.
type Result struct {
	// Source is the rendered Mochi source for the externs.mochi
	// file. Phase 9's build orchestrator writes this verbatim.
	Source string
	// Externs is the index of every extern fun the emitter
	// produced. Phase 8/9 consume this when resolving the
	// `import go ... as <alias>` selectors.
	Externs []EmittedExtern
	// Skipped lists wrapper.EmittedFunc entries the emitter did
	// not lower (none in baseline; reserved for future).
	Skipped []SkipNote
}

// EmittedExtern captures one Mochi extern fun declaration. It is
// the unit phase 8 looks up when resolving a `<alias>.<func>`
// reference against the import.
type EmittedExtern struct {
	// MochiName is the dotted name the Mochi caller references:
	// `<alias>.<GoFuncName>`.
	MochiName string
	// CSymbol is the C-side //export name; matches the wrapper.
	CSymbol string
	// Params keeps wrapper param order.
	Params []EmittedParam
	// Result is the single result MochiType (nil = unit). When
	// HasError is true the actual extern result is
	// `Result<Result, string>` (MEP-13 sum type).
	Result typemap.MochiType
	// HasError reports whether the wrapped Go func returned a
	// trailing error.
	HasError bool
}

// EmittedParam is one extern fun parameter.
type EmittedParam struct {
	Name string
	Type typemap.MochiType
}

// SkipNote records one wrapper item the emitter could not lower.
// Reason strings are stable; phase 11's audit report keys on them.
type SkipNote struct {
	GoSymbol string
	Reason   string
}

// Emitter renders one wrapper.Result into Mochi extern source.
type Emitter struct {
	wrap  *wrapper.Result
	alias string
}

// NewEmitter constructs an Emitter for the given wrapper result
// and import alias. alias is the `as <alias>` token from the
// import-go grammar; it must be a valid Mochi identifier (caller
// validates).
func NewEmitter(wrap *wrapper.Result, alias string) (*Emitter, error) {
	if wrap == nil {
		return nil, fmt.Errorf("%w: nil wrapper result", ErrEmit)
	}
	if alias == "" {
		return nil, fmt.Errorf("%w: empty alias", ErrEmit)
	}
	if !isIdent(alias) {
		return nil, fmt.Errorf("%w: alias %q is not a Mochi identifier", ErrEmit, alias)
	}
	return &Emitter{wrap: wrap, alias: alias}, nil
}

// Emit renders the extern source. The returned Result is non-nil
// even when individual items were skipped.
func (e *Emitter) Emit() (*Result, error) {
	// Sort by C symbol so output is byte-deterministic
	// independent of the wrapper's emission order. (The wrapper
	// already sorts internally; resorting here makes phase 7
	// robust against any future wrapper-side reordering.)
	funcs := make([]wrapper.EmittedFunc, len(e.wrap.Funcs))
	copy(funcs, e.wrap.Funcs)
	sort.Slice(funcs, func(i, j int) bool {
		return funcs[i].Symbol < funcs[j].Symbol
	})

	r := &Result{}
	for _, f := range funcs {
		ext, sk := e.emitOne(&f)
		if sk != nil {
			r.Skipped = append(r.Skipped, *sk)
			continue
		}
		r.Externs = append(r.Externs, *ext)
	}
	r.Source = e.render(r.Externs)
	return r, nil
}

// emitOne lowers one EmittedFunc to either an EmittedExtern or a
// SkipNote. The baseline never returns a SkipNote because phase 6
// guarantees every wrapper.EmittedFunc is fully bridgeable.
func (e *Emitter) emitOne(f *wrapper.EmittedFunc) (*EmittedExtern, *SkipNote) {
	params := make([]EmittedParam, 0, len(f.Params))
	for _, p := range f.Params {
		if p.Mochi == nil {
			return nil, &SkipNote{
				GoSymbol: f.Symbol,
				Reason:   "wrapper param missing MochiType",
			}
		}
		params = append(params, EmittedParam{Name: p.Name, Type: p.Mochi})
	}
	var resultT typemap.MochiType
	switch len(f.Results) {
	case 0:
		// nil result = unit
	case 1:
		resultT = f.Results[0].Mochi
		if resultT == nil {
			return nil, &SkipNote{
				GoSymbol: f.Symbol,
				Reason:   "wrapper result missing MochiType",
			}
		}
	default:
		// Multi-result tuples lower as a Mochi tuple type. Phase
		// 6 baseline short-circuits these as SkipNotes already,
		// so this branch is reserved for sub-phase 6.x outputs.
		return nil, &SkipNote{
			GoSymbol: f.Symbol,
			Reason:   "multi-result tuple lowering lands in phase 7.1",
		}
	}
	return &EmittedExtern{
		MochiName: e.alias + "." + f.GoName,
		CSymbol:   f.Symbol,
		Params:    params,
		Result:    resultT,
		HasError:  f.HasError,
	}, nil
}

// render builds the Mochi source. The banner annotates the file
// as machine-generated so editors and CI skip it on review.
func (e *Emitter) render(externs []EmittedExtern) string {
	var sb strings.Builder
	sb.WriteString("// Code generated by mochi MEP-74 phase 7 extern emitter. DO NOT EDIT.\n")
	sb.WriteString("// Alias: " + e.alias + "\n\n")
	for _, x := range externs {
		sb.WriteString(renderExtern(&x))
		sb.WriteString("\n")
	}
	return sb.String()
}

func renderExtern(x *EmittedExtern) string {
	var sb strings.Builder
	sb.WriteString("extern fun ")
	sb.WriteString(x.MochiName)
	sb.WriteByte('(')
	for i, p := range x.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.Name)
		sb.WriteString(": ")
		sb.WriteString(p.Type.String())
	}
	sb.WriteByte(')')

	res := renderResultType(x)
	if res != "" {
		sb.WriteString(" : ")
		sb.WriteString(res)
	}
	sb.WriteString("  // ")
	sb.WriteString(x.CSymbol)
	sb.WriteByte('\n')
	return sb.String()
}

// renderResultType returns the Mochi type expression for the
// extern's result slot. Error-bearing wrappers wrap the success
// value in Result<T, string> (MEP-13 sum type with payload).
// Unit returns produce empty (no `:` clause); unit-with-error
// becomes Result<unit, string> so the caller still pattern-matches.
func renderResultType(x *EmittedExtern) string {
	if x.HasError {
		if x.Result == nil {
			return "Result<unit, string>"
		}
		return "Result<" + x.Result.String() + ", string>"
	}
	if x.Result == nil {
		return ""
	}
	return x.Result.String()
}

// isIdent reports whether s is a Mochi identifier: first char is
// a letter or underscore, remaining chars are letter/digit/_.
// Phase 8's grammar enforces this on the user-typed alias.
func isIdent(s string) bool {
	for i, r := range s {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r == '_':
			// ok
		case (r >= '0' && r <= '9') && i > 0:
			// ok
		default:
			return false
		}
	}
	return len(s) > 0
}
