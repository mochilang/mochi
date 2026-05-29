// Package wrapper synthesises the Rust wrapper crate that exposes
// an upstream crate's public API via an extern "C" surface. It
// consumes the ApiSurface from package3/rust/rustdoc (phase 2) and
// uses the closed type-mapping table from package3/rust/typemap
// (phase 3) to lower each function into a SynthFn carrying the
// extern-C signature plus a Rust body.
//
// The produced Crate carries three rendered files: src/lib.rs,
// Cargo.toml, and SKIPPED.txt. Later phases feed these into a
// cargo build (phase 7) and link the resulting cdylib into the
// Mochi runtime.
package wrapper

import (
	"fmt"

	"mochi/package3/rust/asyncbridge"
	"mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
	"mochi/package3/rust/typemap"
)

// Crate is a generated wrapper crate ready to be written to disk.
type Crate struct {
	// Name is the wrapper crate's Cargo package name, e.g.
	// "mochi_wrap_hex". Mangled from the upstream crate name.
	Name string

	// Upstream is the upstream crate name, e.g. "hex".
	Upstream string

	// UpstreamVersion is the upstream version, e.g. "0.4.3".
	UpstreamVersion string

	// Functions is the list of successfully synthesised extern-C
	// wrappers, in walker order.
	Functions []SynthFn

	// Skipped is the union of (a) the SkipReports from ApiSurface
	// (i.e. items the phase-2 walker already classified) and (b)
	// the new SkipReports produced when typemap refuses a function
	// signature.
	Skipped []errors.SkipReport

	// AsyncFlavor is the tokio runtime flavor the wrapper crate
	// uses when any synthesised fn is `async fn`. Defaults to
	// current-thread; callers override from the `[rust.runtime]`
	// section of mochi.toml.
	AsyncFlavor asyncbridge.Flavor
}

// HasAsync reports whether any synthesised function is `async fn`.
// When true, EmitLibRS adds `mod mochi_rt;` to the prologue and
// EmitCargoTOML emits tokio + once_cell dependency rows.
func (c *Crate) HasAsync() bool {
	for _, fn := range c.Functions {
		if fn.IsAsync {
			return true
		}
	}
	return false
}

// SynthFn is a single synthesised extern-C wrapper.
type SynthFn struct {
	// ExternName is the exported symbol, e.g.
	// "mochi_hex_encode". Already mangled.
	ExternName string

	// UpstreamPath is the upstream Rust path, e.g.
	// "hex::encode". The wrapper body calls this.
	UpstreamPath string

	// Params is the input parameter list in positional order.
	Params []SynthParam

	// Return is the return-type mapping. Nil iff the function
	// returns the unit type.
	Return *typemap.Mapping

	// IsAsync marks an upstream `async fn`. The emitter wraps the
	// body in `mochi_rt::block_on(async { ... .await })` and the
	// crate-level emit prepends the mochi_rt module + tokio deps.
	IsAsync bool
}

// SynthParam is one input parameter.
type SynthParam struct {
	Name    string
	Mapping typemap.Mapping
}

// Synth lowers an ApiSurface into a Crate. Each function in the
// surface is run through typemap.Map (Direction=In for params,
// Direction=Out for the return). Functions whose signature fails to
// map produce a SkipReport rather than a SynthFn; the
// SkipReport.ItemPath quotes the function's full module-qualified
// path, the Reason is taken from the first mapping refusal, and the
// Detail prefixes which param (or return) failed.
//
// Synth assumes the surface has already been filtered for visibility
// by the phase-2 walker.
func Synth(upstream, version string, surface *rustdoc.ApiSurface) *Crate {
	c := &Crate{
		Name:            crateName(upstream),
		Upstream:        upstream,
		UpstreamVersion: version,
	}
	for _, fn := range surface.Functions {
		sf, sr, detail := synthFn(upstream, fn)
		if sr != errors.SkipUnknown || detail != "" {
			c.Skipped = append(c.Skipped, errors.SkipReport{
				ItemPath: joinPath(fn.Path),
				Reason:   sr,
				Detail:   detail,
			})
			continue
		}
		c.Functions = append(c.Functions, *sf)
	}
	// Carry forward the walker's existing SkipReports.
	c.Skipped = append(c.Skipped, surface.Skipped...)
	return c
}

func synthFn(upstream string, fn rustdoc.FunctionEntry) (*SynthFn, errors.SkipReason, string) {
	params := make([]SynthParam, 0, len(fn.Inputs))
	for i, in := range fn.Inputs {
		m, sr, det := typemap.Map(in.Type, typemap.DirectionIn)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, fmt.Sprintf("param %d (%s): %s", i, in.Name, det)
		}
		name := in.Name
		if name == "" {
			name = fmt.Sprintf("arg%d", i)
		}
		params = append(params, SynthParam{Name: name, Mapping: *m})
	}
	var ret *typemap.Mapping
	if fn.Output != nil {
		m, sr, det := typemap.Map(*fn.Output, typemap.DirectionOut)
		if sr != errors.SkipUnknown || det != "" {
			return nil, sr, fmt.Sprintf("return: %s", det)
		}
		if m.Kind != typemap.KindUnit {
			ret = m
		}
	}
	return &SynthFn{
		ExternName:   externName(upstream, fn.Path),
		UpstreamPath: joinPath(fn.Path),
		Params:       params,
		Return:       ret,
		IsAsync:      fn.Header.IsAsync,
	}, errors.SkipUnknown, ""
}

// crateName mangles an upstream crate name into the wrapper crate
// name. Hyphens become underscores, mixed-case becomes lower; the
// "mochi_wrap_" prefix marks the crate as bridge-synthesised.
func crateName(upstream string) string {
	return "mochi_wrap_" + sanitizeIdent(upstream)
}

// externName mangles a function's module-qualified path into a
// stable, ASCII extern-C symbol.
func externName(upstream string, path []string) string {
	out := "mochi_" + sanitizeIdent(upstream)
	for _, seg := range path[1:] {
		out += "_" + sanitizeIdent(seg)
	}
	return out
}

func sanitizeIdent(s string) string {
	b := make([]byte, 0, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c >= 'a' && c <= 'z', c >= '0' && c <= '9', c == '_':
			b = append(b, c)
		case c >= 'A' && c <= 'Z':
			b = append(b, c-'A'+'a')
		default:
			b = append(b, '_')
		}
	}
	return string(b)
}

func joinPath(p []string) string {
	if len(p) == 0 {
		return ""
	}
	out := p[0]
	for _, s := range p[1:] {
		out += "::" + s
	}
	return out
}
