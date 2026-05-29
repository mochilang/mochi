// Package library is the MEP-73 publish-direction emit layer. It owns
// the lowering of a Mochi package's public surface (the things its
// `pub fn` / `pub type` / `pub struct` declarations expose) into a
// publishable Rust library crate suitable for `mochi pkg publish
// --to=crates.io`.
//
// The package is layering-conservative: it imports no other
// package3/rust/* module. Callers in the MEP-53 build driver compose
// a PublicAPI from their own IR state and hand it to Render; the
// rendered files are an in-memory map suitable for either disk
// materialisation or direct ingestion by a downstream `cargo build`.
//
// Per MEP-73 §3 Direction 2, the rendered crate sets
//   [lib] crate-type = ["rlib", "cdylib"]
// so it links as both a Rust rlib (consumed by downstream Cargo
// users) and a cdylib (consumed by C / C++ / Mochi / other languages
// via the cbindgen header).
package library

import (
	"fmt"
	"sort"
	"strings"
)

// PublicAPI is the lowered public surface of a Mochi package. It is
// the closed input shape Render consumes; callers build it from their
// own IR state and never read it back.
type PublicAPI struct {
	// CrateName is the published crate name. Must match cargo
	// naming rules (lowercase letters, digits, underscores,
	// hyphens; must start with a letter).
	CrateName string
	// Version is the published crate version. Full N.N.N triple
	// or N.N.N-pre per cargo conventions.
	Version string
	// Items is the rendered public surface in source order.
	Items []Item
	// Package is the publish-side metadata block.
	Package PackageMeta
	// Dependencies is the [dependencies] table of the rendered
	// crate, mapping crate name to version requirement. Optional;
	// most published crates declare a small dep set here.
	Dependencies map[string]string
	// CHeader controls whether a cbindgen-compatible C header is
	// rendered. The header includes only Extern functions and
	// ReprC types.
	CHeader bool
	// NoStd renders src/lib.rs with `#![no_std]` and depends on
	// alloc explicitly. Used by the Phase 13 embedded subset.
	NoStd bool
}

// PackageMeta is the publish-side `[package]` table per crates.io
// metadata schema. Empty fields are omitted from the rendered TOML.
type PackageMeta struct {
	Description   string
	License       string
	Repository    string
	Documentation string
	Homepage      string
	Keywords      []string
	Categories    []string
	Authors       []string
	// Readme is the path to the README file the crate ships with,
	// e.g. "README.md". Empty means do not emit a `readme = ` row.
	Readme string
	// Edition is the Rust edition. Defaults to "2021" when empty.
	Edition string
	// RustVersion is the minimum supported Rust version. Empty
	// means do not pin a MSRV.
	RustVersion string
}

// Item is one renderable surface item. Implementations are ItemFn,
// ItemStruct, and ItemEnum. The sealed interface keeps the surface
// closed.
type Item interface {
	itemKind() string
	name() string
}

// ItemFn is a public function. When Extern is true, the rendered Rust
// uses `pub extern "C" fn` and the C header gets a matching
// declaration. The Body is the already-rendered Rust function body
// (callers own the lowering of Mochi expressions to Rust); Body must
// not contain the surrounding braces.
type ItemFn struct {
	Name   string
	Params []Param
	Return string
	Body   string
	Extern bool
	Doc    string
}

func (ItemFn) itemKind() string { return "fn" }
func (f ItemFn) name() string   { return f.Name }

// Param is one function parameter. Type is a literal Rust type
// fragment (e.g. "i64", "&str", "*const c_char").
type Param struct {
	Name string
	Type string
}

// ItemStruct is a public struct. When ReprC is true, the rendered
// struct gets `#[repr(C)]` and the C header gets a matching `typedef
// struct` declaration. Derives is the list of trait paths to expand
// into a `#[derive(...)]` attribute.
type ItemStruct struct {
	Name    string
	Fields  []Field
	ReprC   bool
	Derives []string
	Doc     string
}

func (ItemStruct) itemKind() string { return "struct" }
func (s ItemStruct) name() string   { return s.Name }

// Field is one struct or enum-variant field.
type Field struct {
	Name string
	Type string
	Pub  bool
	Doc  string
}

// ItemEnum is a public enum. ReprC and Derives behave the same way
// as ItemStruct's. Variants are rendered in declaration order.
type ItemEnum struct {
	Name     string
	Variants []Variant
	ReprC    bool
	Derives  []string
	Doc      string
}

func (ItemEnum) itemKind() string { return "enum" }
func (e ItemEnum) name() string   { return e.Name }

// Variant is one enum variant. Fields are the variant's payload
// (empty means a unit variant).
type Variant struct {
	Name   string
	Fields []Field
	Doc    string
}

// RenderError describes a structural validation failure of a
// PublicAPI. It carries the offending item's identifier so callers
// can produce actionable diagnostics.
type RenderError struct {
	Reason string
	Item   string
}

// Error implements the error interface.
func (e RenderError) Error() string {
	if e.Item != "" {
		return fmt.Sprintf("library render: %s (item %q)", e.Reason, e.Item)
	}
	return fmt.Sprintf("library render: %s", e.Reason)
}

// Files is the rendered output of a PublicAPI: a map from relative
// path (forward-slash separated) to file contents. The map always
// contains "Cargo.toml" and "src/lib.rs" when validation passes; it
// additionally contains "include/<crate>.h" when CHeader is true.
type Files map[string]string

// Sorted returns the file paths in stable alphabetical order.
// Convenience for callers that want deterministic iteration.
func (f Files) Sorted() []string {
	out := make([]string, 0, len(f))
	for k := range f {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// Render lowers a PublicAPI into a set of files representing the
// published crate. The result is byte-stable: given the same input,
// Render returns byte-identical output.
//
// Render performs structural validation only (crate name shape,
// duplicate item names, body presence on non-Extern functions). It
// does NOT type-check Rust source; the rendered crate is verified
// downstream by `cargo build`.
func Render(api PublicAPI) (Files, error) {
	if err := validate(api); err != nil {
		return nil, err
	}
	out := Files{}
	out["Cargo.toml"] = RenderManifest(api)
	out["src/lib.rs"] = RenderLibRS(api)
	if api.CHeader {
		out["include/"+headerName(api.CrateName)+".h"] = RenderCHeader(api)
	}
	return out, nil
}

func validate(api PublicAPI) error {
	if !validCrateName(api.CrateName) {
		return RenderError{Reason: "invalid crate name"}
	}
	if strings.TrimSpace(api.Version) == "" {
		return RenderError{Reason: "missing crate version"}
	}
	seen := map[string]struct{}{}
	for _, it := range api.Items {
		if it == nil {
			return RenderError{Reason: "nil item"}
		}
		nm := it.name()
		if nm == "" {
			return RenderError{Reason: "item with empty name", Item: it.itemKind()}
		}
		if _, dup := seen[nm]; dup {
			return RenderError{Reason: "duplicate item name", Item: nm}
		}
		seen[nm] = struct{}{}
		if fn, ok := it.(ItemFn); ok {
			if !fn.Extern && strings.TrimSpace(fn.Body) == "" {
				return RenderError{Reason: "function missing body", Item: nm}
			}
		}
	}
	return nil
}

// validCrateName enforces the crates.io naming rules: a non-empty
// ascii string starting with a letter, followed by letters, digits,
// underscores, or hyphens. The check is strict enough to catch the
// common errors (leading digits, dots, slashes); cargo itself applies
// the same rule.
func validCrateName(name string) bool {
	if name == "" {
		return false
	}
	for i, c := range name {
		switch {
		case c >= 'a' && c <= 'z':
		case c >= 'A' && c <= 'Z':
		case (c >= '0' && c <= '9') && i > 0:
		case (c == '_' || c == '-') && i > 0:
		default:
			return false
		}
	}
	return true
}

// headerName turns a crate name into the C header basename. Hyphens
// are replaced with underscores so the include works on platforms
// that disallow hyphens in identifiers.
func headerName(crate string) string {
	return strings.ReplaceAll(crate, "-", "_")
}
