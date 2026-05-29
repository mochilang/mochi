// Package rustdoc parses the rustdoc-json output produced by
// `cargo +nightly rustdoc --output-format=json` (or its alias
// `cargo +nightly doc --output-format=json`) and emits a normalised
// ApiSurface that downstream MEP-73 phases consume.
//
// The wire format is the rustdoc-types schema maintained at
// https://github.com/rust-lang/rust/tree/master/src/rustdoc-types and
// rendered as JSON-Schema at https://rust-lang.github.io/rustdoc-types/.
// As of May 2026 nightly the canonical format_version is 39; the
// parser refuses versions outside the supported range with a precise
// diagnostic so that a nightly drift never produces silently wrong
// output (MEP-73 §"Schema version compatibility").
//
// The package intentionally only models the variants the bridge can
// turn into a Mochi extern declaration. Everything else is converted
// into a SkipReport so the user can audit refusals and so the build
// remains stable when rust-lang/rust adds new variants.
package rustdoc

import (
	"encoding/json"
	"fmt"
)

// Document is the top-level rustdoc-json shape. The crate is the root
// module identified by Root; everything reachable from there appears in
// Index.
type Document struct {
	// Root is the rustdoc Id of the crate's root module.
	Root string `json:"root"`

	// CrateVersion is the Cargo.toml [package] version. May be empty if
	// the crate was rendered without one.
	CrateVersion string `json:"crate_version,omitempty"`

	// IncludesPrivate is true when rustdoc was invoked with
	// --document-private-items. The bridge only walks public items, so
	// the field is informational.
	IncludesPrivate bool `json:"includes_private"`

	// Index maps Id -> Item for every item in the document.
	Index map[string]Item `json:"index"`

	// Paths maps Id -> PathInfo for short path lookup. The walker
	// consults it to render module-qualified names without
	// reconstructing them from the Index.
	Paths map[string]PathInfo `json:"paths"`

	// ExternalCrates maps crate-id -> name for IDs that resolve to
	// items outside this crate (e.g. std::vec::Vec).
	ExternalCrates map[string]ExternalCrate `json:"external_crates,omitempty"`

	// FormatVersion is the rustdoc-types schema version. The parser
	// pins a supported range; see SupportedFormatVersions.
	FormatVersion int `json:"format_version"`

	// Target captures the host triple + target_features when present.
	Target *TargetInfo `json:"target,omitempty"`
}

// ExternalCrate is a crate referenced but not described by this document.
type ExternalCrate struct {
	Name string `json:"name"`
	HTMLRoot string `json:"html_root_url,omitempty"`
}

// PathInfo is the short Path record in Document.Paths. The Path slice
// holds the dotted segments leading to the item.
type PathInfo struct {
	CrateID int      `json:"crate_id"`
	Path    []string `json:"path"`
	Kind    string   `json:"kind"`
}

// TargetInfo is the optional `target` member added in format_version 35+.
// The bridge does not act on it yet, but parsing it keeps round-tripping
// honest.
type TargetInfo struct {
	Triple         string   `json:"triple,omitempty"`
	TargetFeatures []string `json:"target_features,omitempty"`
}

// Item is a single entry in Document.Index. Each Item carries common
// metadata (visibility, span, attrs, docs) and one variant in Inner.
type Item struct {
	ID            string          `json:"id"`
	CrateID       int             `json:"crate_id"`
	Name          string          `json:"name,omitempty"`
	Span          *Span           `json:"span,omitempty"`
	Visibility    Visibility      `json:"visibility"`
	Docs          string          `json:"docs,omitempty"`
	Links         map[string]string `json:"links,omitempty"`
	Attrs         []string        `json:"attrs,omitempty"`
	Deprecation   *Deprecation    `json:"deprecation,omitempty"`
	Inner         ItemEnum        `json:"inner"`
}

// Span is a source-position record. Used only for diagnostics.
type Span struct {
	Filename string `json:"filename"`
	Begin    [2]int `json:"begin"`
	End      [2]int `json:"end"`
}

// Deprecation captures the #[deprecated(...)] attribute payload.
type Deprecation struct {
	Since string `json:"since,omitempty"`
	Note  string `json:"note,omitempty"`
}

// Visibility is a sum of public / default (crate-private) / restricted.
// The wire format is either the literal string "public" or "default",
// or a tagged object {"restricted": {...}} / {"crate": "..."}.
type Visibility struct {
	Kind  VisibilityKind
	Path  string // for Restricted / Crate
}

// VisibilityKind enumerates the surface forms.
type VisibilityKind int

const (
	VisibilityPublic VisibilityKind = iota
	VisibilityDefault
	VisibilityCrate
	VisibilityRestricted
)

// String returns a stable token, useful for SkipReport rendering.
func (v VisibilityKind) String() string {
	switch v {
	case VisibilityPublic:
		return "public"
	case VisibilityDefault:
		return "default"
	case VisibilityCrate:
		return "crate"
	case VisibilityRestricted:
		return "restricted"
	}
	return "unknown"
}

// IsPublic reports whether the item is exported in the crate's public
// API surface. The bridge only walks public items.
func (v Visibility) IsPublic() bool { return v.Kind == VisibilityPublic }

// UnmarshalJSON handles both the bare string and the tagged-object form.
func (v *Visibility) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err == nil {
		switch s {
		case "public":
			v.Kind = VisibilityPublic
			return nil
		case "default":
			v.Kind = VisibilityDefault
			return nil
		}
		return fmt.Errorf("rustdoc: unknown visibility %q", s)
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(data, &obj); err != nil {
		return fmt.Errorf("rustdoc: visibility: %w", err)
	}
	if raw, ok := obj["restricted"]; ok {
		var r struct {
			ParentID string `json:"parent"`
			Path     string `json:"path"`
		}
		if err := json.Unmarshal(raw, &r); err != nil {
			return fmt.Errorf("rustdoc: visibility.restricted: %w", err)
		}
		v.Kind = VisibilityRestricted
		v.Path = r.Path
		return nil
	}
	if raw, ok := obj["crate"]; ok {
		var s string
		if err := json.Unmarshal(raw, &s); err == nil {
			v.Kind = VisibilityCrate
			v.Path = s
			return nil
		}
	}
	return fmt.Errorf("rustdoc: unknown visibility shape %s", string(data))
}
