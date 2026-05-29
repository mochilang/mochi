package rustdoc

import (
	"fmt"

	"mochi/package3/rust/errors"
)

// ApiSurface is the bridge-level distilled view of a rustdoc Document.
// It carries only what later phases need: a flat per-kind list of
// public items, each with a module-qualified path, and a SkipReport
// for every item that could not be lowered.
type ApiSurface struct {
	CrateName     string
	CrateVersion  string
	FormatVersion int

	Functions   []FunctionEntry
	Structs     []StructEntry
	Enums       []EnumEntry
	TypeAliases []TypeAliasEntry
	Constants   []ConstantEntry
	Traits      []TraitEntry

	Skipped []errors.SkipReport
}

// FunctionEntry is a public function lifted out of the document.
type FunctionEntry struct {
	ID       string
	Path     []string // module-qualified path, e.g. ["serde","de","from_str"]
	Inputs   []ParamEntry
	Output   *Type
	Header   FunctionHeader
	Generics Generics
}

// ParamEntry is one function parameter.
type ParamEntry struct {
	Name string
	Type Type
}

// StructEntry is a public struct.
type StructEntry struct {
	ID       string
	Path     []string
	Kind     string // "plain", "tuple", "unit"
	Fields   []FieldEntry
	Generics Generics
}

// FieldEntry is a struct or struct-variant field.
type FieldEntry struct {
	ID   string
	Name string
	Type Type
}

// EnumEntry is a public enum.
type EnumEntry struct {
	ID       string
	Path     []string
	Variants []VariantEntry
	Generics Generics
}

// VariantEntry is a public enum variant.
type VariantEntry struct {
	ID     string
	Name   string
	Kind   string // "plain", "tuple", "struct"
	Fields []FieldEntry
	Tuple  []Type
}

// TypeAliasEntry is a public `type X = Y;`.
type TypeAliasEntry struct {
	ID       string
	Path     []string
	Type     Type
	Generics Generics
}

// ConstantEntry is a public `const NAME: T = ...`.
type ConstantEntry struct {
	ID   string
	Path []string
	Type Type
}

// TraitEntry is a public trait. The bridge does not yet generate
// trait bindings (phase 4 covers concrete impls), but tracking traits
// keeps SkipReport granularity high.
type TraitEntry struct {
	ID         string
	Path       []string
	IsUnsafe   bool
	IsAuto     bool
	Generics   Generics
}

// Skip appends a SkipReport for a single item, attributing it to the
// item's ID + module-qualified path.
func (s *ApiSurface) skip(itemID string, path []string, reason errors.SkipReason, detail string) {
	name := joinPath(path)
	if name == "" {
		name = itemID
	}
	s.Skipped = append(s.Skipped, errors.SkipReport{
		ItemPath: name,
		Reason:   reason,
		Detail:   detail,
	})
}

// joinPath renders a path slice as a Rust-style `a::b::c` string.
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

// renderTypeKind formats a Type for SkipReport detail strings. Keeps
// the rendering deterministic for golden diffs.
func renderTypeKind(t Type) string {
	switch t.Kind() {
	case "resolved_path":
		if t.ResolvedPath != nil {
			return fmt.Sprintf("resolved_path(%s)", t.ResolvedPath.Path)
		}
	case "primitive":
		return fmt.Sprintf("primitive(%s)", t.Primitive)
	case "generic":
		return fmt.Sprintf("generic(%s)", t.Generic)
	}
	return t.Kind()
}
