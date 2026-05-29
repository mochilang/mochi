// Package apisurface is the MEP-74 schema for the JSON document
// produced by the `go-ingest` helper and consumed by every later
// phase of the bridge. The schema captures the exported surface of a
// Go module — package list, exported funcs / types / consts / vars,
// methods, fields, and a SkipReport-style record of anything the
// ingester deliberately omitted.
//
// The schema version is locked in code: ingester and parser must
// agree on it. Backwards-incompatible changes bump SchemaVersion;
// additive changes (new optional fields) leave it untouched.
package apisurface

import (
	"encoding/json"
	"errors"
	"fmt"
)

// SchemaVersion is the wire-format version. Bumped on any
// backwards-incompatible change; both producer (cmd/go-ingest) and
// consumer (bridge build driver) pin it.
const SchemaVersion = 1

// File is the top-level JSON document. One file per module@version.
type File struct {
	// SchemaVersion is the literal SchemaVersion constant. Parsers
	// reject documents with a mismatched value.
	SchemaVersion int `json:"schema_version"`
	// Module is the resolved module path (e.g. "github.com/spf13/cobra").
	Module string `json:"module"`
	// Version is the resolved semver (e.g. "v1.0.0"). May be empty
	// for source-tree ingests where no version is known.
	Version string `json:"version,omitempty"`
	// GoVersion is the toolchain version the ingester ran under
	// (e.g. "go1.21.5"). Useful when reproducing the surface.
	GoVersion string `json:"go_version,omitempty"`
	// GeneratedBy is a free-form identifier for the producing tool.
	GeneratedBy string `json:"generated_by,omitempty"`
	// Packages is the list of public packages in the module.
	Packages []Package `json:"packages"`
}

// Package is one importable Go package.
type Package struct {
	// ImportPath is the canonical import path, e.g. "example.com/foo/sub".
	ImportPath string `json:"import_path"`
	// Name is the package clause name (often the last path element,
	// but may differ — e.g. "yaml" in gopkg.in/yaml.v3).
	Name string `json:"name"`
	// Doc is the godoc string for the package, joined with newlines.
	Doc string `json:"doc,omitempty"`
	// IsMain reports whether the package's name is "main". main
	// packages are kept in the surface so callers can detect that a
	// module has no library API.
	IsMain bool `json:"is_main,omitempty"`
	// Imports is the de-duplicated list of import paths referenced by
	// the package's exported surface. Sorted lexicographically.
	Imports []string `json:"imports,omitempty"`
	// Funcs lists exported top-level functions (not methods; methods
	// live on Type).
	Funcs []Func `json:"funcs,omitempty"`
	// Types lists exported named types in declaration order.
	Types []Type `json:"types,omitempty"`
	// Consts lists exported constants (named or anonymous, but with
	// an exported identifier).
	Consts []Value `json:"consts,omitempty"`
	// Vars lists exported package-level variables.
	Vars []Value `json:"vars,omitempty"`
	// Skipped collects everything the ingester deliberately omitted
	// from the surface, with a stable reason string.
	Skipped []SkipNote `json:"skipped,omitempty"`
}

// Func is an exported function or method.
type Func struct {
	// Name is the function identifier (no package prefix).
	Name string `json:"name"`
	// Doc is the godoc string.
	Doc string `json:"doc,omitempty"`
	// Position is "file:line:col" relative to the package directory.
	Position string `json:"position,omitempty"`
	// TypeParams lists generic type parameters, if any.
	TypeParams []TypeParam `json:"type_params,omitempty"`
	// Receiver is the method receiver type (named, no pointer
	// prefix). Empty for top-level functions.
	Receiver string `json:"receiver,omitempty"`
	// ReceiverPointer reports whether the receiver is a pointer (i.e.
	// "func (*Foo) M()" vs "func (Foo) M()"). Always false for
	// non-method funcs.
	ReceiverPointer bool `json:"receiver_pointer,omitempty"`
	// Params lists function parameters in declaration order.
	Params []Param `json:"params,omitempty"`
	// Results lists return values in declaration order. Empty
	// when the function has no return.
	Results []Param `json:"results,omitempty"`
	// Variadic reports whether the last parameter is variadic
	// (uses `...T`).
	Variadic bool `json:"variadic,omitempty"`
}

// Param is a function parameter or result. The Name is empty for
// anonymous results (e.g. `func() error`).
type Param struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type"`
}

// TypeParam is a generic type parameter.
type TypeParam struct {
	Name       string `json:"name"`
	Constraint string `json:"constraint,omitempty"`
}

// TypeKind names the high-level shape of a named type.
type TypeKind string

const (
	KindStruct    TypeKind = "struct"
	KindInterface TypeKind = "interface"
	KindAlias     TypeKind = "alias"
	KindBasic     TypeKind = "basic"
	KindSlice     TypeKind = "slice"
	KindMap       TypeKind = "map"
	KindChan      TypeKind = "chan"
	KindFunc      TypeKind = "func"
	KindArray     TypeKind = "array"
	KindPointer   TypeKind = "pointer"
	KindNamed     TypeKind = "named"
)

// Type is a named exported type declaration.
type Type struct {
	// Name is the declared identifier (e.g. "Reader").
	Name string `json:"name"`
	// Kind is the high-level shape (struct, interface, alias, etc.).
	Kind TypeKind `json:"kind"`
	// Doc is the godoc string.
	Doc string `json:"doc,omitempty"`
	// Position is "file:line:col" relative to the package directory.
	Position string `json:"position,omitempty"`
	// TypeParams lists generic type parameters, if any.
	TypeParams []TypeParam `json:"type_params,omitempty"`
	// Underlying is the string form of the underlying type (e.g.
	// "struct { A int; B string }"). May be elided for opaque named
	// types.
	Underlying string `json:"underlying,omitempty"`
	// AliasOf, when Kind == KindAlias, is the right-hand side of the
	// type alias declaration.
	AliasOf string `json:"alias_of,omitempty"`
	// Fields lists struct fields (Kind == KindStruct).
	Fields []Field `json:"fields,omitempty"`
	// Methods lists declared methods on the type (declaration order;
	// independent of receiver-pointer-ness).
	Methods []Func `json:"methods,omitempty"`
	// InterfaceMethods lists method *signatures* an interface type
	// requires. Kind == KindInterface only.
	InterfaceMethods []Func `json:"interface_methods,omitempty"`
	// EmbeddedTypes lists embedded named types (for struct or
	// interface).
	EmbeddedTypes []string `json:"embedded_types,omitempty"`
}

// Field is a struct field.
type Field struct {
	Name     string `json:"name"`
	Type     string `json:"type"`
	Tag      string `json:"tag,omitempty"`
	Doc      string `json:"doc,omitempty"`
	Exported bool   `json:"exported"`
	// Embedded reports whether the field is an embedded type (no
	// explicit name).
	Embedded bool `json:"embedded,omitempty"`
}

// Value is a top-level const or var.
type Value struct {
	Name     string `json:"name"`
	Type     string `json:"type,omitempty"`
	Doc      string `json:"doc,omitempty"`
	Position string `json:"position,omitempty"`
	// Const is the textual form of the constant value (e.g. "42",
	// `"hello"`). Empty for vars.
	Const string `json:"const,omitempty"`
}

// SkipNote records one item the ingester deliberately omitted from
// the surface. Reason values are stable strings — phase 4 (parser)
// matches them via a closed switch.
type SkipNote struct {
	// Kind is one of "func", "type", "const", "var", "method",
	// "field".
	Kind string `json:"kind"`
	// Name is the declared identifier (or "" for anonymous items).
	Name string `json:"name"`
	// Position is "file:line:col" if known.
	Position string `json:"position,omitempty"`
	// Reason is a stable token. See package3/go/errors.SkipReason
	// for the canonical list.
	Reason string `json:"reason"`
}

// Encode serialises f as canonical JSON with 2-space indent and a
// trailing newline. The output is byte-stable across runs because
// (a) every list field is sorted by the ingester and (b)
// encoding/json honours field ordering by declaration.
func (f *File) Encode() ([]byte, error) {
	if f == nil {
		return nil, errors.New("apisurface: nil file")
	}
	buf, err := json.MarshalIndent(f, "", "  ")
	if err != nil {
		return nil, fmt.Errorf("apisurface: marshal: %w", err)
	}
	return append(buf, '\n'), nil
}

// Decode parses an ApiSurface JSON document and validates the schema
// version. Returns ErrSchemaVersion when the document was emitted by
// a producer the current build does not know how to consume.
func Decode(data []byte) (*File, error) {
	var f File
	if err := json.Unmarshal(data, &f); err != nil {
		return nil, fmt.Errorf("apisurface: unmarshal: %w", err)
	}
	if f.SchemaVersion != SchemaVersion {
		return nil, fmt.Errorf("%w: got %d, want %d", ErrSchemaVersion, f.SchemaVersion, SchemaVersion)
	}
	return &f, nil
}

// ErrSchemaVersion is returned by Decode when the document's
// schema_version field does not match the SchemaVersion constant the
// bridge was built against.
var ErrSchemaVersion = errors.New("apisurface: incompatible schema_version")
