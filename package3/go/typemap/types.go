// Package typemap is the MEP-74 closed type-mapping table: it maps
// every shape of Go type (as recovered by package apisurface) to a
// MochiType representation plus a TransferDirection that drives the
// cgo wrapper synthesiser (phase 6) and the extern fn emitter
// (phase 7).
//
// The grammar of MochiType is closed: phase 6 and phase 7 switch
// over the concrete variants exhaustively. Adding a new MochiType
// variant requires updating every consumer in lock-step.
package typemap

import (
	"sort"
	"strings"
)

// MochiType is the Mochi-side representation of a bridged Go type.
type MochiType interface {
	isMochiType()
	// String renders the type in Mochi source syntax.
	String() string
}

// ScalarType is a Mochi scalar: int, float, bool, string, bytes,
// error, any. Mapped from Go basic types and (where applicable)
// from named types whose underlying is a basic type.
type ScalarType struct {
	// Name is one of "int", "float", "bool", "string", "bytes",
	// "error", "any".
	Name string
}

// ListType is Mochi list<T>. Mapped from Go []T (or []byte ->
// ScalarType bytes; that's handled at the mapper layer).
type ListType struct {
	Elem MochiType
}

// MochiMap is Mochi map<K, V>. Mapped from Go map[K]V.
type MochiMap struct {
	Key, Value MochiType
}

// RecordType is a Mochi record with named fields. Mapped from Go
// structs whose fields are themselves bridgeable as Copy or View.
type RecordType struct {
	// Name is the canonical Mochi type name. For named Go structs,
	// derived from the original Go name (no package prefix); for
	// anonymous struct literals, "anon".
	Name   string
	Fields []RecordField
}

// RecordField is one Mochi record field.
type RecordField struct {
	Name string
	Type MochiType
}

// HandleType is a Mochi handle<TName> -- an opaque pointer kept in
// the cgo handle pool. Mapped from Go interfaces, channels, funcs,
// and structs that cannot be copied (private fields, embedded
// unexported types, ...).
type HandleType struct {
	// Name is the Mochi-visible identifier of the handle type. It
	// disambiguates between handles to different Go types (e.g.
	// handle<io.Reader> vs. handle<*os.File>).
	Name string
}

// FuncType is a Mochi closure type (Params) -> Results.
type FuncType struct {
	Params  []MochiType
	Results []MochiType
}

// OptionType is Mochi option<T>. Mapped from Go pointer types when
// the pointee is bridgeable as a value (slices, maps, records).
type OptionType struct {
	Elem MochiType
}

// AnyType is Mochi any. Used as a fallback when the Go type is
// itself any/interface{} with no methods.
type AnyType struct{}

func (ScalarType) isMochiType() {}
func (ListType) isMochiType()   {}
func (MochiMap) isMochiType()   {}
func (RecordType) isMochiType() {}
func (HandleType) isMochiType() {}
func (FuncType) isMochiType()   {}
func (OptionType) isMochiType() {}
func (AnyType) isMochiType()    {}

func (t ScalarType) String() string { return t.Name }
func (t ListType) String() string   { return "list<" + t.Elem.String() + ">" }
func (t MochiMap) String() string   { return "map<" + t.Key.String() + ", " + t.Value.String() + ">" }

func (t RecordType) String() string {
	if t.Name != "" {
		return t.Name
	}
	var sb strings.Builder
	sb.WriteString("record{")
	// Render fields sorted by name so the canonical form is stable.
	names := make([]string, 0, len(t.Fields))
	idx := make(map[string]int, len(t.Fields))
	for i, f := range t.Fields {
		names = append(names, f.Name)
		idx[f.Name] = i
	}
	sort.Strings(names)
	for i, n := range names {
		if i > 0 {
			sb.WriteString("; ")
		}
		sb.WriteString(n)
		sb.WriteString(": ")
		sb.WriteString(t.Fields[idx[n]].Type.String())
	}
	sb.WriteString("}")
	return sb.String()
}

func (t HandleType) String() string {
	if t.Name == "" {
		return "handle"
	}
	return "handle<" + t.Name + ">"
}

func (t FuncType) String() string {
	var sb strings.Builder
	sb.WriteString("fn(")
	for i, p := range t.Params {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(p.String())
	}
	sb.WriteString(") -> ")
	switch len(t.Results) {
	case 0:
		sb.WriteString("unit")
	case 1:
		sb.WriteString(t.Results[0].String())
	default:
		sb.WriteByte('(')
		for i, r := range t.Results {
			if i > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(r.String())
		}
		sb.WriteByte(')')
	}
	return sb.String()
}

func (t OptionType) String() string { return "option<" + t.Elem.String() + ">" }
func (AnyType) String() string      { return "any" }

// TransferDirection captures how a value crosses the Go <-> Mochi
// boundary at runtime. Phase 6 (wrapper synthesiser) uses this to
// decide whether to memcpy, materialise a Mochi-side view, or stash
// the value in the cgo handle pool.
type TransferDirection int

const (
	// Copy means the value is byte-copied across the boundary in a
	// single direction. Scalars, fixed-size structs of scalars, and
	// other plain-old-data types use this.
	Copy TransferDirection = iota
	// View means the Mochi side holds a borrowed pointer or slice
	// header into the original Go memory. The borrow is valid until
	// the wrapper returns; phase 6 enforces this via the cgo
	// `noescape` discipline.
	View
	// Handle means the value is registered in the cgo handle pool;
	// the Mochi side holds an opaque integer key. The Go side
	// resolves the key on every call back.
	Handle
)

func (d TransferDirection) String() string {
	switch d {
	case Copy:
		return "copy"
	case View:
		return "view"
	case Handle:
		return "handle"
	}
	return "unknown"
}

// Mapping is the result of mapping one Go type to a Mochi type. It
// pairs the MochiType with the TransferDirection and, optionally, a
// list of human-readable Notes that document the lowering decision
// (used by phase 7's diagnostic output).
type Mapping struct {
	Mochi     MochiType
	Direction TransferDirection
	Notes     []string
}
