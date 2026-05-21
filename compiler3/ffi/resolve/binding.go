package resolve

import (
	"mochi/compiler3/ffi/typebridge"
)

// PackageBinding is the result of resolving one Go import path. It
// carries every exported symbol of the package as a typed Mochi
// binding plus enough provenance to invalidate the cache cleanly.
type PackageBinding struct {
	// ImportPath is the canonical Go import path (e.g. "encoding/json").
	ImportPath string
	// Name is the Go package's declared name (e.g. "json").
	Name string
	// GoSumHash is the SHA-256 (hex, 64 chars) of the user's go.sum
	// file at resolve time. Empty if the package was resolved outside
	// a module (stdlib walk during testing).
	GoSumHash string
	// MochiVersion is the resolver's binary version at resolve time.
	// A bump on the consumer side invalidates the cache entry.
	MochiVersion string

	Funcs  []FuncBinding
	Types  []TypeBinding
	Vars   []VarBinding
	Consts []ConstBinding

	// Errors carries non-fatal resolution issues (a symbol that mapped
	// to KindOpaque without GoType, an unresolvable constraint). The
	// resolver returns a PackageBinding plus errors; the consumer
	// decides whether to fail the build.
	Errors []BindingError
}

// FuncBinding is one Go function or method.
type FuncBinding struct {
	// Name is the exported symbol name (the receiver type is encoded
	// in Recv).
	Name string
	// Signature is the function type as produced by the bridge. For a
	// method, the receiver is NOT in Signature.Params; Recv carries it.
	Signature typebridge.Type
	// GoCallExpr is the source-level Go expression used at the call
	// site: "strings.ToUpper", "json.NewDecoder", or for a method
	// "(*os.File).Close" or "time.Time.Add" depending on receiver kind.
	GoCallExpr string
	// Recv is the receiver type for methods; nil for top-level funcs.
	Recv *typebridge.Type
	// TypeParams carries free type parameters for generic functions.
	// Each entry has the param name and a constraint summary.
	TypeParams []TypeParamBinding
	// Exported is always true (the resolver filters non-exported), but
	// kept for symmetry with the bridge's Field/Method shape.
	Exported bool
}

// TypeBinding is one named Go type (struct/interface/alias-of-named).
type TypeBinding struct {
	Name       string
	PkgPath    string
	Underlying typebridge.Type
	Methods    []FuncBinding
	TypeParams []TypeParamBinding
	// IsInterface is true when the underlying is a Go interface; the
	// type checker may dispatch interface satisfaction queries on it.
	IsInterface bool
}

// VarBinding is one package-level Go variable.
type VarBinding struct {
	Name   string
	Type   typebridge.Type
	GoExpr string
}

// ConstBinding is one package-level Go constant.
type ConstBinding struct {
	Name   string
	Type   typebridge.Type
	// Value is the Go-source literal form ("42", "\"hi\"", "1.5e-3").
	Value  string
	GoExpr string
}

// TypeParamBinding describes one free type parameter of a generic Go
// declaration. The Constraint is the bridge's structural rendering of
// the constraint interface; the Kind enum captures the well-known
// constraints (Ordered, Comparable, etc.) so the type checker can
// dispatch on them without re-parsing the structural form.
type TypeParamBinding struct {
	Name       string
	Constraint typebridge.Type
	Kind       typebridge.Constraint
	// KindKnown is true when Kind matches a well-known constraint
	// from the bridge's table; false means the constraint is custom
	// and the type checker must consult Constraint structurally.
	KindKnown bool
}

// BindingError is a structured warning attached to a PackageBinding.
type BindingError struct {
	Symbol string
	Reason ErrorReason
	Detail string
}

// ErrorReason enumerates the non-fatal categories the resolver may
// emit. Fatal errors (the package failed to load, the file system
// errored) return as a Go error from Resolve, not in this list.
type ErrorReason uint8

const (
	ErrNone ErrorReason = iota
	// ErrOpaqueOnly: the symbol's type bridge result is KindOpaque
	// with no further structural shape Mochi can dispatch on.
	ErrOpaqueOnly
	// ErrGenericNotInstantiated: the symbol is generic and no concrete
	// instantiation was supplied at resolve time. The binding still
	// records the free type parameters.
	ErrGenericNotInstantiated
	// ErrUnresolvedSymbol: the symbol was found in the package scope
	// but its type could not be resolved (e.g. a circular import the
	// loader returned only partially).
	ErrUnresolvedSymbol
	// ErrConstraintUnsupported: a generic constraint did not match the
	// bridge's well-known table and is not a plain interface.
	ErrConstraintUnsupported
)

// String renders an ErrorReason for diagnostic messages.
func (r ErrorReason) String() string {
	switch r {
	case ErrNone:
		return "none"
	case ErrOpaqueOnly:
		return "opaque-only"
	case ErrGenericNotInstantiated:
		return "generic-not-instantiated"
	case ErrUnresolvedSymbol:
		return "unresolved-symbol"
	case ErrConstraintUnsupported:
		return "constraint-unsupported"
	}
	return "?"
}
