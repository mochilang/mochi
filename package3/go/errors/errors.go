// Package errors carries the cross-cutting error types the MEP-74 Go bridge
// emits at lock time and at build time. The most important one is SkipReport,
// which records why a particular go/packages item was not translated into a
// Mochi extern fn binding. See [website/docs/research/0074/05-type-mapping.md]
// for the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a go/packages
// item. The set mirrors the table in research note 05 §"Refusal cases".
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipUnexportedInPosition: an exported function returns or accepts an
	// unexported type, which the Mochi side cannot construct or inspect.
	SkipUnexportedInPosition
	// SkipInternalPath: the item lives under a `<module>/internal/` package.
	// Go's own visibility rules forbid external imports.
	SkipInternalPath
	// SkipGenericWithoutMonomorphise: a generic item with at least one
	// non-instantiated type parameter and no matching entry in
	// `[go.monomorphise]`.
	SkipGenericWithoutMonomorphise
	// SkipUnsafePointer: the item's signature mentions `unsafe.Pointer`,
	// which the bridge does not translate.
	SkipUnsafePointer
	// SkipReflectValue: the item's signature mentions `reflect.Value` or
	// `reflect.Type`.
	SkipReflectValue
	// SkipCgoHandle: the item's signature mentions `runtime/cgo.Handle`
	// directly, bypassing the bridge's own handle pool.
	SkipCgoHandle
	// SkipInterfaceComplexMethod: an interface method whose signature falls
	// outside the closed type-mapping table.
	SkipInterfaceComplexMethod
	// SkipChanOfUnexportedStruct: a channel whose element type is a struct
	// projection that itself fails the visibility rules.
	SkipChanOfUnexportedStruct
	// SkipMapKeyNotBasic: a `map[K]V` where K is a struct or interface type
	// (Mochi maps key only on string or integer).
	SkipMapKeyNotBasic
	// SkipRequiresCgoCapability: the module declares `import "C"` and the
	// user has not opted in via `[go.capabilities] cgo = true`.
	SkipRequiresCgoCapability
	// SkipBuildTagExcluded: the item is behind a `//go:build` tag that is
	// not in `[go.build-tags]`.
	SkipBuildTagExcluded
	// SkipComplexNumeric: complex64 or complex128 in the signature; Mochi
	// has no complex type.
	SkipComplexNumeric
	// SkipPointerToPointer: `**T`-style double pointers; Mochi has no
	// pointer arithmetic.
	SkipPointerToPointer
	// SkipFuncReturningFunc: a function whose signature returns another
	// function value (the bridge needs the user to flatten the closure).
	SkipFuncReturningFunc
	// SkipVariadicComplex: a variadic with a non-basic element type that
	// the bridge cannot reconstruct on the Go side.
	SkipVariadicComplex
	// SkipConstantItem: a Go-source `const` item. The bridge does not bind
	// consts in v1; the user re-declares them in Mochi.
	SkipConstantItem
	// SkipVarItem: a Go-source `var` item at package scope. The bridge does
	// not bind mutable globals.
	SkipVarItem
	// SkipDeprecated: the item carries a `// Deprecated:` doc-comment line;
	// the bridge defaults to skipping. The user can override via
	// `[go.allow-deprecated]`.
	SkipDeprecated
	// SkipNonPublishedReplace: an item resolves through a replace directive
	// to a local path; only valid in development, not for publishing.
	SkipNonPublishedReplace
)

// String renders the SkipReason as a short token used in the SKIPPED.txt
// output file. The token is stable across releases; do not rename without
// adjusting the SKIPPED.txt golden fixtures.
func (r SkipReason) String() string {
	switch r {
	case SkipUnexportedInPosition:
		return "SkipUnexportedInPosition"
	case SkipInternalPath:
		return "SkipInternalPath"
	case SkipGenericWithoutMonomorphise:
		return "SkipGenericWithoutMonomorphise"
	case SkipUnsafePointer:
		return "SkipUnsafePointer"
	case SkipReflectValue:
		return "SkipReflectValue"
	case SkipCgoHandle:
		return "SkipCgoHandle"
	case SkipInterfaceComplexMethod:
		return "SkipInterfaceComplexMethod"
	case SkipChanOfUnexportedStruct:
		return "SkipChanOfUnexportedStruct"
	case SkipMapKeyNotBasic:
		return "SkipMapKeyNotBasic"
	case SkipRequiresCgoCapability:
		return "SkipRequiresCgoCapability"
	case SkipBuildTagExcluded:
		return "SkipBuildTagExcluded"
	case SkipComplexNumeric:
		return "SkipComplexNumeric"
	case SkipPointerToPointer:
		return "SkipPointerToPointer"
	case SkipFuncReturningFunc:
		return "SkipFuncReturningFunc"
	case SkipVariadicComplex:
		return "SkipVariadicComplex"
	case SkipConstantItem:
		return "SkipConstantItem"
	case SkipVarItem:
		return "SkipVarItem"
	case SkipDeprecated:
		return "SkipDeprecated"
	case SkipNonPublishedReplace:
		return "SkipNonPublishedReplace"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single go/packages item the bridge declined to
// translate. The collection of SkipReports for a module is rendered to
// SKIPPED.txt under the wrapper module directory at the end of phase 4.
type SkipReport struct {
	// ItemPath is the qualified Go item name, e.g.
	// "github.com/spf13/cobra.Command.SetFlagErrorFunc".
	ItemPath string
	// Reason is the classification.
	Reason SkipReason
	// Detail is a free-text explanation specific to this skip.
	Detail string
	// Override is the suggested hand-authored opt-in. May be empty if there
	// is no straightforward override available.
	Override string
}

// String renders a SkipReport in the SKIPPED.txt format documented in
// research note 05.
func (s SkipReport) String() string {
	out := fmt.Sprintf("SKIPPED: %s\n  Reason: %s\n  Detail: %s\n", s.ItemPath, s.Reason, s.Detail)
	if s.Override != "" {
		out += fmt.Sprintf("  Override: %s\n", s.Override)
	}
	return out
}

// BridgeError is the top-level error returned by Driver entry points. It
// records the phase that produced the error and the underlying cause.
type BridgeError struct {
	// Phase is the bridge phase that detected the error, e.g. "lock",
	// "ingest", "wrapper", "build".
	Phase string
	// Module is the upstream Go module path being processed when the error
	// occurred. Empty for phase-agnostic errors.
	Module string
	// Cause is the underlying error.
	Cause error
}

// Error renders BridgeError as "phase[module]: cause".
func (e *BridgeError) Error() string {
	if e.Module == "" {
		return fmt.Sprintf("%s: %v", e.Phase, e.Cause)
	}
	return fmt.Sprintf("%s[%s]: %v", e.Phase, e.Module, e.Cause)
}

// Unwrap exposes the underlying cause for errors.Is / errors.As.
func (e *BridgeError) Unwrap() error { return e.Cause }

// Wrap constructs a BridgeError from a phase, a module (optional), and a
// cause. Returns nil if cause is nil so callers can do `return Wrap(...)`
// safely from a happy path.
func Wrap(phase, module string, cause error) error {
	if cause == nil {
		return nil
	}
	return &BridgeError{Phase: phase, Module: module, Cause: cause}
}
