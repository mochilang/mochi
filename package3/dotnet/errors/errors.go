// Package errors carries the cross-cutting error types the MEP-68 bridge
// emits at lock time and at build time. The most important one is SkipReason,
// which records why a particular CLR type or member was not translated into a
// Mochi extern binding. See [website/docs/research/0068/] for the closed set
// of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a CLR type or
// member. The set mirrors the table in the MEP-68 research notes.
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipGeneric: the type has an open generic parameter and no
	// monomorphise entry has been provided.
	SkipGeneric
	// SkipPointer: unsafe pointer type (System.IntPtr in direct-pointer context).
	SkipPointer
	// SkipRefType: ref/out parameter requires special marshalling that
	// v1 does not support.
	SkipRefType
	// SkipInterface: interface in non-handle position (no concrete factory).
	SkipInterface
	// SkipDelegate: System.Delegate / Action<T> / Func<T> (function pointer).
	SkipDelegate
	// SkipDynamic: dynamic type (runtime binding, no static surface).
	SkipDynamic
	// SkipCOMInterop: [ComImport] type.
	SkipCOMInterop
	// SkipUnsafe: unsafe code block without capabilities opt-in.
	SkipUnsafe
	// SkipInternal: internal visibility (not in the public API surface).
	SkipInternal
	// SkipAbstract: abstract class with no concrete factory.
	SkipAbstract
	// SkipObsolete: [Obsolete] marked item.
	SkipObsolete
	// SkipNestedType: nested type definition (not supported in v1).
	SkipNestedType
	// SkipIndexer: indexer property (this[T]).
	SkipIndexer
	// SkipOperator: operator overload.
	SkipOperator
	// SkipEvent: event (add/remove pattern).
	SkipEvent
	// SkipMultiReturn: out/ref params as multiple return values.
	SkipMultiReturn
	// SkipSpan: Span<T> / ReadOnlySpan<T> (stack-only types).
	SkipSpan
	// SkipValueTask: ValueTask<T> (v1 only bridges Task<T>).
	SkipValueTask
)

// String renders the SkipReason as a short token used in the SKIPPED.txt
// output file. The token is stable across releases; do not rename without
// adjusting the SKIPPED.txt golden fixtures.
func (r SkipReason) String() string {
	switch r {
	case SkipGeneric:
		return "SkipGeneric"
	case SkipPointer:
		return "SkipPointer"
	case SkipRefType:
		return "SkipRefType"
	case SkipInterface:
		return "SkipInterface"
	case SkipDelegate:
		return "SkipDelegate"
	case SkipDynamic:
		return "SkipDynamic"
	case SkipCOMInterop:
		return "SkipCOMInterop"
	case SkipUnsafe:
		return "SkipUnsafe"
	case SkipInternal:
		return "SkipInternal"
	case SkipAbstract:
		return "SkipAbstract"
	case SkipObsolete:
		return "SkipObsolete"
	case SkipNestedType:
		return "SkipNestedType"
	case SkipIndexer:
		return "SkipIndexer"
	case SkipOperator:
		return "SkipOperator"
	case SkipEvent:
		return "SkipEvent"
	case SkipMultiReturn:
		return "SkipMultiReturn"
	case SkipSpan:
		return "SkipSpan"
	case SkipValueTask:
		return "SkipValueTask"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single CLR type or member the bridge declined to
// translate. The collection of SkipReports for an assembly is rendered to
// SKIPPED.txt under the wrapper directory at the end of phase 4.
type SkipReport struct {
	// ItemPath is the CLR path of the item, e.g. "Newtonsoft.Json.JsonConvert.SerializeObject".
	ItemPath string
	// Reason is the classification.
	Reason SkipReason
	// Detail is a free-text explanation specific to this skip.
	Detail string
	// Override is the suggested hand-authored opt-in. May be empty if there
	// is no straightforward override available.
	Override string
}

// String renders a SkipReport in the SKIPPED.txt format.
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
	// Package is the upstream NuGet package name being processed when the
	// error occurred. Empty for phase-agnostic errors.
	Package string
	// Cause is the underlying error.
	Cause error
}

// Error renders BridgeError as "phase[package]: cause".
func (e *BridgeError) Error() string {
	if e.Package == "" {
		return fmt.Sprintf("%s: %v", e.Phase, e.Cause)
	}
	return fmt.Sprintf("%s[%s]: %v", e.Phase, e.Package, e.Cause)
}

// Unwrap exposes the underlying cause for errors.Is / errors.As.
func (e *BridgeError) Unwrap() error { return e.Cause }

// Wrap constructs a BridgeError from a phase, a package (optional), and a cause.
func Wrap(phase, pkg string, cause error) error {
	if cause == nil {
		return nil
	}
	return &BridgeError{Phase: phase, Package: pkg, Cause: cause}
}
