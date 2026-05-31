// Package errors carries the cross-cutting error types the MEP-68 bridge
// emits at lock time and at build time.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a .NET item.
type SkipReason int

const (
	SkipUnknown    SkipReason = iota // zero value; must never be emitted
	SkipGeneric                      // unbound generic type parameter
	SkipPointer                      // unsafe pointer type
	SkipRefType                      // ref/out/in parameter
	SkipInterface                    // interface type not yet bridged
	SkipDelegate                     // delegate, Action<T>, or Func<T>
	SkipDynamic                      // dynamic type; no static Mochi analogue
	SkipCOMInterop                   // COM interop type; Windows-only
	SkipUnsafe                       // unsafe method or type
	SkipInternal                     // internal visibility; not exported
	SkipAbstract                     // abstract class with external subtypes
	SkipObsolete                     // marked [Obsolete]
	SkipNestedType                   // nested type; not representable at top level
	SkipIndexer                      // indexer property
	SkipOperator                     // operator overload
	SkipEvent                        // event declaration
	SkipMultiReturn                  // multiple return values (out params)
	SkipSpan                         // Span<T> / ReadOnlySpan<T>; stack-only
	SkipValueTask                    // ValueTask<T>; use Task<T> instead
	SkipObjectReturn                 // return type is System.Object (untyped)
	SkipAsyncEnumerable              // IAsyncEnumerable<T>; deferred to later phase
	SkipNativeAOTIncompatible        // [RequiresDynamicCode]; requires CoreCLR
)

func (r SkipReason) String() string {
	switch r {
	case SkipUnknown:
		return "SkipUnknown"
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
	case SkipObjectReturn:
		return "SkipObjectReturn"
	case SkipAsyncEnumerable:
		return "SkipAsyncEnumerable"
	case SkipNativeAOTIncompatible:
		return "SkipNativeAOTIncompatible"
	default:
		return fmt.Sprintf("SkipReason(%d)", int(r))
	}
}

// SkipReport records a single item the bridge declined to translate.
type SkipReport struct {
	ItemPath string // e.g. "System.Console.WriteLine"
	Reason   SkipReason
	Detail   string
	Override string // non-empty when the user supplied an allow-list entry
}

func (r SkipReport) String() string {
	s := fmt.Sprintf("SKIPPED: %s\n  Reason: %s", r.ItemPath, r.Reason)
	if r.Detail != "" {
		s += "\n  Detail: " + r.Detail
	}
	if r.Override != "" {
		s += "\n  Override: " + r.Override
	}
	return s + "\n"
}

// BridgeError wraps a lower-level error with the phase and optional package context.
type BridgeError struct {
	Phase   string
	Package string // NuGet package ID; may be empty for non-package phases
	Cause   error
}

func (e *BridgeError) Error() string {
	if e.Package != "" {
		return fmt.Sprintf("%s[%s]: %v", e.Phase, e.Package, e.Cause)
	}
	return fmt.Sprintf("%s: %v", e.Phase, e.Cause)
}

func (e *BridgeError) Unwrap() error { return e.Cause }

// Wrap wraps cause with phase/package context. Returns nil if cause is nil.
func Wrap(phase, pkg string, cause error) error {
	if cause == nil {
		return nil
	}
	return &BridgeError{Phase: phase, Package: pkg, Cause: cause}
}
