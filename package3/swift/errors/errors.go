// Package errors carries the cross-cutting error types the MEP-69 Swift bridge
// emits at lock time and at build time. The most important one is SkipReport,
// which records why a particular .swiftinterface item was not translated into a
// Mochi extern fn binding.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a .swiftinterface
// item. The set mirrors the Swift-specific refusal cases.
type SkipReason int

const (
	// SkipUnknown is the zero value and must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipGeneric: the item uses unconstrained generic type parameters.
	SkipGeneric
	// SkipOpaque: the item uses opaque result types (some Protocol).
	SkipOpaque
	// SkipExistential: the item uses existential types (any Protocol).
	SkipExistential
	// SkipUnsafePointer: the item's signature mentions UnsafePointer or related.
	SkipUnsafePointer
	// SkipClosure: the item accepts or returns a closure / function type.
	SkipClosure
	// SkipActor: the item belongs to an actor or is actor-isolated.
	SkipActor
	// SkipMacro: the item is a Swift macro declaration.
	SkipMacro
	// SkipInternalAccess: the item has internal or private access level.
	SkipInternalAccess
	// SkipAbstract: the item is abstract (protocol requirement without default).
	SkipAbstract
	// SkipNSObject: the item inherits from NSObject or uses ObjC bridging.
	SkipNSObject
	// SkipConcurrencyType: the item uses structured concurrency types like
	// TaskGroup or AsyncStream that cannot be lowered to @_cdecl.
	SkipConcurrencyType
)

// String returns the human-readable name of the SkipReason.
func (s SkipReason) String() string {
	switch s {
	case SkipUnknown:
		return "SkipUnknown"
	case SkipGeneric:
		return "SkipGeneric"
	case SkipOpaque:
		return "SkipOpaque"
	case SkipExistential:
		return "SkipExistential"
	case SkipUnsafePointer:
		return "SkipUnsafePointer"
	case SkipClosure:
		return "SkipClosure"
	case SkipActor:
		return "SkipActor"
	case SkipMacro:
		return "SkipMacro"
	case SkipInternalAccess:
		return "SkipInternalAccess"
	case SkipAbstract:
		return "SkipAbstract"
	case SkipNSObject:
		return "SkipNSObject"
	case SkipConcurrencyType:
		return "SkipConcurrencyType"
	default:
		return fmt.Sprintf("SkipReason(%d)", int(s))
	}
}

// SkipReport records a single .swiftinterface item the bridge declined to
// translate. The collection of SkipReports for a module is rendered to
// SKIPPED.txt under the wrapper module directory.
type SkipReport struct {
	// ItemPath is the qualified Swift item name, e.g. "MyModule.MyType.myFunc".
	ItemPath string
	// Reason is the classification.
	Reason SkipReason
	// Detail is a free-text explanation specific to this skip.
	Detail string
}

// BridgeError is the top-level error returned by Driver entry points. It
// records the phase that produced the error and the underlying cause.
type BridgeError struct {
	// Phase is the bridge phase that detected the error, e.g. "lock",
	// "ingest", "wrapper", "build".
	Phase string
	// Item is the Swift item being processed when the error occurred.
	Item string
	// Message is the error message.
	Message string
}

// Error renders BridgeError as "swift bridge [phase] item: message".
func (e *BridgeError) Error() string {
	return fmt.Sprintf("swift bridge [%s] %s: %s", e.Phase, e.Item, e.Message)
}
