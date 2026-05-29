// Package errors carries the cross-cutting error types the MEP-67 bridge
// emits at lock time and at build time. The most important type is SkipReport,
// which records why a particular Java item was not translated into a Mochi
// extern binding. See the MEP-67 spec for the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a Java item.
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipRawObject: java.lang.Object without a generic context.
	SkipRawObject
	// SkipWildcard: wildcard type (?, ? extends T, ? super T).
	SkipWildcard
	// SkipUnchecked: unchecked cast or raw type usage.
	SkipUnchecked
	// SkipReflective: reflective API (java.lang.reflect.*, sun.*, com.sun.*).
	SkipReflective
	// SkipPrivate: non-public member.
	SkipPrivate
	// SkipVarargs: varargs method (ellipsis parameter).
	SkipVarargs
	// SkipAnnotation: annotation type (@interface).
	SkipAnnotation
	// SkipDeprecated: @Deprecated item (still mappable, but flagged).
	SkipDeprecated
	// SkipGeneric: unparameterised raw generic type (e.g., raw List without <T>).
	SkipGeneric
	// SkipInnerClass: anonymous or local inner class (name contains $).
	SkipInnerClass
	// SkipNative: native method declaration.
	SkipNative
	// SkipSynchronized: synchronized method (still mappable, but flagged).
	SkipSynchronized
	// SkipVolatile: volatile field.
	SkipVolatile
	// SkipFunctionalInterface: @FunctionalInterface (only for v1).
	SkipFunctionalInterface
)

// String renders the SkipReason as a short token used in SKIPPED.txt output.
// The token is stable across releases.
func (r SkipReason) String() string {
	switch r {
	case SkipRawObject:
		return "SkipRawObject"
	case SkipWildcard:
		return "SkipWildcard"
	case SkipUnchecked:
		return "SkipUnchecked"
	case SkipReflective:
		return "SkipReflective"
	case SkipPrivate:
		return "SkipPrivate"
	case SkipVarargs:
		return "SkipVarargs"
	case SkipAnnotation:
		return "SkipAnnotation"
	case SkipDeprecated:
		return "SkipDeprecated"
	case SkipGeneric:
		return "SkipGeneric"
	case SkipInnerClass:
		return "SkipInnerClass"
	case SkipNative:
		return "SkipNative"
	case SkipSynchronized:
		return "SkipSynchronized"
	case SkipVolatile:
		return "SkipVolatile"
	case SkipFunctionalInterface:
		return "SkipFunctionalInterface"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single Java item the bridge declined to translate.
type SkipReport struct {
	// ItemPath is the qualified Java name, e.g. "com.google.guava.base.Optional.of".
	ItemPath string
	// Reason is the classification.
	Reason SkipReason
	// Detail is a free-text explanation specific to this skip.
	Detail string
	// Override is the suggested hand-authored opt-in. May be empty.
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

// BridgeError is the top-level error returned by Driver entry points.
type BridgeError struct {
	// Phase is the bridge phase that detected the error, e.g. "lock", "reflect", "build".
	Phase string
	// Package is the Java package being processed when the error occurred. May be empty.
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
