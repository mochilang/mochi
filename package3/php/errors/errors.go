// Package errors carries the cross-cutting error types the MEP-75 PHP bridge
// emits at lock time and at build time. The most important one is SkipReport,
// which records why a particular PHP reflection item was not translated into a
// Mochi extern binding. See [website/docs/research/0075/05-type-mapping.md]
// for the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a PHP reflection item.
// The set mirrors the closed table in research note 05 §"Refusal reasons".
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipMixed: PHP mixed type has no Mochi equivalent; requires hand annotation.
	SkipMixed
	// SkipObject: bare object type (untyped class instance).
	SkipObject
	// SkipUntypedArray: array without a typed key/value hint.
	SkipUntypedArray
	// SkipSelfStatic: self or static type in non-constructor context.
	SkipSelfStatic
	// SkipCallable: callable pseudo-type; no stable ABI.
	SkipCallable
	// SkipResource: PHP resource handle (pre-8.0 legacy).
	SkipResource
	// SkipIntersection: intersection type (A&B) — v1 has no intersection surface.
	SkipIntersection
	// SkipNever: never return type represents unreachable; emitted as panic boundary.
	SkipNever
	// SkipVararg: variadic parameter (...$args) with mixed type.
	SkipVararg
	// SkipPrivate: private method or property not reachable from Mochi.
	SkipPrivate
	// SkipAbstractNoImpl: abstract method with no concrete implementation path.
	SkipAbstractNoImpl
	// SkipMagicMethod: __get, __set, __call etc.; dynamic dispatch only.
	SkipMagicMethod
	// SkipAnonymousClass: anonymous class expression; no stable name.
	SkipAnonymousClass
	// SkipNoReflection: class or function invisible to PHP ReflectionAPI.
	SkipNoReflection
	// SkipExtension: defined by a C extension; requires native binding override.
	SkipExtension
)

// String renders the SkipReason as a short token used in the SKIPPED.txt
// output file. The token is stable across releases; do not rename without
// adjusting the SKIPPED.txt golden fixtures.
func (r SkipReason) String() string {
	switch r {
	case SkipMixed:
		return "SkipMixed"
	case SkipObject:
		return "SkipObject"
	case SkipUntypedArray:
		return "SkipUntypedArray"
	case SkipSelfStatic:
		return "SkipSelfStatic"
	case SkipCallable:
		return "SkipCallable"
	case SkipResource:
		return "SkipResource"
	case SkipIntersection:
		return "SkipIntersection"
	case SkipNever:
		return "SkipNever"
	case SkipVararg:
		return "SkipVararg"
	case SkipPrivate:
		return "SkipPrivate"
	case SkipAbstractNoImpl:
		return "SkipAbstractNoImpl"
	case SkipMagicMethod:
		return "SkipMagicMethod"
	case SkipAnonymousClass:
		return "SkipAnonymousClass"
	case SkipNoReflection:
		return "SkipNoReflection"
	case SkipExtension:
		return "SkipExtension"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single PHP reflection item the bridge declined to translate.
// The collection of SkipReports for a package is rendered to SKIPPED.txt under
// the shim directory at the end of phase 5.
type SkipReport struct {
	// ItemPath is the PHP FQCN or function path, e.g. "GuzzleHttp\Client::send".
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
	// "reflect", "typemap", "externemit".
	Phase string
	// Package is the upstream Composer package being processed when the error
	// occurred. Empty for phase-agnostic errors.
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
