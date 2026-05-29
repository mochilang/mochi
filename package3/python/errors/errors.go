// Package errors carries the cross-cutting error types the MEP-71 bridge
// emits at lock time and at build time. The most important one is SkipReport,
// which records why a particular Python item discovered through PEP 561
// stub ingest was not translated into a Mochi extern fn binding. See
// [website/docs/research/0071/05-type-mapping.md] §"The refusal table" for
// the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a Python item.
// The set mirrors the table in research note 05 §"The refusal table" plus
// the runtime / import-time refusals from note 10 §"Import-time side effects".
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipNoComplexType: parameter or return type is `complex`. Python has
	// arbitrary-precision complex numbers; Mochi has no counterpart.
	SkipNoComplexType
	// SkipOpenUnion: `Union[A, B, ...]` where at least one branch is `Any`
	// or two branches are subtype-overlapping at runtime.
	SkipOpenUnion
	// SkipParamSpec: `Callable[P, R]` with PEP 612 ParamSpec captures.
	SkipParamSpec
	// SkipTypeVarTuple: PEP 646 `tuple[*Ts]` or `Generic[*Ts]`.
	SkipTypeVarTuple
	// SkipForwardRef: an unresolvable forward reference in a stub.
	SkipForwardRef
	// SkipUnsupportedTypingConstruct: `cast`, `assert_type`, `reveal_type`,
	// or other runtime typing constructs that appear in inline annotations.
	SkipUnsupportedTypingConstruct
	// SkipCFunctionWithoutStubs: C extension function whose stub is missing
	// and stubgen returned `(*args, **kwargs)`.
	SkipCFunctionWithoutStubs
	// SkipOverloadAmbiguity: an `@overload` set where the closed type table
	// cannot pick a single arm.
	SkipOverloadAmbiguity
	// SkipUntypedPackage: the package has no PEP 561 marker, no typeshed
	// stubs, and stubgen fallback is disabled.
	SkipUntypedPackage
	// SkipImportTimeNetwork: the package tries to fetch over the network at
	// import time, which the lock-time sandbox denies.
	SkipImportTimeNetwork
	// SkipImportTimeError: the package raises an uncaught exception at
	// import time when the lock-time sandbox imports it.
	SkipImportTimeError
	// SkipPrivateName: the item is a leading-underscore name and is not
	// reachable through the package's __all__.
	SkipPrivateName
	// SkipDunder: a dunder method (`__init__`, `__del__`, ...) that the
	// bridge does not synthesise into a Mochi extern fn.
	SkipDunder
	// SkipDescriptor: a class with `__get__` / `__set__` / `__delete__` that
	// the bridge cannot bridge through a Mochi struct.
	SkipDescriptor
	// SkipMetaclass: a class with a non-`type` metaclass. The bridge does
	// not bridge metaclass behaviour.
	SkipMetaclass
	// SkipDynamicAttribute: a class with `__getattr__` / `__setattr__`. The
	// attribute surface is not statically discoverable.
	SkipDynamicAttribute
	// SkipIncompatibleAsyncRuntime: the package uses trio or another
	// asyncio-incompatible runtime.
	SkipIncompatibleAsyncRuntime
	// SkipBytearrayMutable: a `bytearray` parameter where the bridge would
	// have to write back through the boundary.
	SkipBytearrayMutable
	// SkipPyodideUnavailable: target is wasm32-emscripten but the package
	// is not in the Pyodide curated wheel index.
	SkipPyodideUnavailable
)

// String renders the SkipReason as a short token used in the SKIPPED.txt
// output file. The token is stable across releases; do not rename without
// adjusting the SKIPPED.txt golden fixtures.
func (r SkipReason) String() string {
	switch r {
	case SkipNoComplexType:
		return "SkipNoComplexType"
	case SkipOpenUnion:
		return "SkipOpenUnion"
	case SkipParamSpec:
		return "SkipParamSpec"
	case SkipTypeVarTuple:
		return "SkipTypeVarTuple"
	case SkipForwardRef:
		return "SkipForwardRef"
	case SkipUnsupportedTypingConstruct:
		return "SkipUnsupportedTypingConstruct"
	case SkipCFunctionWithoutStubs:
		return "SkipCFunctionWithoutStubs"
	case SkipOverloadAmbiguity:
		return "SkipOverloadAmbiguity"
	case SkipUntypedPackage:
		return "SkipUntypedPackage"
	case SkipImportTimeNetwork:
		return "SkipImportTimeNetwork"
	case SkipImportTimeError:
		return "SkipImportTimeError"
	case SkipPrivateName:
		return "SkipPrivateName"
	case SkipDunder:
		return "SkipDunder"
	case SkipDescriptor:
		return "SkipDescriptor"
	case SkipMetaclass:
		return "SkipMetaclass"
	case SkipDynamicAttribute:
		return "SkipDynamicAttribute"
	case SkipIncompatibleAsyncRuntime:
		return "SkipIncompatibleAsyncRuntime"
	case SkipBytearrayMutable:
		return "SkipBytearrayMutable"
	case SkipPyodideUnavailable:
		return "SkipPyodideUnavailable"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single Python item the bridge declined to translate.
// The collection of SkipReports for a package is rendered to SKIPPED.txt
// under the wrapper module directory at the end of phase 5.
type SkipReport struct {
	// ItemPath is the qualified Python path of the item, e.g.
	// "httpx.AsyncClient.send".
	ItemPath string
	// Reason is the classification.
	Reason SkipReason
	// Detail is a free-text explanation specific to this skip.
	Detail string
	// Override is the suggested sidecar override the user can write in the
	// MEP-51 Phase 12 *_externs.py file. May be empty when there is no
	// straightforward override available.
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
	// "ingest", "wrapper", "build", "publish".
	Phase string
	// Package is the upstream PyPI distribution name being processed when
	// the error occurred. Empty for phase-agnostic errors.
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

// Wrap constructs a BridgeError from a phase, a package (optional), and a
// cause. Returns nil if cause is nil.
func Wrap(phase, pkg string, cause error) error {
	if cause == nil {
		return nil
	}
	return &BridgeError{Phase: phase, Package: pkg, Cause: cause}
}
