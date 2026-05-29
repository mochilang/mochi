// Package errors carries the cross-cutting error types the MEP-66 bridge
// emits at lock time and at build time. The most important type is SkipReport,
// which records why a particular Erlang item discovered through BEAM abstract
// code ingest was not translated into a Mochi extern fn binding. See
// [website/docs/research/0066/05-type-mapping.md] for the closed set of
// refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate an Erlang item.
// The set mirrors the table in research note 05 §"Complete translation table"
// plus the runtime refusals from note 12 §"Risk register".
type SkipReason int

const (
	// SkipUnknown is the zero value and must never be emitted in practice.
	SkipUnknown SkipReason = iota

	// SkipAnyTerm: parameter or return type is any() or term() (the Erlang
	// top type). Mochi has no counterpart for the unrestricted top type.
	SkipAnyTerm

	// SkipBitstring: type is bitstring() or a non-byte-aligned bit string.
	// The bridge only handles byte-aligned binary() (maps to bytes).
	SkipBitstring

	// SkipCharlist: type is string() in Erlang's sense — a list of Unicode
	// codepoint integers. Use binary() instead for UTF-8 strings.
	SkipCharlist

	// SkipComplexUnion: a union type with 3+ branches that does not match
	// the ok/error pattern recogniser.
	SkipComplexUnion

	// SkipEDoc: the package shipped no BEAM abstract code with -spec
	// annotations; the bridge fell back to EDoc XML extraction, which is
	// less precise. The translation proceeded but with lower type fidelity.
	SkipEDoc

	// SkipFunArgNotInTable: a fun() type where at least one argument type
	// is outside the closed translation table.
	SkipFunArgNotInTable

	// SkipIodata: type is iodata() or iolist() — a recursive union of
	// binary and list-of-binary that Mochi cannot represent directly.
	SkipIodata

	// SkipNonOkErrorUnion: a 2-branch union where the branches do not
	// match the {ok, T} | {error, Reason} pattern.
	SkipNonOkErrorUnion

	// SkipNoSpec: the function is exported but has no -spec annotation.
	// The bridge cannot generate a typed binding without a spec.
	SkipNoSpec

	// SkipNoTypeinfo: the package ships neither BEAM abstract code with
	// -spec annotations nor EDoc XML. No bindings can be generated.
	SkipNoTypeinfo

	// SkipRecursiveType: a user-defined type alias is recursive (directly
	// or transitively refers back to itself). Expansion depth limit hit.
	SkipRecursiveType

	// SkipRemoteType: a remote_type reference (Module:TypeName) refers to a
	// module that is not in the current dep graph.
	SkipRemoteType

	// SkipTypedMap: type is a map with typed keys/values (#{K := V}).
	// Mochi has no structural map type equivalent.
	SkipTypedMap

	// SkipUntypedFun: type is fun() with no arity or type annotation.
	SkipUntypedFun

	// SkipUntypedMap: type is map() (untyped map). Mochi requires typed
	// keys and values.
	SkipUntypedMap

	// SkipUntypedTuple: type is tuple() with no element types.
	SkipUntypedTuple

	// SkipElixirRuntime: the module requires the Elixir runtime (uses
	// Elixir.Kernel, __struct__, or Protocol dispatch). Not bridgeable
	// without elixir-compat mode.
	SkipElixirRuntime
)

// String renders the SkipReason as a stable short token used in SKIPPED.txt
// output. Do not rename tokens without updating SKIPPED.txt golden fixtures.
func (r SkipReason) String() string {
	switch r {
	case SkipAnyTerm:
		return "SkipAnyTerm"
	case SkipBitstring:
		return "SkipBitstring"
	case SkipCharlist:
		return "SkipCharlist"
	case SkipComplexUnion:
		return "SkipComplexUnion"
	case SkipEDoc:
		return "SkipEDoc"
	case SkipFunArgNotInTable:
		return "SkipFunArgNotInTable"
	case SkipIodata:
		return "SkipIodata"
	case SkipNonOkErrorUnion:
		return "SkipNonOkErrorUnion"
	case SkipNoSpec:
		return "SkipNoSpec"
	case SkipNoTypeinfo:
		return "SkipNoTypeinfo"
	case SkipRecursiveType:
		return "SkipRecursiveType"
	case SkipRemoteType:
		return "SkipRemoteType"
	case SkipTypedMap:
		return "SkipTypedMap"
	case SkipUntypedFun:
		return "SkipUntypedFun"
	case SkipUntypedMap:
		return "SkipUntypedMap"
	case SkipUntypedTuple:
		return "SkipUntypedTuple"
	case SkipElixirRuntime:
		return "SkipElixirRuntime"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single Erlang item the bridge declined to translate.
// The collection of SkipReports for a package is rendered to SKIPPED.txt
// under the erlang_shims/<app>/ directory at the end of phase 5.
type SkipReport struct {
	// Module is the Erlang module name, e.g. "hackney".
	Module string
	// Function is the exported function name. Empty for type-level skips.
	Function string
	// Arity is the function arity. -1 when not applicable.
	Arity int
	// Position is where in the signature the skip occurred: "arg1",
	// "arg2", ..., "return", or "type" for type-alias skips.
	Position string
	// Reason is the classification.
	Reason SkipReason
	// Detail is a free-text note specific to this skip.
	Detail string
}

// String renders a SkipReport in the SKIPPED.txt format.
func (s SkipReport) String() string {
	var loc string
	if s.Function != "" && s.Arity >= 0 {
		loc = fmt.Sprintf("%s:%s/%d@%s", s.Module, s.Function, s.Arity, s.Position)
	} else if s.Function != "" {
		loc = fmt.Sprintf("%s:%s@%s", s.Module, s.Function, s.Position)
	} else {
		loc = s.Module
	}
	return fmt.Sprintf("SKIPPED: %s\n  Reason: %s\n  Detail: %s\n", loc, s.Reason, s.Detail)
}

// BridgeError is the top-level error returned by Driver entry points. It
// records the phase that produced the error and the underlying cause.
type BridgeError struct {
	// Phase is the bridge phase that detected the error, e.g. "lock",
	// "ingest", "shim-emit", "build", "publish".
	Phase string
	// Package is the Hex.pm package name being processed. Empty for
	// phase-agnostic errors.
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
