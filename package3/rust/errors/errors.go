// Package errors carries the cross-cutting error types the MEP-73 bridge
// emits at lock time and at build time. The most important one is SkipReport,
// which records why a particular rustdoc item was not translated into a
// Mochi extern fn binding. See [website/docs/research/0073/05-type-mapping.md]
// for the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a rustdoc item.
// The set mirrors the table in research note 05 §"Refusal reasons".
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipLifetime: the item carries a non-'static lifetime that the bridge
	// cannot erase safely.
	SkipLifetime
	// SkipGeneric: the item has an unconcretised type parameter and the
	// user has not enumerated it in [rust.monomorphise].
	SkipGeneric
	// SkipImplTrait: impl Trait in return position. Requires explicit
	// monomorphisation.
	SkipImplTrait
	// SkipDynTrait: dyn Trait. v1 has no opaque-trait-object surface.
	SkipDynTrait
	// SkipRawPointer: *const T or *mut T parameter or return.
	SkipRawPointer
	// SkipUnsafe: unsafe fn without [rust.capabilities] unsafe = true.
	SkipUnsafe
	// SkipPin: Pin<Box<T>> or Pin<&T> parameter or return.
	SkipPin
	// SkipFuture: Box<dyn Future> direct return.
	SkipFuture
	// SkipCow: Cow<str> or Cow<[T]>.
	SkipCow
	// SkipOsString: OsString / OsStr / PathBuf / CString.
	SkipOsString
	// SkipNonClone: a struct without #[derive(Clone)].
	SkipNonClone
	// SkipPubCrate: pub(crate) or pub(super); not visible in rustdoc public surface.
	SkipPubCrate
	// SkipTrait: trait definition (v1 has no trait dispatch).
	SkipTrait
	// SkipMacro: macro_rules! or proc-macro.
	SkipMacro
	// SkipConstant: const item (v1 doesn't bind consts).
	SkipConstant
	// SkipExternFnUnsafe: extern "C" unsafe fn.
	SkipExternFnUnsafe
	// SkipCustomAbi: extern "stdcall" / "fastcall" / etc.
	SkipCustomAbi
	// SkipQualifiedPath: <T as Trait>::Item.
	SkipQualifiedPath
	// SkipOpaqueTypeAlias: type T = impl Trait.
	SkipOpaqueTypeAlias
)

// String renders the SkipReason as a short token used in the SKIPPED.txt
// output file. The token is stable across releases; do not rename without
// adjusting the SKIPPED.txt golden fixtures.
func (r SkipReason) String() string {
	switch r {
	case SkipLifetime:
		return "SkipLifetime"
	case SkipGeneric:
		return "SkipGeneric"
	case SkipImplTrait:
		return "SkipImplTrait"
	case SkipDynTrait:
		return "SkipDynTrait"
	case SkipRawPointer:
		return "SkipRawPointer"
	case SkipUnsafe:
		return "SkipUnsafe"
	case SkipPin:
		return "SkipPin"
	case SkipFuture:
		return "SkipFuture"
	case SkipCow:
		return "SkipCow"
	case SkipOsString:
		return "SkipOsString"
	case SkipNonClone:
		return "SkipNonClone"
	case SkipPubCrate:
		return "SkipPubCrate"
	case SkipTrait:
		return "SkipTrait"
	case SkipMacro:
		return "SkipMacro"
	case SkipConstant:
		return "SkipConstant"
	case SkipExternFnUnsafe:
		return "SkipExternFnUnsafe"
	case SkipCustomAbi:
		return "SkipCustomAbi"
	case SkipQualifiedPath:
		return "SkipQualifiedPath"
	case SkipOpaqueTypeAlias:
		return "SkipOpaqueTypeAlias"
	default:
		return "SkipUnknown"
	}
}

// SkipReport records a single rustdoc item the bridge declined to translate.
// The collection of SkipReports for a crate is rendered to SKIPPED.txt under
// the wrapper crate directory at the end of phase 4.
type SkipReport struct {
	// ItemPath is the rustdoc path of the item, e.g. "tokio::sync::mpsc::Receiver::poll_recv".
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
	// Crate is the upstream crate name being processed when the error
	// occurred. Empty for phase-agnostic errors.
	Crate string
	// Cause is the underlying error.
	Cause error
}

// Error renders BridgeError as "phase[crate]: cause".
func (e *BridgeError) Error() string {
	if e.Crate == "" {
		return fmt.Sprintf("%s: %v", e.Phase, e.Cause)
	}
	return fmt.Sprintf("%s[%s]: %v", e.Phase, e.Crate, e.Cause)
}

// Unwrap exposes the underlying cause for errors.Is / errors.As.
func (e *BridgeError) Unwrap() error { return e.Cause }

// Wrap constructs a BridgeError from a phase, a crate (optional), and a cause.
func Wrap(phase, crate string, cause error) error {
	if cause == nil {
		return nil
	}
	return &BridgeError{Phase: phase, Crate: crate, Cause: cause}
}
