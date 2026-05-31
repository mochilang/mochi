// Package errors carries the cross-cutting error types the MEP-68 bridge
// emits at lock time and at build time. The most important one is SkipReport,
// which records why a particular .NET assembly item was not translated into a
// Mochi extern fn binding. See [website/docs/research/0068/05-type-mapping.md]
// for the closed set of refusal reasons.
package errors

import "fmt"

// SkipReason classifies why the bridge declined to translate a .NET item.
// The set mirrors the table in research note 05 §"Refusal cases".
type SkipReason int

const (
	// SkipUnknown is the zero value. It must never be emitted in practice.
	SkipUnknown SkipReason = iota
	// SkipUnsafePointer: the item carries a void* or T* parameter/return.
	SkipUnsafePointer
	// SkipByRef: a ref/out/in parameter of a reference type.
	SkipByRef
	// SkipSpanMemory: Span<T>, ReadOnlySpan<T>, Memory<T>, ReadOnlyMemory<T>.
	SkipSpanMemory
	// SkipDynamic: the type is dynamic; no static Mochi analogue.
	SkipDynamic
	// SkipObjectReturn: return type is System.Object (untyped).
	SkipObjectReturn
	// SkipAsyncEnumerable: IAsyncEnumerable<T>; deferred to phase 11.
	SkipAsyncEnumerable
	// SkipGeneric: unbound generic type parameter; add to [dotnet.monomorphise].
	SkipGeneric
	// SkipFlagsEnum: [Flags] enum translated to int, not extern type.
	SkipFlagsEnum
	// SkipUnsafeMethod: MethodImplAttributes.Unmanaged without unsafe capability.
	SkipUnsafeMethod
	// SkipOpenHierarchy: abstract class with external subtypes; cannot emit ADT.
	SkipOpenHierarchy
	// SkipEventDeclaration: event declaration; deferred to phase 11.
	SkipEventDeclaration
	// SkipCOMInterop: COM interop type; Windows-only, not NativeAOT-compatible.
	SkipCOMInterop
	// SkipRefStruct: ref struct type; stack-only, not transferable across ABI.
	SkipRefStruct
	// SkipNoNullableAnnotation: reference type with no NullableAttribute; assumed non-null.
	SkipNoNullableAnnotation
	// SkipNativeAOTIncompatible: method annotated [RequiresDynamicCode]; needs CoreCLR fallback.
	SkipNativeAOTIncompatible
	// SkipMulticastDelegate: multi-cast delegate; not representable as Mochi fn type.
	SkipMulticastDelegate
	// SkipIEnumerableMaterialisation: IEnumerable<T> materialised to list; may be expensive.
	SkipIEnumerableMaterialisation
	// SkipDecimalArithmetic: decimal arithmetic requires wrapper symbols; see research note 05.
	SkipDecimalArithmetic
)

// SkipReport records a single item the bridge declined to translate, with the
// reason and enough context to let the user understand and optionally override.
type SkipReport struct {
	PackageID  string
	Namespace  string
	TypeName   string
	ItemName   string // method, field, or property name; empty for type-level skips
	Reason     SkipReason
	Detail     string // human-readable detail (e.g. the specific type that caused the skip)
}

func (r SkipReport) Error() string {
	item := r.TypeName
	if r.ItemName != "" {
		item += "." + r.ItemName
	}
	return fmt.Sprintf("skip %s/%s.%s: %s: %s",
		r.PackageID, r.Namespace, item, r.Reason, r.Detail)
}

func (r SkipReason) String() string {
	switch r {
	case SkipUnsafePointer:
		return "unsafe pointer"
	case SkipByRef:
		return "by-ref parameter"
	case SkipSpanMemory:
		return "Span/Memory type"
	case SkipDynamic:
		return "dynamic type"
	case SkipObjectReturn:
		return "untyped object return"
	case SkipAsyncEnumerable:
		return "IAsyncEnumerable<T> (phase 11)"
	case SkipGeneric:
		return "unbound generic (add to [dotnet.monomorphise])"
	case SkipFlagsEnum:
		return "[Flags] enum translated to int"
	case SkipUnsafeMethod:
		return "unmanaged method (requires [dotnet.capabilities] unsafe = true)"
	case SkipOpenHierarchy:
		return "open abstract class hierarchy"
	case SkipEventDeclaration:
		return "event declaration (phase 11)"
	case SkipCOMInterop:
		return "COM interop type"
	case SkipRefStruct:
		return "ref struct (stack-only)"
	case SkipNoNullableAnnotation:
		return "no nullable annotation (assuming non-null)"
	case SkipNativeAOTIncompatible:
		return "requires dynamic code ([RequiresDynamicCode]); use nativeaot = false"
	case SkipMulticastDelegate:
		return "multi-cast delegate"
	case SkipIEnumerableMaterialisation:
		return "IEnumerable<T> materialised to list (may be expensive)"
	case SkipDecimalArithmetic:
		return "decimal arithmetic via wrapper symbols"
	default:
		return fmt.Sprintf("unknown(%d)", int(r))
	}
}

// BridgeError wraps a lower-level error with the package context that produced it.
type BridgeError struct {
	PackageID string
	Phase     string
	Err       error
}

func (e *BridgeError) Error() string {
	return fmt.Sprintf("dotnet bridge [%s/%s]: %v", e.PackageID, e.Phase, e.Err)
}

func (e *BridgeError) Unwrap() error { return e.Err }

// Wrap wraps err with package/phase context. Returns nil if err is nil.
func Wrap(packageID, phase string, err error) error {
	if err == nil {
		return nil
	}
	return &BridgeError{PackageID: packageID, Phase: phase, Err: err}
}
