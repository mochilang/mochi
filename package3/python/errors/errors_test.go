package errors

import (
	"errors"
	"strings"
	"testing"
)

func TestSkipReasonString(t *testing.T) {
	cases := []struct {
		reason SkipReason
		want   string
	}{
		{SkipUnknown, "SkipUnknown"},
		{SkipNoComplexType, "SkipNoComplexType"},
		{SkipOpenUnion, "SkipOpenUnion"},
		{SkipParamSpec, "SkipParamSpec"},
		{SkipTypeVarTuple, "SkipTypeVarTuple"},
		{SkipForwardRef, "SkipForwardRef"},
		{SkipUnsupportedTypingConstruct, "SkipUnsupportedTypingConstruct"},
		{SkipCFunctionWithoutStubs, "SkipCFunctionWithoutStubs"},
		{SkipOverloadAmbiguity, "SkipOverloadAmbiguity"},
		{SkipUntypedPackage, "SkipUntypedPackage"},
		{SkipImportTimeNetwork, "SkipImportTimeNetwork"},
		{SkipImportTimeError, "SkipImportTimeError"},
		{SkipPrivateName, "SkipPrivateName"},
		{SkipDunder, "SkipDunder"},
		{SkipDescriptor, "SkipDescriptor"},
		{SkipMetaclass, "SkipMetaclass"},
		{SkipDynamicAttribute, "SkipDynamicAttribute"},
		{SkipIncompatibleAsyncRuntime, "SkipIncompatibleAsyncRuntime"},
		{SkipBytearrayMutable, "SkipBytearrayMutable"},
		{SkipPyodideUnavailable, "SkipPyodideUnavailable"},
	}
	for _, tc := range cases {
		if got := tc.reason.String(); got != tc.want {
			t.Errorf("SkipReason(%d).String() = %q; want %q", tc.reason, got, tc.want)
		}
	}
}

func TestSkipReasonStringExhaustive(t *testing.T) {
	// All declared SkipReason constants from SkipNoComplexType through
	// SkipPyodideUnavailable must produce a non-"SkipUnknown" String. This
	// catches additions that forget to update the switch.
	for i := int(SkipNoComplexType); i <= int(SkipPyodideUnavailable); i++ {
		got := SkipReason(i).String()
		if got == "SkipUnknown" {
			t.Errorf("SkipReason(%d).String() returned SkipUnknown; add a case", i)
		}
		if !strings.HasPrefix(got, "Skip") {
			t.Errorf("SkipReason(%d).String() = %q; want Skip-prefix", i, got)
		}
	}
}

func TestSkipReasonUnknownOutOfRange(t *testing.T) {
	// Any SkipReason value outside the declared range renders as
	// SkipUnknown. This protects callers that bit-mask or arithmetic-
	// compute a SkipReason from undefined behaviour.
	for _, v := range []SkipReason{-1, SkipPyodideUnavailable + 1, 999} {
		if got := v.String(); got != "SkipUnknown" {
			t.Errorf("SkipReason(%d).String() = %q; want SkipUnknown", v, got)
		}
	}
}

func TestSkipReportString(t *testing.T) {
	r := SkipReport{
		ItemPath: "httpx.AsyncClient.send",
		Reason:   SkipParamSpec,
		Detail:   "parameter `**kwargs: P.kwargs` cannot be expressed in Mochi",
		Override: "write a hand-authored extern fn in httpx_externs.py",
	}
	got := r.String()
	wantLines := []string{
		"SKIPPED: httpx.AsyncClient.send",
		"  Reason: SkipParamSpec",
		"  Detail: parameter `**kwargs: P.kwargs` cannot be expressed in Mochi",
		"  Override: write a hand-authored extern fn in httpx_externs.py",
	}
	for _, line := range wantLines {
		if !strings.Contains(got, line) {
			t.Errorf("SkipReport.String() missing %q\n--- full output ---\n%s", line, got)
		}
	}
}

func TestSkipReportStringNoOverride(t *testing.T) {
	r := SkipReport{
		ItemPath: "numpy.complex128",
		Reason:   SkipNoComplexType,
		Detail:   "complex scalar not supported in Mochi v1",
	}
	got := r.String()
	if strings.Contains(got, "Override:") {
		t.Errorf("SkipReport.String() emitted Override: when none was set\n%s", got)
	}
	if !strings.Contains(got, "SKIPPED: numpy.complex128") {
		t.Errorf("SkipReport.String() missing item path\n%s", got)
	}
	if !strings.Contains(got, "Reason: SkipNoComplexType") {
		t.Errorf("SkipReport.String() missing reason\n%s", got)
	}
}

func TestSkipReportStringEmptyDetail(t *testing.T) {
	r := SkipReport{
		ItemPath: "foo.bar",
		Reason:   SkipMetaclass,
	}
	got := r.String()
	if !strings.Contains(got, "Detail: \n") {
		t.Errorf("SkipReport.String() should preserve empty Detail field\n%s", got)
	}
}

func TestBridgeErrorFormat(t *testing.T) {
	cause := errors.New("the cause")
	e := Wrap("ingest", "httpx", cause)
	if e == nil {
		t.Fatalf("Wrap returned nil with non-nil cause")
	}
	if e.Error() != "ingest[httpx]: the cause" {
		t.Errorf("BridgeError.Error() = %q; want %q", e.Error(), "ingest[httpx]: the cause")
	}
}

func TestBridgeErrorFormatNoPackage(t *testing.T) {
	cause := errors.New("phase-wide failure")
	e := Wrap("lock", "", cause)
	if e == nil {
		t.Fatalf("Wrap returned nil with non-nil cause")
	}
	if e.Error() != "lock: phase-wide failure" {
		t.Errorf("BridgeError.Error() = %q; want %q", e.Error(), "lock: phase-wide failure")
	}
}

func TestBridgeErrorUnwrap(t *testing.T) {
	cause := errors.New("the cause")
	e := Wrap("phase", "pkg", cause)
	if !errors.Is(e, cause) {
		t.Errorf("errors.Is(e, cause) was false; expected true via Unwrap")
	}
}

func TestBridgeErrorAs(t *testing.T) {
	cause := errors.New("the cause")
	e := Wrap("phase", "pkg", cause)
	var be *BridgeError
	if !errors.As(e, &be) {
		t.Fatalf("errors.As(e, *BridgeError) was false")
	}
	if be.Phase != "phase" {
		t.Errorf("be.Phase = %q; want %q", be.Phase, "phase")
	}
	if be.Package != "pkg" {
		t.Errorf("be.Package = %q; want %q", be.Package, "pkg")
	}
}

func TestWrapNil(t *testing.T) {
	if got := Wrap("phase", "pkg", nil); got != nil {
		t.Errorf("Wrap returned %v for nil cause; want nil", got)
	}
}

func TestBridgeErrorChain(t *testing.T) {
	// A BridgeError wrapped in another BridgeError should still unwrap to
	// the original cause via errors.Is.
	root := errors.New("root cause")
	inner := Wrap("ingest", "httpx", root)
	outer := Wrap("build", "httpx", inner)
	if !errors.Is(outer, root) {
		t.Errorf("errors.Is(outer, root) was false; chain broken")
	}
	if !errors.Is(outer, inner) {
		t.Errorf("errors.Is(outer, inner) was false; chain broken")
	}
}
