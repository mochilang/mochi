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
		{SkipUnexportedInPosition, "SkipUnexportedInPosition"},
		{SkipInternalPath, "SkipInternalPath"},
		{SkipGenericWithoutMonomorphise, "SkipGenericWithoutMonomorphise"},
		{SkipUnsafePointer, "SkipUnsafePointer"},
		{SkipReflectValue, "SkipReflectValue"},
		{SkipCgoHandle, "SkipCgoHandle"},
		{SkipInterfaceComplexMethod, "SkipInterfaceComplexMethod"},
		{SkipChanOfUnexportedStruct, "SkipChanOfUnexportedStruct"},
		{SkipMapKeyNotBasic, "SkipMapKeyNotBasic"},
		{SkipRequiresCgoCapability, "SkipRequiresCgoCapability"},
		{SkipBuildTagExcluded, "SkipBuildTagExcluded"},
		{SkipComplexNumeric, "SkipComplexNumeric"},
		{SkipPointerToPointer, "SkipPointerToPointer"},
		{SkipFuncReturningFunc, "SkipFuncReturningFunc"},
		{SkipVariadicComplex, "SkipVariadicComplex"},
		{SkipConstantItem, "SkipConstantItem"},
		{SkipVarItem, "SkipVarItem"},
		{SkipDeprecated, "SkipDeprecated"},
		{SkipNonPublishedReplace, "SkipNonPublishedReplace"},
	}
	for _, tc := range cases {
		if got := tc.reason.String(); got != tc.want {
			t.Errorf("SkipReason(%d).String() = %q; want %q", tc.reason, got, tc.want)
		}
	}
}

func TestSkipReasonStringExhaustive(t *testing.T) {
	// Every declared SkipReason constant must produce a non-"SkipUnknown"
	// String when non-zero. Adding a new constant without updating the
	// switch trips this test.
	for i := int(SkipUnexportedInPosition); i <= int(SkipNonPublishedReplace); i++ {
		got := SkipReason(i).String()
		if got == "SkipUnknown" {
			t.Errorf("SkipReason(%d).String() returned SkipUnknown; add a case", i)
		}
		if !strings.HasPrefix(got, "Skip") {
			t.Errorf("SkipReason(%d).String() = %q; want Skip-prefix", i, got)
		}
	}
}

func TestSkipReasonStringUnknownFallback(t *testing.T) {
	// Out-of-range values fall back to "SkipUnknown".
	if got := SkipReason(9999).String(); got != "SkipUnknown" {
		t.Errorf("SkipReason(9999).String() = %q; want SkipUnknown", got)
	}
	if got := SkipReason(-1).String(); got != "SkipUnknown" {
		t.Errorf("SkipReason(-1).String() = %q; want SkipUnknown", got)
	}
}

func TestSkipReportString(t *testing.T) {
	r := SkipReport{
		ItemPath: "github.com/spf13/cobra.Command.SetFlagErrorFunc",
		Reason:   SkipFuncReturningFunc,
		Detail:   "callback parameter is itself func(*Command, error) error",
		Override: "wrap in a Mochi closure and pass via [go.callback-shim]",
	}
	got := r.String()
	wantLines := []string{
		"SKIPPED: github.com/spf13/cobra.Command.SetFlagErrorFunc",
		"  Reason: SkipFuncReturningFunc",
		"  Detail: callback parameter is itself func(*Command, error) error",
		"  Override: wrap in a Mochi closure and pass via [go.callback-shim]",
	}
	for _, line := range wantLines {
		if !strings.Contains(got, line) {
			t.Errorf("SkipReport.String() missing %q\n--- full output ---\n%s", line, got)
		}
	}
}

func TestSkipReportStringNoOverride(t *testing.T) {
	r := SkipReport{
		ItemPath: "example.com/foo.Bar",
		Reason:   SkipDeprecated,
		Detail:   "doc-comment marks the item as deprecated",
	}
	got := r.String()
	if strings.Contains(got, "Override:") {
		t.Errorf("SkipReport.String() emitted Override: when none was set\n%s", got)
	}
	if !strings.Contains(got, "SKIPPED: example.com/foo.Bar") {
		t.Errorf("SkipReport.String() missing item path\n%s", got)
	}
}

func TestBridgeErrorFormat(t *testing.T) {
	cause := errors.New("the cause")
	e := Wrap("ingest", "github.com/spf13/cobra", cause)
	if e == nil {
		t.Fatalf("Wrap returned nil with non-nil cause")
	}
	want := "ingest[github.com/spf13/cobra]: the cause"
	if e.Error() != want {
		t.Errorf("BridgeError.Error() = %q; want %q", e.Error(), want)
	}
}

func TestBridgeErrorFormatNoModule(t *testing.T) {
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
	e := Wrap("phase", "module", cause)
	if !errors.Is(e, cause) {
		t.Errorf("errors.Is(e, cause) was false; expected true via Unwrap")
	}
}

func TestBridgeErrorUnwrapDirect(t *testing.T) {
	cause := errors.New("the cause")
	be := &BridgeError{Phase: "p", Module: "m", Cause: cause}
	if be.Unwrap() != cause {
		t.Errorf("Unwrap() = %v; want %v", be.Unwrap(), cause)
	}
}

func TestWrapNil(t *testing.T) {
	if got := Wrap("phase", "module", nil); got != nil {
		t.Errorf("Wrap returned %v for nil cause; want nil", got)
	}
}
