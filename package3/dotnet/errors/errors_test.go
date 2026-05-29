package errors_test

import (
	"fmt"
	"strings"
	"testing"

	"mochi/package3/dotnet/errors"
)

func TestSkipReason_String_allNonEmpty(t *testing.T) {
	reasons := []errors.SkipReason{
		errors.SkipGeneric,
		errors.SkipPointer,
		errors.SkipRefType,
		errors.SkipInterface,
		errors.SkipDelegate,
		errors.SkipDynamic,
		errors.SkipCOMInterop,
		errors.SkipUnsafe,
		errors.SkipInternal,
		errors.SkipAbstract,
		errors.SkipObsolete,
		errors.SkipNestedType,
		errors.SkipIndexer,
		errors.SkipOperator,
		errors.SkipEvent,
		errors.SkipMultiReturn,
		errors.SkipSpan,
		errors.SkipValueTask,
	}
	for _, r := range reasons {
		s := r.String()
		if s == "" {
			t.Errorf("SkipReason(%d).String() is empty", int(r))
		}
		if s == "SkipUnknown" {
			t.Errorf("SkipReason(%d).String() returned SkipUnknown unexpectedly", int(r))
		}
	}
}

func TestSkipReason_String_zeroIsUnknown(t *testing.T) {
	var r errors.SkipReason
	if got := r.String(); got != "SkipUnknown" {
		t.Errorf("zero SkipReason.String() = %q; want %q", got, "SkipUnknown")
	}
}

func TestSkipReason_String_specificValues(t *testing.T) {
	cases := []struct {
		reason errors.SkipReason
		want   string
	}{
		{errors.SkipGeneric, "SkipGeneric"},
		{errors.SkipPointer, "SkipPointer"},
		{errors.SkipRefType, "SkipRefType"},
		{errors.SkipInterface, "SkipInterface"},
		{errors.SkipDelegate, "SkipDelegate"},
		{errors.SkipDynamic, "SkipDynamic"},
		{errors.SkipCOMInterop, "SkipCOMInterop"},
		{errors.SkipUnsafe, "SkipUnsafe"},
		{errors.SkipInternal, "SkipInternal"},
		{errors.SkipAbstract, "SkipAbstract"},
		{errors.SkipObsolete, "SkipObsolete"},
		{errors.SkipNestedType, "SkipNestedType"},
		{errors.SkipIndexer, "SkipIndexer"},
		{errors.SkipOperator, "SkipOperator"},
		{errors.SkipEvent, "SkipEvent"},
		{errors.SkipMultiReturn, "SkipMultiReturn"},
		{errors.SkipSpan, "SkipSpan"},
		{errors.SkipValueTask, "SkipValueTask"},
	}
	for _, tc := range cases {
		if got := tc.reason.String(); got != tc.want {
			t.Errorf("SkipReason.String() = %q; want %q", got, tc.want)
		}
	}
}

func TestBridgeError_Error_withPackage(t *testing.T) {
	err := &errors.BridgeError{
		Phase:   "lock",
		Package: "Newtonsoft.Json",
		Cause:   fmt.Errorf("network timeout"),
	}
	got := err.Error()
	if got != "lock[Newtonsoft.Json]: network timeout" {
		t.Errorf("BridgeError.Error() = %q; want %q", got, "lock[Newtonsoft.Json]: network timeout")
	}
}

func TestBridgeError_Error_withoutPackage(t *testing.T) {
	err := &errors.BridgeError{
		Phase: "ingest",
		Cause: fmt.Errorf("missing assembly"),
	}
	got := err.Error()
	if got != "ingest: missing assembly" {
		t.Errorf("BridgeError.Error() = %q; want %q", got, "ingest: missing assembly")
	}
}

func TestBridgeError_Unwrap(t *testing.T) {
	cause := fmt.Errorf("root cause")
	err := &errors.BridgeError{Phase: "build", Cause: cause}
	if err.Unwrap() != cause {
		t.Error("Unwrap() did not return the original cause")
	}
}

func TestWrap_nilCause(t *testing.T) {
	if errors.Wrap("lock", "pkg", nil) != nil {
		t.Error("Wrap with nil cause must return nil")
	}
}

func TestWrap_nonNil(t *testing.T) {
	cause := fmt.Errorf("boom")
	wrapped := errors.Wrap("build", "MyLib", cause)
	if wrapped == nil {
		t.Fatal("Wrap returned nil for non-nil cause")
	}
	if !strings.Contains(wrapped.Error(), "build[MyLib]") {
		t.Errorf("wrapped error %q missing phase+package prefix", wrapped.Error())
	}
}

func TestSkipReport_String(t *testing.T) {
	r := errors.SkipReport{
		ItemPath: "System.Console.WriteLine",
		Reason:   errors.SkipDelegate,
		Detail:   "function pointer param",
	}
	s := r.String()
	if !strings.Contains(s, "SKIPPED") {
		t.Error("SkipReport.String() missing SKIPPED prefix")
	}
	if !strings.Contains(s, "SkipDelegate") {
		t.Error("SkipReport.String() missing reason token")
	}
}

func TestSkipReport_String_withOverride(t *testing.T) {
	r := errors.SkipReport{
		ItemPath: "System.Foo",
		Reason:   errors.SkipObsolete,
		Detail:   "marked [Obsolete]",
		Override: "[[dotnet-package.allow-obsolete]]",
	}
	s := r.String()
	if !strings.Contains(s, "Override") {
		t.Error("SkipReport.String() missing Override line")
	}
}
