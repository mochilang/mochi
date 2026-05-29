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
		{SkipLifetime, "SkipLifetime"},
		{SkipGeneric, "SkipGeneric"},
		{SkipImplTrait, "SkipImplTrait"},
		{SkipDynTrait, "SkipDynTrait"},
		{SkipRawPointer, "SkipRawPointer"},
		{SkipUnsafe, "SkipUnsafe"},
		{SkipPin, "SkipPin"},
		{SkipFuture, "SkipFuture"},
		{SkipCow, "SkipCow"},
		{SkipOsString, "SkipOsString"},
		{SkipNonClone, "SkipNonClone"},
		{SkipPubCrate, "SkipPubCrate"},
		{SkipTrait, "SkipTrait"},
		{SkipMacro, "SkipMacro"},
		{SkipConstant, "SkipConstant"},
		{SkipExternFnUnsafe, "SkipExternFnUnsafe"},
		{SkipCustomAbi, "SkipCustomAbi"},
		{SkipQualifiedPath, "SkipQualifiedPath"},
		{SkipOpaqueTypeAlias, "SkipOpaqueTypeAlias"},
	}
	for _, tc := range cases {
		if got := tc.reason.String(); got != tc.want {
			t.Errorf("SkipReason(%d).String() = %q; want %q", tc.reason, got, tc.want)
		}
	}
}

func TestSkipReasonStringExhaustive(t *testing.T) {
	// All declared SkipReason constants must produce a non-"SkipUnknown"
	// String when they are non-zero. This catches additions that forget to
	// update the switch.
	for i := int(SkipLifetime); i <= int(SkipOpaqueTypeAlias); i++ {
		got := SkipReason(i).String()
		if got == "SkipUnknown" {
			t.Errorf("SkipReason(%d).String() returned SkipUnknown; add a case", i)
		}
		if !strings.HasPrefix(got, "Skip") {
			t.Errorf("SkipReason(%d).String() = %q; want Skip-prefix", i, got)
		}
	}
}

func TestSkipReportString(t *testing.T) {
	r := SkipReport{
		ItemPath: "tokio::sync::mpsc::Receiver::poll_recv",
		Reason:   SkipPin,
		Detail:   "parameter `Pin<&mut Self>` cannot be expressed in Mochi",
		Override: "write `extern fn poll_recv(...) ... custom`",
	}
	got := r.String()
	wantLines := []string{
		"SKIPPED: tokio::sync::mpsc::Receiver::poll_recv",
		"  Reason: SkipPin",
		"  Detail: parameter `Pin<&mut Self>` cannot be expressed in Mochi",
		"  Override: write `extern fn poll_recv(...) ... custom`",
	}
	for _, line := range wantLines {
		if !strings.Contains(got, line) {
			t.Errorf("SkipReport.String() missing %q\n--- full output ---\n%s", line, got)
		}
	}
}

func TestSkipReportStringNoOverride(t *testing.T) {
	r := SkipReport{
		ItemPath: "foo::bar",
		Reason:   SkipMacro,
		Detail:   "macro definitions not supported in v1",
	}
	got := r.String()
	if strings.Contains(got, "Override:") {
		t.Errorf("SkipReport.String() emitted Override: when none was set\n%s", got)
	}
	if !strings.Contains(got, "SKIPPED: foo::bar") {
		t.Errorf("SkipReport.String() missing item path\n%s", got)
	}
}

func TestBridgeErrorFormat(t *testing.T) {
	cause := errors.New("the cause")
	e := Wrap("ingest", "tokio", cause)
	if e == nil {
		t.Fatalf("Wrap returned nil with non-nil cause")
	}
	if e.Error() != "ingest[tokio]: the cause" {
		t.Errorf("BridgeError.Error() = %q; want %q", e.Error(), "ingest[tokio]: the cause")
	}
}

func TestBridgeErrorFormatNoCrate(t *testing.T) {
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
	e := Wrap("phase", "crate", cause)
	if !errors.Is(e, cause) {
		t.Errorf("errors.Is(e, cause) was false; expected true via Unwrap")
	}
}

func TestWrapNil(t *testing.T) {
	if got := Wrap("phase", "crate", nil); got != nil {
		t.Errorf("Wrap returned %v for nil cause; want nil", got)
	}
}
