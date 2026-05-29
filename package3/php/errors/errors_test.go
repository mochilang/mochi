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
		{SkipMixed, "SkipMixed"},
		{SkipObject, "SkipObject"},
		{SkipUntypedArray, "SkipUntypedArray"},
		{SkipSelfStatic, "SkipSelfStatic"},
		{SkipCallable, "SkipCallable"},
		{SkipResource, "SkipResource"},
		{SkipIntersection, "SkipIntersection"},
		{SkipNever, "SkipNever"},
		{SkipVararg, "SkipVararg"},
		{SkipPrivate, "SkipPrivate"},
		{SkipAbstractNoImpl, "SkipAbstractNoImpl"},
		{SkipMagicMethod, "SkipMagicMethod"},
		{SkipAnonymousClass, "SkipAnonymousClass"},
		{SkipNoReflection, "SkipNoReflection"},
		{SkipExtension, "SkipExtension"},
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
	for i := int(SkipMixed); i <= int(SkipExtension); i++ {
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
		ItemPath: `GuzzleHttp\Client::send`,
		Reason:   SkipCallable,
		Detail:   "parameter $handler is callable pseudo-type with no stable ABI",
		Override: "write `extern fn send(...) ... custom`",
	}
	got := r.String()
	wantLines := []string{
		`SKIPPED: GuzzleHttp\Client::send`,
		"  Reason: SkipCallable",
		"  Detail: parameter $handler is callable pseudo-type with no stable ABI",
		"  Override: write `extern fn send(...) ... custom`",
	}
	for _, line := range wantLines {
		if !strings.Contains(got, line) {
			t.Errorf("SkipReport.String() missing %q\n--- full output ---\n%s", line, got)
		}
	}
}

func TestSkipReportStringNoOverride(t *testing.T) {
	r := SkipReport{
		ItemPath: `Foo\Bar::magic`,
		Reason:   SkipMagicMethod,
		Detail:   "__get/__set magic methods use dynamic dispatch only",
	}
	got := r.String()
	if strings.Contains(got, "Override:") {
		t.Errorf("SkipReport.String() emitted Override: when none was set\n%s", got)
	}
	if !strings.Contains(got, `SKIPPED: Foo\Bar::magic`) {
		t.Errorf("SkipReport.String() missing item path\n%s", got)
	}
}

func TestBridgeErrorFormat(t *testing.T) {
	cause := errors.New("the cause")
	e := Wrap("reflect", "guzzlehttp/guzzle", cause)
	if e == nil {
		t.Fatalf("Wrap returned nil with non-nil cause")
	}
	if e.Error() != "reflect[guzzlehttp/guzzle]: the cause" {
		t.Errorf("BridgeError.Error() = %q; want %q", e.Error(), "reflect[guzzlehttp/guzzle]: the cause")
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
	e := Wrap("phase", "vendor/pkg", cause)
	if !errors.Is(e, cause) {
		t.Errorf("errors.Is(e, cause) was false; expected true via Unwrap")
	}
}

func TestWrapNil(t *testing.T) {
	if got := Wrap("phase", "vendor/pkg", nil); got != nil {
		t.Errorf("Wrap returned %v for nil cause; want nil", got)
	}
}
