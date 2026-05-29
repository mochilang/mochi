package errors

import (
	"errors"
	"strings"
	"testing"
)

func TestSkipReasonString(t *testing.T) {
	cases := []struct {
		r    SkipReason
		want string
	}{
		{SkipUnknown, "SkipUnknown"},
		{SkipRawObject, "SkipRawObject"},
		{SkipWildcard, "SkipWildcard"},
		{SkipUnchecked, "SkipUnchecked"},
		{SkipReflective, "SkipReflective"},
		{SkipPrivate, "SkipPrivate"},
		{SkipVarargs, "SkipVarargs"},
		{SkipAnnotation, "SkipAnnotation"},
		{SkipDeprecated, "SkipDeprecated"},
		{SkipGeneric, "SkipGeneric"},
		{SkipInnerClass, "SkipInnerClass"},
		{SkipNative, "SkipNative"},
		{SkipSynchronized, "SkipSynchronized"},
		{SkipVolatile, "SkipVolatile"},
		{SkipFunctionalInterface, "SkipFunctionalInterface"},
	}
	for _, c := range cases {
		if got := c.r.String(); got != c.want {
			t.Errorf("SkipReason(%d).String() = %q; want %q", c.r, got, c.want)
		}
	}
}

func TestSkipReportString(t *testing.T) {
	r := SkipReport{
		ItemPath: "com.example.Foo.bar",
		Reason:   SkipNative,
		Detail:   "native method",
		Override: "",
	}
	s := r.String()
	if !strings.Contains(s, "SKIPPED: com.example.Foo.bar") {
		t.Errorf("missing item path in %q", s)
	}
	if !strings.Contains(s, "SkipNative") {
		t.Errorf("missing reason in %q", s)
	}

	r2 := SkipReport{
		ItemPath: "com.example.Foo.baz",
		Reason:   SkipDeprecated,
		Detail:   "deprecated",
		Override: "[java.allow-deprecated]",
	}
	s2 := r2.String()
	if !strings.Contains(s2, "Override:") {
		t.Errorf("missing override line in %q", s2)
	}
}

func TestBridgeError(t *testing.T) {
	cause := errors.New("something went wrong")
	err := Wrap("reflect", "com.google.guava", cause)
	be := err.(*BridgeError)
	if be.Phase != "reflect" {
		t.Errorf("Phase = %q; want %q", be.Phase, "reflect")
	}
	if !strings.Contains(be.Error(), "reflect[com.google.guava]") {
		t.Errorf("Error() = %q; missing expected prefix", be.Error())
	}
	if !errors.Is(err, cause) {
		t.Errorf("errors.Is failed for wrapped cause")
	}
}

func TestWrapNil(t *testing.T) {
	if Wrap("phase", "pkg", nil) != nil {
		t.Error("Wrap with nil cause should return nil")
	}
}

func TestBridgeErrorNoPackage(t *testing.T) {
	err := Wrap("lock", "", errors.New("oops"))
	if !strings.Contains(err.Error(), "lock: oops") {
		t.Errorf("Error() = %q; want 'lock: oops'", err.Error())
	}
}
