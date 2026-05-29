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
		{SkipAnyTerm, "SkipAnyTerm"},
		{SkipBitstring, "SkipBitstring"},
		{SkipCharlist, "SkipCharlist"},
		{SkipComplexUnion, "SkipComplexUnion"},
		{SkipEDoc, "SkipEDoc"},
		{SkipFunArgNotInTable, "SkipFunArgNotInTable"},
		{SkipIodata, "SkipIodata"},
		{SkipNonOkErrorUnion, "SkipNonOkErrorUnion"},
		{SkipNoSpec, "SkipNoSpec"},
		{SkipNoTypeinfo, "SkipNoTypeinfo"},
		{SkipRecursiveType, "SkipRecursiveType"},
		{SkipRemoteType, "SkipRemoteType"},
		{SkipTypedMap, "SkipTypedMap"},
		{SkipUntypedFun, "SkipUntypedFun"},
		{SkipUntypedMap, "SkipUntypedMap"},
		{SkipUntypedTuple, "SkipUntypedTuple"},
		{SkipElixirRuntime, "SkipElixirRuntime"},
		{SkipReason(999), "SkipUnknown"},
	}
	for _, c := range cases {
		if got := c.reason.String(); got != c.want {
			t.Errorf("SkipReason(%d).String() = %q, want %q", int(c.reason), got, c.want)
		}
	}
}

func TestAllSkipReasonsHaveUniqueStrings(t *testing.T) {
	reasons := []SkipReason{
		SkipAnyTerm, SkipBitstring, SkipCharlist, SkipComplexUnion,
		SkipEDoc, SkipFunArgNotInTable, SkipIodata, SkipNonOkErrorUnion,
		SkipNoSpec, SkipNoTypeinfo, SkipRecursiveType, SkipRemoteType,
		SkipTypedMap, SkipUntypedFun, SkipUntypedMap, SkipUntypedTuple,
		SkipElixirRuntime,
	}
	seen := make(map[string]bool)
	for _, r := range reasons {
		s := r.String()
		if s == "SkipUnknown" {
			t.Errorf("reason %d maps to SkipUnknown", int(r))
		}
		if seen[s] {
			t.Errorf("duplicate string %q for reason %d", s, int(r))
		}
		seen[s] = true
	}
}

func TestSkipReportString_WithFunction(t *testing.T) {
	sr := SkipReport{
		Module:   "hackney",
		Function: "get",
		Arity:    4,
		Position: "return",
		Reason:   SkipComplexUnion,
		Detail:   "union has 3 branches",
	}
	got := sr.String()
	if !strings.Contains(got, "hackney:get/4@return") {
		t.Errorf("expected location in output, got: %s", got)
	}
	if !strings.Contains(got, "SkipComplexUnion") {
		t.Errorf("expected SkipComplexUnion in output, got: %s", got)
	}
	if !strings.Contains(got, "union has 3 branches") {
		t.Errorf("expected detail in output, got: %s", got)
	}
}

func TestSkipReportString_NoFunction(t *testing.T) {
	sr := SkipReport{
		Module: "lager",
		Arity:  -1,
		Reason: SkipNoTypeinfo,
		Detail: "no abstract code or EDoc",
	}
	got := sr.String()
	if !strings.Contains(got, "SKIPPED: lager") {
		t.Errorf("expected module-only location, got: %s", got)
	}
}

func TestSkipReportString_FunctionNoArity(t *testing.T) {
	sr := SkipReport{
		Module:   "cowboy",
		Function: "start_clear",
		Arity:    -1,
		Position: "arg1",
		Reason:   SkipUntypedMap,
		Detail:   "untyped map()",
	}
	got := sr.String()
	if !strings.Contains(got, "cowboy:start_clear@arg1") {
		t.Errorf("expected function@position location, got: %s", got)
	}
}

func TestBridgeError_NoPackage(t *testing.T) {
	err := &BridgeError{Phase: "lock", Cause: errors.New("network error")}
	got := err.Error()
	if got != "lock: network error" {
		t.Errorf("BridgeError.Error() = %q, want %q", got, "lock: network error")
	}
}

func TestBridgeError_WithPackage(t *testing.T) {
	err := &BridgeError{Phase: "ingest", Package: "cowboy", Cause: errors.New("missing Dbgi chunk")}
	got := err.Error()
	if got != "ingest[cowboy]: missing Dbgi chunk" {
		t.Errorf("BridgeError.Error() = %q", got)
	}
}

func TestBridgeError_Unwrap(t *testing.T) {
	inner := errors.New("inner")
	err := &BridgeError{Phase: "build", Cause: inner}
	if !errors.Is(err, inner) {
		t.Error("errors.Is should find inner error via Unwrap")
	}
}

func TestWrap_NilCause(t *testing.T) {
	if got := Wrap("lock", "cowboy", nil); got != nil {
		t.Errorf("Wrap with nil cause should return nil, got %v", got)
	}
}

func TestWrap_NonNil(t *testing.T) {
	inner := errors.New("boom")
	got := Wrap("lock", "hackney", inner)
	if got == nil {
		t.Fatal("Wrap with non-nil cause should not return nil")
	}
	var be *BridgeError
	if !errors.As(got, &be) {
		t.Fatal("Wrap should return *BridgeError")
	}
	if be.Phase != "lock" || be.Package != "hackney" {
		t.Errorf("unexpected BridgeError fields: %+v", be)
	}
}
