package errors_test

import (
	"testing"

	swifterrors "mochi/package3/swift/errors"
)

func TestSkipReasonString(t *testing.T) {
	cases := []struct {
		reason swifterrors.SkipReason
		want   string
	}{
		{swifterrors.SkipUnknown, "SkipUnknown"},
		{swifterrors.SkipGeneric, "SkipGeneric"},
		{swifterrors.SkipOpaque, "SkipOpaque"},
		{swifterrors.SkipExistential, "SkipExistential"},
		{swifterrors.SkipUnsafePointer, "SkipUnsafePointer"},
		{swifterrors.SkipClosure, "SkipClosure"},
		{swifterrors.SkipActor, "SkipActor"},
		{swifterrors.SkipMacro, "SkipMacro"},
		{swifterrors.SkipInternalAccess, "SkipInternalAccess"},
		{swifterrors.SkipAbstract, "SkipAbstract"},
		{swifterrors.SkipNSObject, "SkipNSObject"},
		{swifterrors.SkipConcurrencyType, "SkipConcurrencyType"},
	}
	for _, tc := range cases {
		if got := tc.reason.String(); got != tc.want {
			t.Errorf("SkipReason(%d).String() = %q, want %q", int(tc.reason), got, tc.want)
		}
	}
}

func TestBridgeError(t *testing.T) {
	e := &swifterrors.BridgeError{Phase: "ingest", Item: "MyModule.foo", Message: "unsupported type"}
	got := e.Error()
	want := "swift bridge [ingest] MyModule.foo: unsupported type"
	if got != want {
		t.Errorf("BridgeError.Error() = %q, want %q", got, want)
	}
}
