package errors_test

import (
	"strings"
	"testing"

	brerrors "mochi/package3/typescript/errors"
)

// TestPhase0Errors is the MEP-72 Phase 0 gate for the errors package.
func TestPhase0Errors(t *testing.T) {
	// All skip reasons have non-empty String() output.
	reasons := []brerrors.SkipReason{
		brerrors.SkipUnknown,
		brerrors.SkipGeneric,
		brerrors.SkipConditional,
		brerrors.SkipMappedType,
		brerrors.SkipTemplateLiteral,
		brerrors.SkipIntersection,
		brerrors.SkipSymbol,
		brerrors.SkipUnknownType,
		brerrors.SkipAnyType,
		brerrors.SkipNeverType,
		brerrors.SkipFunction,
		brerrors.SkipInternal,
		brerrors.SkipDeprecated,
		brerrors.SkipBrowser,
		brerrors.SkipNodeBuiltin,
	}
	for _, r := range reasons {
		s := r.String()
		if s == "" {
			t.Errorf("SkipReason(%d).String() is empty", r)
		}
		if strings.HasPrefix(s, "SkipReason(") {
			t.Errorf("SkipReason(%d) has no name: %s", r, s)
		}
	}

	// BridgeError.
	err := &brerrors.BridgeError{Phase: "ingest", Item: "lodash", Message: "parse failed"}
	msg := err.Error()
	if !strings.Contains(msg, "ts bridge") {
		t.Errorf("BridgeError.Error() = %q, missing 'ts bridge'", msg)
	}
	if !strings.Contains(msg, "ingest") {
		t.Errorf("BridgeError.Error() = %q, missing phase", msg)
	}

	// SkipReport.
	sr := brerrors.SkipReport{ItemPath: "myFunc", Reason: brerrors.SkipGeneric, Detail: "T"}
	if sr.ItemPath != "myFunc" {
		t.Error("SkipReport.ItemPath not set")
	}
}
