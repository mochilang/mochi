package typemap_test

import (
	"testing"

	brerrors "mochi/package3/typescript/errors"
	"mochi/package3/typescript/typemap"
)

// TestPhase5TypeMap is the MEP-72 Phase 5 gate for the typemap package.
func TestPhase5TypeMap(t *testing.T) {
	cases := []struct {
		ts        string
		param     string
		wantType  typemap.MochiType
		wantSkip  bool
		wantReason brerrors.SkipReason
	}{
		{"string", "", typemap.TypeString, false, 0},
		{"boolean", "", typemap.TypeBool, false, 0},
		{"bigint", "", typemap.TypeInt, false, 0},
		{"void", "", typemap.TypeUnit, false, 0},
		{"number", "index", typemap.TypeInt, false, 0},
		{"number", "value", typemap.TypeFloat, false, 0},
		{"any", "", "", true, brerrors.SkipAnyType},
		{"unknown", "", "", true, brerrors.SkipUnknownType},
		{"never", "", "", true, brerrors.SkipNeverType},
		{"T", "", "", true, brerrors.SkipGeneric},
		{"A & B", "", "", true, brerrors.SkipIntersection},
		{"string[]", "", "list", false, 0},
		{"Array<number>", "", "list", false, 0},
		{"Map<string, number>", "", "map", false, 0},
		{"Set<string>", "", "set", false, 0},
		{"string | null", "", typemap.TypeString, false, 0},
		{"Promise<string>", "", typemap.TypeAsync, false, 0},
		{"AsyncIterable<number>", "", typemap.TypeStream, false, 0},
	}

	for _, c := range cases {
		r := typemap.MapType(c.ts, c.param)
		if c.wantSkip {
			if !r.Skipped {
				t.Errorf("MapType(%q, %q): want skipped", c.ts, c.param)
			} else if r.Reason != c.wantReason {
				t.Errorf("MapType(%q, %q): reason = %v, want %v", c.ts, c.param, r.Reason, c.wantReason)
			}
		} else {
			if r.Skipped {
				t.Errorf("MapType(%q, %q): unexpected skip: %v", c.ts, c.param, r.Reason)
			} else if c.wantType != "" {
				got := r.Type
				if c.wantType == "list" || c.wantType == "map" || c.wantType == "set" {
					got = typemap.MochiType(r.Container)
				}
				if got != c.wantType {
					t.Errorf("MapType(%q, %q): type = %q, want %q", c.ts, c.param, got, c.wantType)
				}
			}
		}
	}
}
