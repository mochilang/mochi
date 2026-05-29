package freethread

import (
	"strings"
	"testing"
)

func TestParseABITagHappy(t *testing.T) {
	cases := []struct {
		raw          string
		major, minor int
		free         bool
	}{
		{"cp312", 3, 12, false},
		{"cp313", 3, 13, false},
		{"cp313t", 3, 13, true},
		{"cp314", 3, 14, false},
		{"cp314t", 3, 14, true},
		{"cp315t", 3, 15, true},
	}
	for _, tc := range cases {
		tag, err := ParseABITag(tc.raw)
		if err != nil {
			t.Errorf("ParseABITag(%q) error: %v", tc.raw, err)
			continue
		}
		if tag.Major != tc.major || tag.Minor != tc.minor || tag.FreeThreaded != tc.free {
			t.Errorf("ParseABITag(%q) = %+v, want major=%d minor=%d free=%v", tc.raw, tag, tc.major, tc.minor, tc.free)
		}
		if got := tag.String(); got != tc.raw {
			t.Errorf("Tag.String() = %q, want %q", got, tc.raw)
		}
	}
}

func TestParseABITagErrors(t *testing.T) {
	cases := []struct {
		raw  string
		want string
	}{
		{"pp310", "only cp"},
		{"cp", "missing version"},
		{"cp1", "version must be"},
		{"cpXY", "bad major"},
		{"cp3x", "bad minor"},
		{"cp312t", "Python >=3.13"},
		{"cp310t", "Python >=3.13"},
		{"cp013", "bad major"},
	}
	for _, tc := range cases {
		_, err := ParseABITag(tc.raw)
		if err == nil {
			t.Errorf("ParseABITag(%q): expected error", tc.raw)
			continue
		}
		if !strings.Contains(err.Error(), tc.want) {
			t.Errorf("ParseABITag(%q) error %q does not contain %q", tc.raw, err.Error(), tc.want)
		}
	}
}

func TestABITagCompatible(t *testing.T) {
	target313 := ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: false}
	target313t := ABITag{Interpreter: "cp", Major: 3, Minor: 13, FreeThreaded: true}

	cp313, _ := ParseABITag("cp313")
	cp313t, _ := ParseABITag("cp313t")
	cp314, _ := ParseABITag("cp314")

	if !target313.Compatible(cp313) {
		t.Error("cp313 -> cp313 should be compatible")
	}
	if target313.Compatible(cp313t) {
		t.Error("cp313 must NOT accept cp313t (refcount semantics differ)")
	}
	if target313.Compatible(cp314) {
		t.Error("cp313 must NOT accept cp314 (minor mismatch)")
	}

	if !target313t.Compatible(cp313t) {
		t.Error("cp313t -> cp313t should be compatible")
	}
	if target313t.Compatible(cp313) {
		t.Error("cp313t must NOT accept cp313 by default (resolver gates via SupportLevel)")
	}

	// pure-python ("" interpreter) accepted on either side.
	pure := ABITag{}
	if !target313.Compatible(pure) {
		t.Error("pure should be compatible with cp313")
	}
	if !target313t.Compatible(pure) {
		t.Error("pure should be compatible with cp313t")
	}

	// Cross-interpreter (pp vs cp) fails.
	pp := ABITag{Interpreter: "pp", Major: 3, Minor: 13}
	if target313.Compatible(pp) {
		t.Error("cp313 should not accept pp313")
	}
}

func TestABITagStringEmpty(t *testing.T) {
	if got := (ABITag{}).String(); got != "" {
		t.Errorf("empty Tag.String() = %q, want empty", got)
	}
}
