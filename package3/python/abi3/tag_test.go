package abi3

import (
	"strings"
	"testing"
)

func TestParseWheelTagOK(t *testing.T) {
	tag, err := ParseWheelTag("cp312-cp312-manylinux_2_28_x86_64")
	if err != nil {
		t.Fatalf("ParseWheelTag: %v", err)
	}
	if tag.Interpreter != "cp312" || tag.ABI != "cp312" || tag.Platform != "manylinux_2_28_x86_64" {
		t.Fatalf("ParseWheelTag fields wrong: %+v", tag)
	}
}

func TestParseWheelTagRejectsWrongFieldCount(t *testing.T) {
	cases := []string{"", "cp312", "cp312-cp312", "cp312-cp312-platform-extra"}
	for _, c := range cases {
		if _, err := ParseWheelTag(c); err == nil {
			t.Errorf("ParseWheelTag(%q) should error", c)
		}
	}
}

func TestParseWheelTagRejectsEmptyField(t *testing.T) {
	if _, err := ParseWheelTag("cp312--manylinux_2_28_x86_64"); err == nil {
		t.Fatal("ParseWheelTag should reject empty abi field")
	}
}

func TestWheelTagStringRoundTrip(t *testing.T) {
	in := "cp312-abi3-manylinux_2_28_x86_64"
	tag, err := ParseWheelTag(in)
	if err != nil {
		t.Fatalf("ParseWheelTag: %v", err)
	}
	if tag.String() != in {
		t.Fatalf("round-trip: got %q, want %q", tag.String(), in)
	}
}

func TestWheelTagIsABI3(t *testing.T) {
	cases := map[string]bool{
		"cp312-abi3-manylinux_2_28_x86_64":  true,
		"cp312-cp312-manylinux_2_28_x86_64": false,
		"py3-none-any":                      false,
	}
	for s, want := range cases {
		tag, err := ParseWheelTag(s)
		if err != nil {
			t.Fatalf("ParseWheelTag(%q): %v", s, err)
		}
		if got := tag.IsABI3(); got != want {
			t.Errorf("IsABI3(%q) = %v, want %v", s, got, want)
		}
	}
}

func TestToABI3OK(t *testing.T) {
	tag, err := ParseWheelTag("cp312-cp312-manylinux_2_28_x86_64")
	if err != nil {
		t.Fatalf("ParseWheelTag: %v", err)
	}
	out, err := ToABI3(tag)
	if err != nil {
		t.Fatalf("ToABI3: %v", err)
	}
	if out.Interpreter != "cp32" || out.ABI != "abi3" || out.Platform != tag.Platform {
		t.Fatalf("ToABI3 result wrong: %+v", out)
	}
}

func TestToABI3RejectsNonCPython(t *testing.T) {
	tag := WheelTag{Interpreter: "pp310", ABI: "pypy310_pp73", Platform: "manylinux_2_28_x86_64"}
	if _, err := ToABI3(tag); err == nil || !strings.Contains(err.Error(), "CPython") {
		t.Fatalf("ToABI3 should reject PyPy, got %v", err)
	}
}

func TestToABI3RejectsBadInterpreter(t *testing.T) {
	tag := WheelTag{Interpreter: "cpfoo", ABI: "cpfoo", Platform: "any"}
	if _, err := ToABI3(tag); err == nil {
		t.Fatal("ToABI3 should reject non-numeric interpreter")
	}
}

func TestSplitWheelFilenameOK(t *testing.T) {
	prefix, tag, err := SplitWheelFilename("mochi-alpha-0.2.0-cp312-cp312-manylinux_2_28_x86_64.whl")
	if err != nil {
		t.Fatalf("SplitWheelFilename: %v", err)
	}
	if prefix != "mochi-alpha-0.2.0" {
		t.Errorf("prefix = %q", prefix)
	}
	if tag.String() != "cp312-cp312-manylinux_2_28_x86_64" {
		t.Errorf("tag = %q", tag.String())
	}
}

func TestSplitWheelFilenameRejectsNonWhl(t *testing.T) {
	if _, _, err := SplitWheelFilename("foo.tar.gz"); err == nil {
		t.Fatal("SplitWheelFilename should reject non-.whl")
	}
}

func TestSplitWheelFilenameRejectsTooFewFields(t *testing.T) {
	if _, _, err := SplitWheelFilename("foo-bar-baz.whl"); err == nil {
		t.Fatal("SplitWheelFilename should reject too-few-fields")
	}
}
