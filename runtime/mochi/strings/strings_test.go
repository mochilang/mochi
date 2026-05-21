package strings

import (
	"fmt"
	"testing"
)

func TestUpperLower(t *testing.T) {
	if got := Upper("héllo"); got != "HÉLLO" {
		t.Errorf("Upper = %q", got)
	}
	if got := Lower("HÉLLO"); got != "héllo" {
		t.Errorf("Lower = %q", got)
	}
}

func TestReverseUnicode(t *testing.T) {
	cases := []struct{ in, out string }{
		{"", ""},
		{"a", "a"},
		{"abc", "cba"},
		{"héllo", "olléh"},
		{"日本語", "語本日"},
	}
	for _, c := range cases {
		if got := Reverse(c.in); got != c.out {
			t.Errorf("Reverse(%q) = %q, want %q", c.in, got, c.out)
		}
	}
}

func TestContainsIndexOf(t *testing.T) {
	if !Contains("hello world", "world") {
		t.Error("Contains world")
	}
	if Contains("hello world", "WORLD") {
		t.Error("Contains case sensitive expected false")
	}
	if got := IndexOf("hello world", "world"); got != 6 {
		t.Errorf("IndexOf = %d", got)
	}
	if got := IndexOf("hello world", "xyz"); got != -1 {
		t.Errorf("IndexOf missing = %d", got)
	}
}

func TestSplitJoin(t *testing.T) {
	parts := Split("a,b,c", ",")
	if len(parts) != 3 || parts[0] != "a" || parts[2] != "c" {
		t.Errorf("Split = %v", parts)
	}
	if got := Join(parts, "-"); got != "a-b-c" {
		t.Errorf("Join = %q", got)
	}
}

func TestReplace(t *testing.T) {
	if got := Replace("aaa", "a", "b"); got != "bbb" {
		t.Errorf("Replace all = %q", got)
	}
}

func TestTrimSpace(t *testing.T) {
	if got := TrimSpace("  hi  "); got != "hi" {
		t.Errorf("TrimSpace = %q", got)
	}
}

func TestSubstr(t *testing.T) {
	cases := []struct {
		in         string
		start, end int
		out        string
	}{
		{"hello", 0, 5, "hello"},
		{"hello", 1, 4, "ell"},
		{"hello", 3, 1, ""}, // inverted range
		{"hello", -2, 3, "hel"},
		{"hello", 0, 99, "hello"},
		{"日本語", 0, 2, "日本"},
	}
	for _, c := range cases {
		if got := Substr(c.in, c.start, c.end); got != c.out {
			t.Errorf("Substr(%q,%d,%d) = %q, want %q", c.in, c.start, c.end, got, c.out)
		}
	}
}

func TestHasPrefixSuffix(t *testing.T) {
	if !HasPrefix("hello.go", "hello") {
		t.Error("HasPrefix")
	}
	if !HasSuffix("hello.go", ".go") {
		t.Error("HasSuffix")
	}
}

func TestIsWhitespace(t *testing.T) {
	for _, c := range []struct {
		in   string
		want bool
	}{
		{"", true},
		{"   ", true},
		{"\t\n", true},
		{" a ", false},
	} {
		if got := IsWhitespace(c.in); got != c.want {
			t.Errorf("IsWhitespace(%q) = %v", c.in, got)
		}
	}
}

func ExampleUpper() {
	fmt.Println(Upper("hello"))
	// Output: HELLO
}

func ExampleReverse() {
	fmt.Println(Reverse("héllo"))
	// Output: olléh
}

func ExampleSubstr() {
	fmt.Println(Substr("hello, world", 7, 12))
	// Output: world
}
