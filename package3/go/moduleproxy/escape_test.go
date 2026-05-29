package moduleproxy

import "testing"

func TestEscapePath(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"github.com/spf13/cobra", "github.com/spf13/cobra"},
		{"github.com/Spf13/Cobra", "github.com/!spf13/!cobra"},
		{"k8s.io/api", "k8s.io/api"},
		{"go.uber.org/Zap", "go.uber.org/!zap"},
		{"github.com/AAA/BBB", "github.com/!a!a!a/!b!b!b"},
		{"lower.case/only", "lower.case/only"},
		{"!leading-bang", "!leading-bang"}, // bare ! pass-through in path
	}
	for _, tc := range cases {
		got, err := EscapePath(tc.in)
		if err != nil {
			t.Errorf("EscapePath(%q) returned err = %v; want nil", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("EscapePath(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestEscapePathEmpty(t *testing.T) {
	if _, err := EscapePath(""); err == nil {
		t.Errorf("EscapePath(\"\") returned nil err; want error")
	}
}

func TestEscapeVersion(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"v1.2.3", "v1.2.3"},
		{"v1.2.3-Pre", "v1.2.3-!pre"},
		{"v0.0.0-20210101000000-abcdef123456", "v0.0.0-20210101000000-abcdef123456"},
		{"v1.0.0+incompatible", "v1.0.0+incompatible"},
	}
	for _, tc := range cases {
		got, err := EscapeVersion(tc.in)
		if err != nil {
			t.Errorf("EscapeVersion(%q) err = %v; want nil", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("EscapeVersion(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestEscapeVersionEmpty(t *testing.T) {
	if _, err := EscapeVersion(""); err == nil {
		t.Errorf("EscapeVersion(\"\") returned nil; want error")
	}
}

func TestEscapeVersionForbiddenChars(t *testing.T) {
	cases := []string{
		"v1/2",
		"v1\\2",
		"v1:2",
		"v1\x00",
	}
	for _, tc := range cases {
		if _, err := EscapeVersion(tc); err == nil {
			t.Errorf("EscapeVersion(%q) returned nil; want error", tc)
		}
	}
}

func TestUnescapePath(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"github.com/spf13/cobra", "github.com/spf13/cobra"},
		{"github.com/!spf13/!cobra", "github.com/Spf13/Cobra"},
		{"go.uber.org/!zap", "go.uber.org/Zap"},
		{"github.com/!a!a!a/!b!b!b", "github.com/AAA/BBB"},
	}
	for _, tc := range cases {
		got, err := UnescapePath(tc.in)
		if err != nil {
			t.Errorf("UnescapePath(%q) err = %v; want nil", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("UnescapePath(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestUnescapePathTrailingBang(t *testing.T) {
	if _, err := UnescapePath("foo!"); err == nil {
		t.Errorf("UnescapePath(\"foo!\") returned nil; want error")
	}
}

func TestUnescapePathBangNotLower(t *testing.T) {
	cases := []string{"foo!A", "foo!1", "foo!!"}
	for _, tc := range cases {
		if _, err := UnescapePath(tc); err == nil {
			t.Errorf("UnescapePath(%q) returned nil; want error", tc)
		}
	}
}

func TestUnescapeVersionDelegatesToPath(t *testing.T) {
	got, err := UnescapeVersion("v1.2.3-!pre")
	if err != nil {
		t.Fatalf("UnescapeVersion err: %v", err)
	}
	if got != "v1.2.3-Pre" {
		t.Errorf("UnescapeVersion = %q; want v1.2.3-Pre", got)
	}
}

func TestEscapeUnescapeRoundTrip(t *testing.T) {
	cases := []string{
		"github.com/Spf13/Cobra",
		"go.uber.org/Zap",
		"K8s.io/Api",
		"github.com/lowercase/only",
	}
	for _, tc := range cases {
		esc, err := EscapePath(tc)
		if err != nil {
			t.Fatalf("EscapePath(%q): %v", tc, err)
		}
		got, err := UnescapePath(esc)
		if err != nil {
			t.Fatalf("UnescapePath(%q): %v", esc, err)
		}
		if got != tc {
			t.Errorf("round-trip on %q produced %q (via %q)", tc, got, esc)
		}
	}
}
