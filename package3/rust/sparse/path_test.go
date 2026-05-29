package sparse

import "testing"

func TestBucketPath(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"a", "1/a"},
		{"A", "1/a"},
		{"ab", "2/ab"},
		{"AB", "2/ab"},
		{"abc", "3/a/abc"},
		{"ABC", "3/a/abc"},
		{"abcd", "ab/cd/abcd"},
		{"ABCD", "ab/cd/abcd"},
		{"serde", "se/rd/serde"},
		{"tokio", "to/ki/tokio"},
		{"anyhow", "an/yh/anyhow"},
		{"thiserror", "th/is/thiserror"},
		{"clap", "cl/ap/clap"},
		{"libc", "li/bc/libc"},
		// Hyphen and underscore are preserved literally in the path.
		{"foo-bar", "fo/o-/foo-bar"},
		{"foo_bar", "fo/o_/foo_bar"},
		{"x86_64-linux", "x8/6_/x86_64-linux"},
	}
	for _, tc := range cases {
		got, err := BucketPath(tc.in)
		if err != nil {
			t.Errorf("BucketPath(%q) err = %v", tc.in, err)
			continue
		}
		if got != tc.want {
			t.Errorf("BucketPath(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestBucketPathInvalid(t *testing.T) {
	cases := []string{
		"",
		"-leading-dash",
		"0starts-with-digit",
		"has space",
		"has/slash",
		"has\x00null",
	}
	for _, c := range cases {
		if _, err := BucketPath(c); err == nil {
			t.Errorf("BucketPath(%q) err = nil; want error", c)
		}
	}
}

func TestBucketPathLongName(t *testing.T) {
	long := ""
	for i := 0; i < 65; i++ {
		long += "a"
	}
	if _, err := BucketPath(long); err == nil {
		t.Errorf("BucketPath(65-char name) err = nil; want error")
	}
}

func TestValidateCrateName(t *testing.T) {
	good := []string{"a", "A", "_x", "serde", "foo-bar", "foo_bar", "x86_64-linux", "rust-1-2-3"}
	for _, n := range good {
		if err := ValidateCrateName(n); err != nil {
			t.Errorf("ValidateCrateName(%q) = %v; want nil", n, err)
		}
	}
	bad := []string{"", "-leading", "1digit-first", "has space", "has.dot", "has/slash"}
	for _, n := range bad {
		if err := ValidateCrateName(n); err == nil {
			t.Errorf("ValidateCrateName(%q) = nil; want error", n)
		}
	}
}

func TestCrateNameEqual(t *testing.T) {
	cases := []struct {
		a, b string
		want bool
	}{
		{"serde", "serde", true},
		{"serde", "SERDE", true},
		{"foo-bar", "foo_bar", true},
		{"foo-bar", "Foo_Bar", true},
		{"openssl-sys", "OPENSSL_SYS", true},
		{"foo", "bar", false},
		{"foo", "foo-bar", false},
	}
	for _, tc := range cases {
		if got := CrateNameEqual(tc.a, tc.b); got != tc.want {
			t.Errorf("CrateNameEqual(%q, %q) = %v; want %v", tc.a, tc.b, got, tc.want)
		}
	}
}
