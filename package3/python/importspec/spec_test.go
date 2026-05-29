package importspec

import (
	"strings"
	"testing"
)

func TestParseEmpty(t *testing.T) {
	if _, err := Parse(""); err == nil {
		t.Fatal("expected error for empty spec")
	}
}

func TestParseLeadingWhitespace(t *testing.T) {
	if _, err := Parse(" requests"); err == nil {
		t.Fatal("expected error for leading whitespace")
	}
}

func TestParseTrailingWhitespace(t *testing.T) {
	if _, err := Parse("requests "); err == nil {
		t.Fatal("expected error for trailing whitespace")
	}
}

func TestParseEmptyQualifier(t *testing.T) {
	if _, err := Parse("requests@"); err == nil {
		t.Fatal("expected error for empty qualifier after '@'")
	}
}

func TestParseBare(t *testing.T) {
	s, err := Parse("requests")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceRegistry {
		t.Errorf("Source = %v, want SourceRegistry", s.Source)
	}
	if s.Name != "requests" || s.RawName != "requests" {
		t.Errorf("Name=%q RawName=%q", s.Name, s.RawName)
	}
	if len(s.Specifier.Clauses) != 0 {
		t.Errorf("Specifier should be empty: %+v", s.Specifier)
	}
}

func TestParseSemverPinned(t *testing.T) {
	s, err := Parse("requests@>=2.0,<3.0")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceRegistry {
		t.Errorf("Source = %v, want SourceRegistry", s.Source)
	}
	if len(s.Specifier.Clauses) != 2 {
		t.Fatalf("clauses = %d, want 2", len(s.Specifier.Clauses))
	}
	if got := s.Specifier.String(); got != ">=2.0, <3.0" {
		t.Errorf("Specifier.String = %q", got)
	}
}

func TestParseCompatRelease(t *testing.T) {
	s, err := Parse("rich@~=13.0")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceRegistry {
		t.Errorf("Source = %v", s.Source)
	}
	if len(s.Specifier.Clauses) != 1 {
		t.Fatalf("clauses = %d, want 1", len(s.Specifier.Clauses))
	}
}

func TestParseExactPin(t *testing.T) {
	s, err := Parse("pydantic@==2.6.1")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceRegistry {
		t.Errorf("Source = %v", s.Source)
	}
	if got := s.Specifier.String(); got != "==2.6.1" {
		t.Errorf("Specifier.String = %q", got)
	}
}

func TestParseInvalidVersion(t *testing.T) {
	if _, err := Parse("requests@notaversion"); err == nil {
		t.Fatal("expected error for invalid version")
	}
}

func TestParseGitWithRev(t *testing.T) {
	s, err := Parse("mypkg@git+https://github.com/user/repo#abc123")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceGit {
		t.Errorf("Source = %v, want SourceGit", s.Source)
	}
	if s.GitURL != "https://github.com/user/repo" {
		t.Errorf("GitURL = %q", s.GitURL)
	}
	if s.GitRev != "abc123" {
		t.Errorf("GitRev = %q", s.GitRev)
	}
}

func TestParseGitWithoutRev(t *testing.T) {
	s, err := Parse("mypkg@git+https://github.com/user/repo")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceGit {
		t.Errorf("Source = %v", s.Source)
	}
	if s.GitRev != "" {
		t.Errorf("GitRev = %q, want empty", s.GitRev)
	}
}

func TestParseGitSSHScheme(t *testing.T) {
	s, err := Parse("mypkg@git+ssh://git@github.com/user/repo.git#main")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceGit {
		t.Errorf("Source = %v", s.Source)
	}
	if s.GitRev != "main" {
		t.Errorf("GitRev = %q", s.GitRev)
	}
}

func TestParseGitEmptyURL(t *testing.T) {
	if _, err := Parse("mypkg@git+"); err == nil {
		t.Fatal("expected error for empty git URL")
	}
}

func TestParseGitEmptyURLBeforeFragment(t *testing.T) {
	if _, err := Parse("mypkg@git+#main"); err == nil {
		t.Fatal("expected error for empty git URL before fragment")
	}
}

func TestParseGitEmptyFragment(t *testing.T) {
	if _, err := Parse("mypkg@git+https://github.com/user/repo#"); err == nil {
		t.Fatal("expected error for empty fragment")
	}
}

func TestParseGitInvalidURL(t *testing.T) {
	if _, err := Parse("mypkg@git+not-a-url"); err == nil {
		t.Fatal("expected error for non-URL git target")
	}
}

func TestParsePath(t *testing.T) {
	s, err := Parse("mypkg@path+../sibling")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourcePath {
		t.Errorf("Source = %v, want SourcePath", s.Source)
	}
	if s.LocalPath != "../sibling" {
		t.Errorf("LocalPath = %q", s.LocalPath)
	}
}

func TestParsePathAbsolute(t *testing.T) {
	s, err := Parse("mypkg@path+/opt/wheel")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.LocalPath != "/opt/wheel" {
		t.Errorf("LocalPath = %q", s.LocalPath)
	}
}

func TestParsePathEmpty(t *testing.T) {
	if _, err := Parse("mypkg@path+"); err == nil {
		t.Fatal("expected error for empty path")
	}
}

func TestParsePathRoot(t *testing.T) {
	if _, err := Parse("mypkg@path+/"); err == nil {
		t.Fatal("expected error for path that resolves to /")
	}
}

func TestParsePathDot(t *testing.T) {
	if _, err := Parse("mypkg@path+./."); err == nil {
		t.Fatal("expected error for path that resolves to .")
	}
}

func TestParseIndex(t *testing.T) {
	s, err := Parse("torch@torch+https://download.pytorch.org/whl/cu121")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceIndex {
		t.Errorf("Source = %v, want SourceIndex", s.Source)
	}
	if s.IndexURL != "https://download.pytorch.org/whl/cu121" {
		t.Errorf("IndexURL = %q", s.IndexURL)
	}
}

func TestParseIndexHTTPScheme(t *testing.T) {
	s, err := Parse("foo@bar+http://example.org/simple")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceIndex {
		t.Errorf("Source = %v", s.Source)
	}
}

func TestParseIndexEmptyName(t *testing.T) {
	if _, err := Parse("torch@+https://example.org"); err == nil {
		t.Fatal("expected error for empty index identifier")
	}
}

func TestParseLocalVersionDistinguishedFromIndex(t *testing.T) {
	// PEP 440 local version: `+` followed by alphanumerics, not a URL scheme.
	s, err := Parse("numpy@==1.26.0+cuda12")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if s.Source != SourceRegistry {
		t.Errorf("Source = %v, want SourceRegistry (local version, not index)", s.Source)
	}
}

func TestParseInvalidNameLeadingDash(t *testing.T) {
	if _, err := Parse("-requests"); err == nil {
		t.Fatal("expected error for leading dash")
	}
}

func TestParseInvalidNameLeadingDot(t *testing.T) {
	if _, err := Parse(".requests"); err == nil {
		t.Fatal("expected error for leading dot")
	}
}

func TestParseInvalidNameLeadingUnderscore(t *testing.T) {
	if _, err := Parse("_requests"); err == nil {
		t.Fatal("expected error for leading underscore")
	}
}

func TestParseInvalidNameTrailingDash(t *testing.T) {
	if _, err := Parse("requests-"); err == nil {
		t.Fatal("expected error for trailing dash")
	}
}

func TestParseInvalidNameSpace(t *testing.T) {
	if _, err := Parse("re quests"); err == nil {
		t.Fatal("expected error for space in name")
	}
}

func TestParseInvalidNameSlash(t *testing.T) {
	if _, err := Parse("foo/bar"); err == nil {
		t.Fatal("expected error for slash in name")
	}
}

func TestParseEmptyName(t *testing.T) {
	if _, err := Parse("@>=2.0"); err == nil {
		t.Fatal("expected error for empty name")
	}
}

func TestParseNameNormalisation(t *testing.T) {
	cases := []struct{ raw, want string }{
		{"Requests", "requests"},
		{"Pillow", "pillow"},
		{"Flask-Login", "flask-login"},
		{"jaraco.classes", "jaraco-classes"},
		{"backports_zoneinfo", "backports-zoneinfo"},
		{"Foo.Bar_baz", "foo-bar-baz"},
		{"my--lib", "my-lib"},
		{"x.._.y", "x-y"},
	}
	for _, tc := range cases {
		s, err := Parse(tc.raw)
		if err != nil {
			t.Errorf("Parse(%q): %v", tc.raw, err)
			continue
		}
		if s.Name != tc.want {
			t.Errorf("Parse(%q).Name = %q, want %q", tc.raw, s.Name, tc.want)
		}
		if s.RawName != tc.raw {
			t.Errorf("Parse(%q).RawName = %q, want %q", tc.raw, s.RawName, tc.raw)
		}
	}
}

func TestParseDigitOnlyNameOK(t *testing.T) {
	if _, err := Parse("123"); err != nil {
		t.Errorf("Parse(123): %v", err)
	}
}

func TestSourceStringCoverage(t *testing.T) {
	cases := []struct {
		s    Source
		want string
	}{
		{SourceRegistry, "registry"},
		{SourceIndex, "index"},
		{SourceGit, "git"},
		{SourcePath, "path"},
		{Source(99), "unknown"},
	}
	for _, tc := range cases {
		if got := tc.s.String(); got != tc.want {
			t.Errorf("Source(%d).String = %q, want %q", tc.s, got, tc.want)
		}
	}
}

func TestSpecStringRoundTripBare(t *testing.T) {
	s, _ := Parse("requests")
	if got := s.String(); got != "requests" {
		t.Errorf("round-trip = %q", got)
	}
}

func TestSpecStringRoundTripSemver(t *testing.T) {
	s, _ := Parse("requests@>=2.0,<3.0")
	// Specifier.String inserts spaces; that is documented.
	if got := s.String(); got != "requests@>=2.0, <3.0" {
		t.Errorf("round-trip = %q", got)
	}
}

func TestSpecStringRoundTripGitWithRev(t *testing.T) {
	in := "mypkg@git+https://github.com/user/repo#abc123"
	s, _ := Parse(in)
	if got := s.String(); got != in {
		t.Errorf("round-trip = %q, want %q", got, in)
	}
}

func TestSpecStringRoundTripGitNoRev(t *testing.T) {
	in := "mypkg@git+https://github.com/user/repo"
	s, _ := Parse(in)
	if got := s.String(); got != in {
		t.Errorf("round-trip = %q, want %q", got, in)
	}
}

func TestSpecStringRoundTripPath(t *testing.T) {
	in := "mypkg@path+../sibling"
	s, _ := Parse(in)
	if got := s.String(); got != in {
		t.Errorf("round-trip = %q, want %q", got, in)
	}
}

func TestSpecStringRoundTripIndex(t *testing.T) {
	in := "torch@torch+https://download.pytorch.org/whl/cu121"
	s, _ := Parse(in)
	if got := s.String(); got != in {
		t.Errorf("round-trip = %q, want %q", got, in)
	}
}

func TestSpecStringUnknownSource(t *testing.T) {
	s := Spec{RawName: "x", Source: Source(99)}
	if got := s.String(); got != "x" {
		t.Errorf("unknown source String = %q", got)
	}
}

func TestIsURLLikeSchemes(t *testing.T) {
	cases := map[string]bool{
		"https://example.org":            true,
		"http://example.org":             true,
		"ssh://git@github.com/foo":       true,
		"git://github.com/foo":           true,
		"file:///opt/wheel":              true,
		"ftp://example.org/pkg":          true,
		"":                               false,
		"cuda12":                         false,
		"not-a-url":                      false,
		"unknown-scheme://example.org/x": false,
		"1.0+local":                      false,
	}
	for in, want := range cases {
		if got := isURLLike(in); got != want {
			t.Errorf("isURLLike(%q) = %v, want %v", in, got, want)
		}
	}
}

func TestParseIndexURLNotRecognised(t *testing.T) {
	// `name@idx+something-without-scheme` is not an index form because the
	// fragment after `+` is not URL-like. We try to interpret the whole rest
	// as a version specifier and reject it as a malformed version.
	if _, err := Parse("foo@bar+baz"); err == nil {
		t.Fatal("expected error: neither a URL nor a valid PEP 440 spec")
	}
}

func TestParseErrorMessagesAreInformative(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"", "empty spec"},
		{" foo", "whitespace"},
		{"foo@", "empty qualifier"},
		{"foo@git+", "empty git URL"},
		{"foo@path+", "empty path"},
		{"foo@+https://example.org", "empty index identifier"},
	}
	for _, tc := range cases {
		_, err := Parse(tc.in)
		if err == nil {
			t.Errorf("Parse(%q): expected error", tc.in)
			continue
		}
		if !strings.Contains(err.Error(), tc.want) {
			t.Errorf("Parse(%q): err = %q, want substring %q", tc.in, err.Error(), tc.want)
		}
	}
}
