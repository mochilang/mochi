package parser

import "testing"

func TestGoImportRef(t *testing.T) {
	cases := []struct {
		name            string
		in              string
		wantMod, wantV  string
		wantOK          bool
	}{
		{"plain cobra", `github.com/spf13/cobra@v1.8.0`, "github.com/spf13/cobra", "v1.8.0", true},
		{"quoted cobra", `"github.com/spf13/cobra@v1.8.0"`, "github.com/spf13/cobra", "v1.8.0", true},
		{"yaml v3", `gopkg.in/yaml.v3@v3.0.1`, "gopkg.in/yaml.v3", "v3.0.1", true},
		{"x tools", `golang.org/x/tools@v0.20.0`, "golang.org/x/tools", "v0.20.0", true},
		{"major v2 path", `github.com/foo/bar/v2@v2.1.0`, "github.com/foo/bar/v2", "v2.1.0", true},
		{"pseudo version", `github.com/foo/bar@v0.0.0-20260520150000-abcdef012345`, "github.com/foo/bar", "v0.0.0-20260520150000-abcdef012345", true},
		{"semver pre-release", `example.com/x@v1.2.3-rc.1`, "example.com/x", "v1.2.3-rc.1", true},
		{"semver build meta", `example.com/x@v1.2.3+build.5`, "example.com/x", "v1.2.3+build.5", true},
		{"single dotted segment", `example.com@v1.0.0`, "example.com", "v1.0.0", true},
		{"hyphenated segment", `github.com/go-yaml/yaml@v2.4.0`, "github.com/go-yaml/yaml", "v2.4.0", true},

		// Stdlib-style and pre-MEP-74 FFI imports return ok=false but
		// don't error (caller distinguishes via HasGoSemverPin).
		{"no version (stdlib)", `fmt`, "", "", false},
		{"no version path-only", `net/http`, "", "", false},
		// Structural errors.
		{"empty version", `github.com/foo/bar@`, "", "", false},
		{"empty module", `@v1.0.0`, "", "", false},
		{"missing dot in first seg", `foo/bar@v1.0.0`, "", "", false},
		{"empty segment", `github.com//bar@v1.0.0`, "", "", false},
		{"underscore start segment", `github.com/_bar@v1.0.0`, "", "", false},
		{"dot start segment", `github.com/.bar@v1.0.0`, "", "", false},
		{"hyphen start segment", `github.com/-bar@v1.0.0`, "", "", false},
		{"consecutive dots", `github..com/foo@v1.0.0`, "", "", false},
		{"plus in path", `github.com/foo+bar@v1.0.0`, "", "", false},
		{"whitespace in version", `github.com/foo/bar@v1.0.0 alpha`, "", "", false},
		{"newline in version", "github.com/foo/bar@v1.0.0\n", "", "", false},
		{"empty", ``, "", "", false},
		{"only at", `@`, "", "", false},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			gotM, gotV, ok := GoImportRef(c.in)
			if ok != c.wantOK {
				t.Fatalf("GoImportRef(%q) ok = %v; want %v", c.in, ok, c.wantOK)
			}
			if !ok {
				return
			}
			if gotM != c.wantMod || gotV != c.wantV {
				t.Errorf("GoImportRef(%q) = (%q, %q); want (%q, %q)", c.in, gotM, gotV, c.wantMod, c.wantV)
			}
		})
	}
}

func TestHasGoSemverPin(t *testing.T) {
	cases := map[string]bool{
		`github.com/spf13/cobra@v1.8.0`:   true,
		`"github.com/spf13/cobra@v1.8.0"`: true,
		`fmt`:                             false,
		`net/http`:                        false,
		`@`:                               true, // structurally invalid but carries `@`
	}
	for in, want := range cases {
		if got := HasGoSemverPin(in); got != want {
			t.Errorf("HasGoSemverPin(%q) = %v; want %v", in, got, want)
		}
	}
}

func TestIsGoModulePath(t *testing.T) {
	cases := map[string]bool{
		"github.com/spf13/cobra":         true,
		"gopkg.in/yaml.v3":               true,
		"golang.org/x/tools":             true,
		"github.com/foo/bar/v2":          true,
		"example.com":                    true,
		"github.com/go-yaml/yaml":        true,
		"github.com/foo_bar/baz":         true,
		"github.com/foo.bar/baz":         true,
		"github.com/foo~bar/baz":         true,
		"fmt":                            false,
		"net/http":                       false,
		"":                               false,
		"github..com/foo":                false,
		"github.com/":                    false,
		"github.com//foo":                false,
		"github.com/_foo":                false,
		"github.com/-foo":                false,
		"github.com/foo bar":             false,
		"github.com/foo+bar":             false,
	}
	for in, want := range cases {
		if got := isGoModulePath(in); got != want {
			t.Errorf("isGoModulePath(%q) = %v; want %v", in, got, want)
		}
	}
}
