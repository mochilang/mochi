package parser

import (
	"strings"
	"testing"
)

// TestPhase8GoImport is the MEP-74 phase 8 sentinel. It exercises
// the full pipeline (lex -> parse -> normalize -> validate) over a
// mixed source covering every shape the phase 8 grammar admits:
//
//   - version-pinned FQDN-style imports with aliases
//   - stdlib FFI imports (no version pin) -- still admitted, no alias required
//   - version-pinned imports with non-trivial paths (gopkg.in/yaml.v3,
//     major-version path /v2, pseudo-versions)
//
// It also asserts that the structural errors (bad module path, missing
// alias, empty version) surface as positioned diagnostics with the
// MEP-74-allocated P067/P068 codes; the golden fixtures under
// tests/parser/errors/import_go_*.err cover the rendered diagnostic
// shape end-to-end.
func TestPhase8GoImport(t *testing.T) {
	t.Run("happy path", func(t *testing.T) {
		src := `import go "github.com/spf13/cobra@v1.8.0" as cobra
import go "gopkg.in/yaml.v3@v3.0.1" as yaml
import go "github.com/foo/bar/v2@v2.1.0" as bar
import go "example.com/x@v0.0.0-20260520150000-abcdef012345" as x
import go "fmt"
import go "net/http" as http
`
		prog, err := ParseString(src)
		if err != nil {
			t.Fatalf("Parse: %v", err)
		}
		if got, want := len(prog.Statements), 6; got != want {
			t.Fatalf("statements = %d; want %d", got, want)
		}
		wantPaths := []string{
			"github.com/spf13/cobra@v1.8.0",
			"gopkg.in/yaml.v3@v3.0.1",
			"github.com/foo/bar/v2@v2.1.0",
			"example.com/x@v0.0.0-20260520150000-abcdef012345",
			"fmt",
			"net/http",
		}
		for i, s := range prog.Statements {
			if s.Import == nil {
				t.Fatalf("stmt %d not import: %+v", i, s)
			}
			if s.Import.Path != wantPaths[i] {
				t.Errorf("stmt %d path = %q; want %q", i, s.Import.Path, wantPaths[i])
			}
			if s.Import.Lang == nil || *s.Import.Lang != "go" {
				t.Errorf("stmt %d lang != go: %+v", i, s.Import.Lang)
			}
		}

		// Cross-check GoImportRef against the pinned imports.
		mod, ver, ok := GoImportRef(prog.Statements[0].Import.Path)
		if !ok || mod != "github.com/spf13/cobra" || ver != "v1.8.0" {
			t.Errorf("split cobra = (%q, %q, %v)", mod, ver, ok)
		}
		mod, ver, ok = GoImportRef(prog.Statements[3].Import.Path)
		if !ok || mod != "example.com/x" || !strings.HasPrefix(ver, "v0.0.0-20260520150000-") {
			t.Errorf("split pseudo = (%q, %q, %v)", mod, ver, ok)
		}

		// Stdlib imports report no pin.
		if HasGoSemverPin(prog.Statements[4].Import.Path) {
			t.Errorf("stdlib fmt should report no pin")
		}
		if HasGoSemverPin(prog.Statements[5].Import.Path) {
			t.Errorf("stdlib net/http should report no pin")
		}
	})

	t.Run("rejects pinned import without alias", func(t *testing.T) {
		_, err := ParseString(`import go "github.com/spf13/cobra@v1.8.0"`)
		if err == nil {
			t.Fatalf("expected error; got nil")
		}
		if !strings.Contains(err.Error(), "P068") {
			t.Errorf("err missing P068 code: %v", err)
		}
	})

	t.Run("rejects malformed module path", func(t *testing.T) {
		_, err := ParseString(`import go "foo/bar@v1.0.0" as foo`)
		if err == nil {
			t.Fatalf("expected error; got nil")
		}
		if !strings.Contains(err.Error(), "P067") {
			t.Errorf("err missing P067 code: %v", err)
		}
	})

	t.Run("rejects empty version", func(t *testing.T) {
		_, err := ParseString(`import go "github.com/spf13/cobra@" as cobra`)
		if err == nil {
			t.Fatalf("expected error; got nil")
		}
		if !strings.Contains(err.Error(), "P067") {
			t.Errorf("err missing P067 code: %v", err)
		}
	})

	t.Run("rejects consecutive dots in module", func(t *testing.T) {
		_, err := ParseString(`import go "github..com/foo@v1.0.0" as foo`)
		if err == nil {
			t.Fatalf("expected error")
		}
	})

	t.Run("accepts stdlib without alias", func(t *testing.T) {
		_, err := ParseString(`import go "fmt"`)
		if err != nil {
			t.Errorf("stdlib without alias should parse: %v", err)
		}
	})

	t.Run("accepts stdlib with alias", func(t *testing.T) {
		_, err := ParseString(`import go "net/http" as http`)
		if err != nil {
			t.Errorf("stdlib with alias should parse: %v", err)
		}
	})
}
