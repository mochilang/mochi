package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase4Records exercises record declarations, construction, and
// field access. Records lower to Ruby's Data.define.
func TestPhase4Records(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	cases := []struct {
		name     string
		src      string
		want     string
		wantInRb []string // substrings the rendered .rb must contain
	}{
		{
			name: "record_decl_access",
			src: "type User {\n" +
				"  id: int\n" +
				"  name: string\n" +
				"}\n" +
				"let u: User = User { id: 7, name: \"Mochi\" }\n" +
				"print(u.id)\n" +
				"print(u.name)\n",
			want: "7\nMochi\n",
			wantInRb: []string{
				"User = Data.define(:id, :name)",
				"User.new(id: 7, name: \"Mochi\")",
				"u.id",
				"u.name",
			},
		},
		{
			name: "record_equality",
			src: "type Pair {\n" +
				"  a: int\n" +
				"  b: int\n" +
				"}\n" +
				"let p1: Pair = Pair { a: 1, b: 2 }\n" +
				"let p2: Pair = Pair { a: 1, b: 2 }\n" +
				"let r = p1 == p2\n" +
				"print(r)\n",
			want: "true\n",
		},
	}

	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			srcDir := t.TempDir()
			src := filepath.Join(srcDir, c.name+".mochi")
			if err := os.WriteFile(src, []byte(c.src), 0o644); err != nil {
				t.Fatalf("write src: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{}
			if err := d.Build(src, outDir, TargetRubySource); err != nil {
				t.Fatalf("Build: %v\nsrc:\n%s", err, c.src)
			}
			rb := filepath.Join(outDir, c.name+".rb")
			rbSource := readFile(t, rb)
			for _, want := range c.wantInRb {
				if !strings.Contains(rbSource, want) {
					t.Errorf("missing %q in rb source:\n%s", want, rbSource)
				}
			}
			cmd := exec.Command(tc.Ruby, "-I", runtimeLib, rb)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run failed: %v\noutput: %s\nrb source:\n%s", err, out, rbSource)
			}
			got := string(out)
			if got != c.want {
				t.Fatalf("%s output mismatch:\ngot:\n%s\nwant:\n%s\nrb source:\n%s",
					c.name, got, c.want, rbSource)
			}
		})
	}
}
