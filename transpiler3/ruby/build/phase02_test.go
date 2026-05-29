package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase2Scalars builds and runs fixtures that exercise scalar let
// bindings + arithmetic + comparison + print.
func TestPhase2Scalars(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	cases := []struct {
		name    string
		fixture string
		want    string
	}{
		{
			name:    "let",
			fixture: filepath.Join(repoRoot, "examples", "v0.1", "let.mochi"),
			want:    "42\nMochi\n",
		},
	}

	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(c.fixture, outDir, TargetRubySource); err != nil {
				t.Fatalf("Build %s: %v", c.fixture, err)
			}
			rbName := strings.TrimSuffix(filepath.Base(c.fixture), ".mochi") + ".rb"
			rb := filepath.Join(outDir, rbName)
			if _, err := os.Stat(rb); err != nil {
				t.Fatalf("expected emitted %s: %v", rb, err)
			}
			cmd := exec.Command(tc.Ruby, "-I", runtimeLib, rb)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run %s failed: %v\noutput: %s", rb, err, out)
			}
			got := string(out)
			if got != c.want {
				t.Fatalf("%s output mismatch:\ngot:\n%s\nwant:\n%s", c.name, got, c.want)
			}
		})
	}
}

// TestPhase2InlineFixtures exercises scalar arithmetic via inline-written
// Mochi sources. Avoids relying on examples/v0.1/binary.mochi which uses
// multi-arg print (rejected by the C lowerer).
func TestPhase2InlineFixtures(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			// Mochi groups binary ops left-to-right with no operator precedence,
			// so `a + b * c` parses as `(a + b) * c`. Tests use parens to be explicit.
			name: "arith_int",
			src: "let a = 3 + (4 * 2)\n" +
				"let b = (3 + 4) * 2\n" +
				"let c = 10 - ((2 * 3) - 4)\n" +
				"print(a)\n" +
				"print(b)\n" +
				"print(c)\n",
			want: "11\n14\n8\n",
		},
		{
			name: "arith_float",
			src: "let e = 5.0 + (2.5 * 2.0)\n" +
				"print(e)\n",
			want: "10.0\n",
		},
		{
			name: "compare_int",
			src: "let lt = 3 < 5\n" +
				"let eq = 10 == 10\n" +
				"print(lt)\n" +
				"print(eq)\n",
			want: "true\ntrue\n",
		},
		{
			name: "compare_string",
			src: "let a = \"hello\"\n" +
				"let b = \"hello\"\n" +
				"let r = a == b\n" +
				"print(r)\n",
			want: "true\n",
		},
		{
			name: "unary",
			src: "let n = 5\n" +
				"let m = -n\n" +
				"print(m)\n",
			want: "-5\n",
		},
		{
			name: "string_concat",
			src: "let s = \"Hello, \" + \"world\"\n" +
				"print(s)\n",
			want: "Hello, world\n",
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
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outDir, TargetRubySource); err != nil {
				t.Fatalf("Build: %v", err)
			}

			rb := filepath.Join(outDir, c.name+".rb")
			cmd := exec.Command(tc.Ruby, "-I", runtimeLib, rb)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run failed: %v\noutput: %s", err, out)
			}
			got := string(out)
			if got != c.want {
				t.Fatalf("%s output mismatch:\ngot:\n%s\nwant:\n%s\nrb source:\n%s",
					c.name, got, c.want, readFile(t, rb))
			}
		})
	}
}

func readFile(t *testing.T, p string) string {
	t.Helper()
	b, err := os.ReadFile(p)
	if err != nil {
		return "(read failed: " + err.Error() + ")"
	}
	return string(b)
}
