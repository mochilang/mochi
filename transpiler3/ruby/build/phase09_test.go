package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase9Datalog exercises Mochi `fact` / `rule` / `query` declarations
// lowered to a precomputed Ruby array (the datalog program is evaluated at
// transpile time).
func TestPhase9Datalog(t *testing.T) {
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
			name: "dl_parent_basic",
			src: "fact parent(\"tom\", \"bob\")\n" +
				"fact parent(\"bob\", \"ann\")\n" +
				"let xs = query parent(\"tom\", Y)\n" +
				"for x in xs {\n" +
				"  print(x)\n" +
				"}\n",
			want: "bob\n",
		},
		{
			name: "dl_ancestor",
			src: "fact parent(\"tom\", \"bob\")\n" +
				"fact parent(\"bob\", \"ann\")\n" +
				"fact parent(\"ann\", \"pat\")\n" +
				"rule ancestor(X, Y) :- parent(X, Y)\n" +
				"rule ancestor(X, Y) :- ancestor(X, Z), parent(Z, Y)\n" +
				"let xs = query ancestor(\"tom\", Y)\n" +
				"for x in xs {\n" +
				"  print(x)\n" +
				"}\n",
			want: "bob\nann\npat\n",
		},
		{
			name: "dl_empty_result",
			src: "fact parent(\"tom\", \"bob\")\n" +
				"let xs = query parent(\"missing\", Y)\n" +
				"print(len(xs))\n",
			want: "0\n",
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
				t.Fatalf("Build: %v\nsrc:\n%s", err, c.src)
			}
			rb := filepath.Join(outDir, c.name+".rb")
			cmd := exec.Command(tc.Ruby, "-I", runtimeLib, rb)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("ruby run failed: %v\noutput: %s\nrb source:\n%s", err, out, readFile(t, rb))
			}
			got := string(out)
			if got != c.want {
				t.Fatalf("%s output mismatch:\ngot:\n%s\nwant:\n%s\nrb source:\n%s",
					c.name, got, c.want, readFile(t, rb))
			}
		})
	}
}
