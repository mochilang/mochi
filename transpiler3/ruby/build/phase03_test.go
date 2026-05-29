package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase3Lists exercises list literal, index, len, append, for-each,
// and list[i] = v.
func TestPhase3Lists(t *testing.T) {
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
			name: "list_lit_index",
			src: "let xs = [10, 20, 30]\n" +
				"print(xs[0])\n" +
				"print(xs[2])\n",
			want: "10\n30\n",
		},
		{
			name: "list_len",
			src: "let xs = [1, 2, 3, 4]\n" +
				"print(len(xs))\n",
			want: "4\n",
		},
		{
			name: "for_each",
			src: "let xs = [5, 7, 11]\n" +
				"for x in xs {\n" +
				"  print(x)\n" +
				"}\n",
			want: "5\n7\n11\n",
		},
		{
			name: "list_set",
			src: "var xs = [1, 2, 3]\n" +
				"xs[1] = 99\n" +
				"print(xs[1])\n",
			want: "99\n",
		},
		{
			name: "list_append",
			src: "var xs = [1, 2]\n" +
				"xs = append(xs, 3)\n" +
				"print(xs[2])\n" +
				"print(len(xs))\n",
			want: "3\n3\n",
		},
		{
			name: "str_len",
			src: "let s = \"hello\"\n" +
				"print(len(s))\n",
			want: "5\n",
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
				t.Fatalf("ruby run failed: %v\noutput: %s\nsrc:\n%s", err, out, readFile(t, rb))
			}
			got := string(out)
			if got != c.want {
				t.Fatalf("%s output mismatch:\ngot:\n%s\nwant:\n%s\nrb source:\n%s",
					c.name, got, c.want, readFile(t, rb))
			}
		})
	}
}
