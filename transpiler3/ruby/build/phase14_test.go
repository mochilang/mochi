package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase14StringDeep exercises the deep string operators: indexing,
// contains, substring, reverse, upper, lower, split, join, str (to_s).
func TestPhase14StringDeep(t *testing.T) {
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
			name: "str_index",
			src: "let s = \"hello\"\n" +
				"print(s[1])\n" +
				"print(s[4])\n",
			want: "e\no\n",
		},
		{
			name: "str_contains",
			src: "let s = \"hello world\"\n" +
				"print(s.contains(\"world\"))\n" +
				"print(s.contains(\"xyz\"))\n",
			want: "true\nfalse\n",
		},
		{
			name: "str_substring",
			src: "let s = \"hello world\"\n" +
				"print(substring(s, 6, 11))\n",
			want: "world\n",
		},
		{
			name: "str_reverse",
			src: "let s = \"mochi\"\n" +
				"print(reverse(s))\n",
			want: "ihcom\n",
		},
		{
			name: "str_upper_lower",
			src: "let s = \"Mochi\"\n" +
				"print(upper(s))\n" +
				"print(lower(s))\n",
			want: "MOCHI\nmochi\n",
		},
		{
			name: "str_split_join",
			src: "let parts = split(\"a,b,c\", \",\")\n" +
				"print(parts[1])\n" +
				"print(join(parts, \"-\"))\n",
			want: "b\na-b-c\n",
		},
		{
			name: "str_convert",
			src: "let n: int = 42\n" +
				"let s = str(n)\n" +
				"print(s)\n",
			want: "42\n",
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
