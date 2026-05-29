package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase15ListAggHOF exercises list aggregates (min, max, sum, in) and
// higher-order list ops (map, filter, reduce).
func TestPhase15ListAggHOF(t *testing.T) {
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
			name: "list_min_max_sum",
			src: "let xs = [3, 1, 4, 1, 5, 9, 2, 6]\n" +
				"print(min(xs))\n" +
				"print(max(xs))\n" +
				"print(sum(xs))\n",
			want: "1\n9\n31\n",
		},
		{
			name: "list_in",
			src: "let xs = [10, 20, 30]\n" +
				"print(20 in xs)\n" +
				"print(99 in xs)\n",
			want: "true\nfalse\n",
		},
		{
			name: "list_map",
			src: "fun dbl(n: int): int {\n" +
				"  return n * 2\n" +
				"}\n" +
				"let xs = [1, 2, 3]\n" +
				"let ys = map(xs, dbl)\n" +
				"print(ys[0])\n" +
				"print(ys[1])\n" +
				"print(ys[2])\n",
			want: "2\n4\n6\n",
		},
		{
			name: "list_filter",
			src: "fun is_even(n: int): bool {\n" +
				"  return n % 2 == 0\n" +
				"}\n" +
				"let xs = [1, 2, 3, 4, 5, 6]\n" +
				"let ys = filter(xs, is_even)\n" +
				"print(len(ys))\n" +
				"print(ys[0])\n" +
				"print(ys[2])\n",
			want: "3\n2\n6\n",
		},
		{
			name: "list_reduce",
			src: "fun plus(a: int, b: int): int {\n" +
				"  return a + b\n" +
				"}\n" +
				"let xs = [1, 2, 3, 4]\n" +
				"let s = reduce(xs, plus, 0)\n" +
				"print(s)\n",
			want: "10\n",
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
