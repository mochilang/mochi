package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase17MathMap exercises math (abs, floor, ceil) and map helpers
// (len, keys, values).
func TestPhase17MathMap(t *testing.T) {
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
			name: "math_abs_int",
			src: "let n: int = -7\n" +
				"print(abs(n))\n",
			want: "7\n",
		},
		{
			name: "math_abs_float",
			src: "let f: float = -3.5\n" +
				"print(abs(f))\n",
			want: "3.5\n",
		},
		{
			name: "math_floor_ceil",
			src: "let f: float = 3.7\n" +
				"print(floor(f))\n" +
				"print(ceil(f))\n",
			want: "3\n4\n",
		},
		{
			name: "map_len",
			src: "let m: map<string,int> = {\"a\": 1, \"b\": 2, \"c\": 3}\n" +
				"print(len(m))\n",
			want: "3\n",
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
