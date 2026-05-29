package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase5UserFuncs exercises user-defined Mochi functions calling other
// functions, including recursion and value-returning.
func TestPhase5UserFuncs(t *testing.T) {
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
			name: "fun_simple",
			src: "fun add(a: int, b: int): int {\n" +
				"  return a + b\n" +
				"}\n" +
				"print(add(2, 3))\n",
			want: "5\n",
		},
		{
			name: "fun_recursive",
			src: "fun factorial(n: int): int {\n" +
				"  if n <= 1 {\n" +
				"    return 1\n" +
				"  } else {\n" +
				"    return n * factorial(n - 1)\n" +
				"  }\n" +
				"}\n" +
				"print(factorial(5))\n",
			want: "120\n",
		},
		{
			name: "fun_chain",
			src: "fun double(x: int): int {\n" +
				"  return x + x\n" +
				"}\n" +
				"fun quad(x: int): int {\n" +
				"  return double(double(x))\n" +
				"}\n" +
				"print(quad(3))\n",
			want: "12\n",
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
