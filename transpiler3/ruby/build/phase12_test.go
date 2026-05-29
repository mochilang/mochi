package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase12AsyncAwait exercises async/await: async wraps a body in
// Thread.new { ... }; await calls .value to block on the result.
func TestPhase12AsyncAwait(t *testing.T) {
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
			name: "async_basic",
			src: "fun compute(): int {\n" +
				"  return 42\n" +
				"}\n" +
				"let fut = async compute()\n" +
				"let result = await fut\n" +
				"print(result)\n",
			want: "42\n",
		},
		{
			name: "async_two",
			src: "fun times_two(x: int): int {\n" +
				"  return x * 2\n" +
				"}\n" +
				"let f1 = async times_two(3)\n" +
				"let f2 = async times_two(7)\n" +
				"let r1 = await f1\n" +
				"let r2 = await f2\n" +
				"print(r1)\n" +
				"print(r2)\n",
			want: "6\n14\n",
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
