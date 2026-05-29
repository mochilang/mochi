package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase25ControlFlow exercises if / else if / else, while, and
// for ... in start..end.
func TestPhase25ControlFlow(t *testing.T) {
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
			name: "if_else",
			src: "let age = 20\n" +
				"if age >= 18 {\n" +
				"  print(\"adult\")\n" +
				"} else {\n" +
				"  print(\"minor\")\n" +
				"}\n",
			want: "adult\n",
		},
		{
			name: "if_elsif_else",
			src: "let score = 85\n" +
				"if score >= 90 {\n" +
				"  print(\"A\")\n" +
				"} else if score >= 80 {\n" +
				"  print(\"B\")\n" +
				"} else {\n" +
				"  print(\"C\")\n" +
				"}\n",
			want: "B\n",
		},
		{
			name: "while_loop",
			src: "var i = 0\n" +
				"while i < 3 {\n" +
				"  print(i)\n" +
				"  i = i + 1\n" +
				"}\n",
			want: "0\n1\n2\n",
		},
		{
			name: "for_range",
			src: "for i in 0..3 {\n" +
				"  print(i)\n" +
				"}\n",
			want: "0\n1\n2\n",
		},
		{
			name: "for_range_sum",
			src: "var sum = 0\n" +
				"for x in 1..11 {\n" +
				"  sum = sum + x\n" +
				"}\n" +
				"print(sum)\n",
			want: "55\n",
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
				t.Fatalf("Build: %v", err)
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
