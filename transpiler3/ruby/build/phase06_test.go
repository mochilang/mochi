package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase6SumTypes exercises Mochi union (sum) types lowering to a Ruby
// module with Data.define variants, and `match` lowering to case/in.
func TestPhase6SumTypes(t *testing.T) {
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
		wantInRb []string
	}{
		{
			name: "sum_variant_match",
			src: "type Num = Pos(n: int) | Neg(n: int) | Zero\n" +
				"fun abs_val(x: Num): int {\n" +
				"  return match x {\n" +
				"    Pos(n) => n\n" +
				"    Neg(n) => -n\n" +
				"    Zero => 0\n" +
				"  }\n" +
				"}\n" +
				"print(abs_val(Pos(5)))\n" +
				"print(abs_val(Neg(-3)))\n" +
				"print(abs_val(Zero))\n",
			want: "5\n3\n0\n",
			wantInRb: []string{
				"module Num",
				"Pos = Data.define(:n)",
				"Neg = Data.define(:n)",
				"Zero = Data.define()",
				"case x",
				"in Num::Pos(n:)",
				"in Num::Neg(n:)",
				"in Num::Zero",
			},
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
