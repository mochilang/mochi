package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase11Agents exercises Mochi agent declarations lowered to a
// Ruby class. The single-threaded synchronous dispatch path covers all
// of Phase 9.3 (mutable fields, value-returning intents, multi-intent
// programs).
func TestPhase11Agents(t *testing.T) {
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
			name: "agent_basic",
			src: "agent Counter {\n" +
				"  var count: int = 0\n" +
				"  intent increment() { count = count + 1 }\n" +
				"  intent value(): int { return count }\n" +
				"}\n" +
				"let c = Counter { count: 0 }\n" +
				"c.increment()\n" +
				"c.increment()\n" +
				"c.increment()\n" +
				"print(c.value())\n",
			want: "3\n",
			wantInRb: []string{
				"class Counter",
				"def initialize(count:)",
				"@count = count",
				"def increment",
				"def value",
			},
		},
		{
			name: "agent_multi_intent",
			src: "agent Adder {\n" +
				"  var total: int = 0\n" +
				"  intent add(n: int) { total = total + n }\n" +
				"  intent sub(n: int) { total = total - n }\n" +
				"  intent get(): int { return total }\n" +
				"}\n" +
				"let a = Adder { total: 0 }\n" +
				"a.add(10)\n" +
				"a.add(5)\n" +
				"a.sub(3)\n" +
				"print(a.get())\n",
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
