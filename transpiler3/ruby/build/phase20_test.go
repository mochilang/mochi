package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase20AgentSpawn exercises `spawn AgentType()` which constructs a
// fresh agent with each declared field at its zero value, then routes
// intent calls into the instance just like the AgentLit form.
func TestPhase20AgentSpawn(t *testing.T) {
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
			name: "spawn_counter",
			src: "agent Counter {\n" +
				"  var count: int = 0\n" +
				"  intent increment() { count = count + 1 }\n" +
				"  intent value(): int { return count }\n" +
				"}\n" +
				"let c = spawn Counter()\n" +
				"c.increment()\n" +
				"c.increment()\n" +
				"c.increment()\n" +
				"let v = c.value()\n" +
				"print(v)\n",
			want: "3\n",
		},
		{
			name: "spawn_string_state",
			src: "agent Greeter {\n" +
				"  var prefix: string = \"hi\"\n" +
				"  intent set_prefix(p: string) { prefix = p }\n" +
				"  intent greet(name: string): string { return prefix + \"-\" + name }\n" +
				"}\n" +
				"let g = spawn Greeter()\n" +
				"g.set_prefix(\"hey\")\n" +
				"print(g.greet(\"sam\"))\n",
			want: "hey-sam\n",
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
