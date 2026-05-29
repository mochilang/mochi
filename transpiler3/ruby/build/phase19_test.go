package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase19TryCatch exercises Mochi try/catch and panic via Ruby's
// begin/rescue mapped through Mochi::Runtime::Panic.
func TestPhase19TryCatch(t *testing.T) {
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
			name: "panic_caught",
			src: "try {\n" +
				"  print(\"before\")\n" +
				"  panic(42, \"boom\")\n" +
				"  print(\"after\")\n" +
				"} catch e {\n" +
				"  print(\"caught\")\n" +
				"  print(e)\n" +
				"}\n",
			want: "before\ncaught\n42\n",
		},
		{
			name: "panic_caught_in_fun",
			src: "fun safe(n: int): int {\n" +
				"  var r: int = 0\n" +
				"  try {\n" +
				"    panic(n, \"x\")\n" +
				"  } catch e {\n" +
				"    r = e\n" +
				"  }\n" +
				"  return r\n" +
				"}\n" +
				"print(safe(7))\n" +
				"print(safe(11))\n",
			want: "7\n11\n",
		},
		{
			name: "try_no_panic",
			src: "var r: int = 0\n" +
				"try {\n" +
				"  r = 1\n" +
				"} catch e {\n" +
				"  r = -1\n" +
				"}\n" +
				"print(r)\n",
			want: "1\n",
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
