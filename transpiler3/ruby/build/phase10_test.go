package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase10Channels exercises Mochi chan<T> values lowered to Ruby
// Thread::SizedQueue. Single-thread fixtures (cap >= number of sends
// before any recv) avoid the blocking-producer issue.
func TestPhase10Channels(t *testing.T) {
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
			name: "chan_basic",
			src: "let ch: chan<int> = make_chan(1)\n" +
				"send(ch, 42)\n" +
				"let x = recv(ch)\n" +
				"print(x)\n",
			want: "42\n",
		},
		{
			name: "chan_bool",
			src: "let ch: chan<bool> = make_chan(2)\n" +
				"send(ch, true)\n" +
				"send(ch, false)\n" +
				"let a = recv(ch)\n" +
				"let b = recv(ch)\n" +
				"print(a)\n" +
				"print(b)\n",
			want: "true\nfalse\n",
		},
		{
			name: "chan_buffered",
			src: "let ch: chan<int> = make_chan(3)\n" +
				"send(ch, 10)\n" +
				"send(ch, 20)\n" +
				"send(ch, 30)\n" +
				"let a = recv(ch)\n" +
				"let b = recv(ch)\n" +
				"let c = recv(ch)\n" +
				"print(a)\n" +
				"print(b)\n" +
				"print(c)\n",
			want: "10\n20\n30\n",
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
