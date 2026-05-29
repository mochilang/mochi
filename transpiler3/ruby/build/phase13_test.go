package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase13Streams exercises stream<T> broadcast channels lowered to
// Mochi::Runtime::Stream (Mutex-guarded per-subscriber SizedQueues).
func TestPhase13Streams(t *testing.T) {
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
			name: "stream_basic",
			src: "let s: stream<int> = make_stream(4)\n" +
				"let sub = subscribe(s)\n" +
				"emit(s, 10)\n" +
				"emit(s, 20)\n" +
				"emit(s, 30)\n" +
				"let a = recv_sub(sub)\n" +
				"let b = recv_sub(sub)\n" +
				"let c = recv_sub(sub)\n" +
				"print(a)\n" +
				"print(b)\n" +
				"print(c)\n",
			want: "10\n20\n30\n",
		},
		{
			name: "stream_multi_sub",
			src: "let s: stream<int> = make_stream(4)\n" +
				"let sub1 = subscribe(s)\n" +
				"let sub2 = subscribe(s)\n" +
				"emit(s, 7)\n" +
				"let a = recv_sub(sub1)\n" +
				"let b = recv_sub(sub2)\n" +
				"print(a)\n" +
				"print(b)\n",
			want: "7\n7\n",
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
