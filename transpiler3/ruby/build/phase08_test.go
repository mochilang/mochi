package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase8Queries exercises `from x in src [where cond] select expr`
// query expressions lowered to a Ruby for-each over the source list
// that appends to a fresh result array.
func TestPhase8Queries(t *testing.T) {
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
			name: "query_map",
			src: "let nums = [1, 2, 3, 4, 5]\n" +
				"let doubled = from n in nums select n * 2\n" +
				"for d in doubled {\n" +
				"  print(d)\n" +
				"}\n",
			want: "2\n4\n6\n8\n10\n",
		},
		{
			name: "query_filter",
			src: "let nums = [1, 2, 3, 4, 5, 6]\n" +
				"let evens = from n in nums where n % 2 == 0 select n\n" +
				"for e in evens {\n" +
				"  print(e)\n" +
				"}\n",
			want: "2\n4\n6\n",
		},
		{
			name: "query_filter_map",
			src: "let nums = [1, 2, 3, 4, 5]\n" +
				"let big_doubled = from n in nums where n > 2 select n * 2\n" +
				"for v in big_doubled {\n" +
				"  print(v)\n" +
				"}\n",
			want: "6\n8\n10\n",
		},
		{
			name: "query_order_by",
			src: "let nums: list<int> = [5, 3, 1, 4, 2]\n" +
				"let sorted = from n in nums order by n select n\n" +
				"for x in sorted {\n" +
				"  print(x)\n" +
				"}\n",
			want: "1\n2\n3\n4\n5\n",
		},
		{
			name: "query_skip_take",
			src: "let nums: list<int> = [10, 20, 30, 40, 50]\n" +
				"let page = from n in nums skip 1 take 3 select n\n" +
				"for x in page {\n" +
				"  print(x)\n" +
				"}\n",
			want: "20\n30\n40\n",
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
