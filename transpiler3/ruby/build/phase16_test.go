package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestPhase16SetsOMaps exercises set literals + ops (add, has, len, iter)
// and ordered map literals + ops (get, set, has, len).
func TestPhase16SetsOMaps(t *testing.T) {
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
			name: "set_add_has",
			src: "var s: set<int> = set{1, 2}\n" +
				"s = add(s, 3)\n" +
				"print(has(s, 3))\n" +
				"print(has(s, 4))\n",
			want: "true\nfalse\n",
		},
		{
			name: "set_len",
			src: "var s: set<int> = set{1, 2, 3, 4}\n" +
				"print(len(s))\n",
			want: "4\n",
		},
		{
			name: "set_forin",
			src: "var s: set<int> = set{42}\n" +
				"for x in s {\n" +
				"  print(x)\n" +
				"}\n",
			want: "42\n",
		},
		{
			name: "omap_literal",
			src: "var m: omap<string,int> = omap{\"a\": 1, \"b\": 2, \"c\": 3}\n" +
				"print(m[\"a\"])\n" +
				"print(m[\"c\"])\n",
			want: "1\n3\n",
		},
		{
			name: "omap_set",
			src: "var m: omap<string,int> = omap{\"x\": 10}\n" +
				"m[\"y\"] = 20\n" +
				"print(m[\"x\"])\n" +
				"print(m[\"y\"])\n",
			want: "10\n20\n",
		},
		{
			name: "omap_has_len",
			src: "var m: omap<string,int> = omap{\"p\": 1, \"q\": 2}\n" +
				"print(has(m, \"p\"))\n" +
				"print(has(m, \"r\"))\n" +
				"print(len(m))\n",
			want: "true\nfalse\n2\n",
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
