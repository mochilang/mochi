package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase7Closures exercises non-capturing and capturing fun-typed
// closures lowered to Ruby Procs over lifted module methods.
func TestPhase7Closures(t *testing.T) {
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
			name: "closure_simple",
			src: "let double_it = fun(x: int): int => x * 2\n" +
				"let r = double_it(21)\n" +
				"print(r)\n",
			want: "42\n",
			wantInRb: []string{
				"def self.__anon_",
				"double_it.call(21)",
			},
		},
		{
			name: "closure_two_arg",
			src: "let plus = fun(a: int, b: int): int => a + b\n" +
				"let r = plus(3, 4)\n" +
				"print(r)\n",
			want: "7\n",
		},
		{
			name: "closure_in_function",
			src: "fun compute(base: int): int {\n" +
				"  let triple = fun(x: int): int => x * 3\n" +
				"  return triple(base)\n" +
				"}\n" +
				"print(compute(5))\n" +
				"print(compute(10))\n",
			want: "15\n30\n",
		},
		{
			name: "capture_int",
			src: "let x = 10\n" +
				"let add_x = fun(n: int): int => n + x\n" +
				"let r = add_x(5)\n" +
				"print(r)\n",
			want: "15\n",
			wantInRb: []string{
				":x =>",
			},
		},
		{
			name: "capture_multi",
			src: "let a = 3\n" +
				"let b = 7\n" +
				"let combine = fun(x: int): int => x + a + b\n" +
				"let r = combine(10)\n" +
				"print(r)\n",
			want: "20\n",
		},
		{
			name: "capture_string",
			src: "let prefix = \"hello\"\n" +
				"let greet = fun(name: string): string => prefix + \" \" + name\n" +
				"let r = greet(\"world\")\n" +
				"print(r)\n",
			want: "hello world\n",
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
