//go:build slow

package sqlite2duck

import (
	"bufio"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/slt/logic"
)

// TestCLI runs the sqlite2duck command against each query and compares
// the output to the golden files. Building the command once keeps the
// test relatively quick.
func TestCLI(t *testing.T) {
	root, err := logic.FindRepoRoot()
	if err != nil {
		t.Fatal(err)
	}

	bin := filepath.Join(t.TempDir(), "sqlite2duck")
	cmd := exec.Command("go", "build", "-o", bin, "../../cmd/sqlite2duck")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("build cli: %v\n%s", err, out)
	}

	files, err := filepath.Glob("testdata/*.sql")
	if err != nil {
		t.Fatal(err)
	}
	for _, outPath := range files {
		base := filepath.Base(outPath)
		name := strings.TrimSuffix(base, ".sql")
		var inPath string
		switch {
		case strings.HasPrefix(base, "select"):
			inPath = filepath.Join(root, "tests/dataset/slt", name+".test")
		case strings.HasPrefix(base, "slt_lang"):
			inPath = filepath.Join(root, "tests/dataset/slt/evidence", name+".test")
		case strings.HasPrefix(base, "aggregates_"):
			inPath = filepath.Join(root, "tests/dataset/slt/test/random/aggregates", strings.TrimPrefix(name, "aggregates_")+".test")
		case strings.HasPrefix(base, "expr_"):
			inPath = filepath.Join(root, "tests/dataset/slt/test/random/expr", strings.TrimPrefix(name, "expr_")+".test")
		case strings.HasPrefix(base, "groupby_"):
			inPath = filepath.Join(root, "tests/dataset/slt/test/random/groupby", strings.TrimPrefix(name, "groupby_")+".test")
		case strings.HasPrefix(base, "extra_"):
			inPath = filepath.Join(root, "tests/dataset/slt/test/extra", strings.TrimPrefix(name, "extra_")+".test")
		default:
			t.Fatalf("unknown golden file %s", base)
		}

		t.Run(base, func(t *testing.T) {
			cases, err := logic.ParseFile(inPath)
			if err != nil {
				t.Fatal(err)
			}
			f, err := os.Open(outPath)
			if err != nil {
				t.Fatal(err)
			}
			defer f.Close()
			scanner := bufio.NewScanner(f)
			var lines []string
			for scanner.Scan() {
				line := strings.TrimSpace(scanner.Text())
				if line != "" {
					line = strings.TrimSuffix(line, ";")
					lines = append(lines, line)
				}
			}
			if err := scanner.Err(); err != nil {
				t.Fatal(err)
			}
			if len(lines) != len(cases) {
				t.Fatalf("case count mismatch: %d != %d", len(lines), len(cases))
			}
			for i, c := range cases {
				if i >= 5 {
					break
				}
				want := lines[i]
				execCmd := exec.Command(bin, "-")
				execCmd.Stdin = strings.NewReader(c.Query)
				out, err := execCmd.Output()
				if err != nil {
					if ee, ok := err.(*exec.ExitError); ok {
						t.Fatalf("cli error: %v %s", err, string(ee.Stderr))
					}
					t.Fatal(err)
				}
				got := strings.TrimSpace(string(out))
				if got != want {
					t.Fatalf("convert %q\nwant %q", c.Query, want)
				}
			}
		})
	}
}
