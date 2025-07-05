//go:build slow

package sqlite2duck

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/slt/logic"
)

func readLines(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var lines []string
	s := bufio.NewScanner(f)
	for s.Scan() {
		line := strings.TrimSpace(s.Text())
		if line != "" {
			line = strings.TrimSuffix(line, ";")
			lines = append(lines, line)
		}
	}
	return lines, s.Err()
}

func TestConvertGolden(t *testing.T) {
	root, err := logic.FindRepoRoot()
	if err != nil {
		t.Fatal(err)
	}
	files, err := filepath.Glob("testdata/*.sql")
	if err != nil {
		t.Fatal(err)
	}
	for _, out := range files {
		base := filepath.Base(out)
		name := strings.TrimSuffix(base, ".sql")
		var in string
		switch {
		case strings.HasPrefix(base, "select"):
			in = filepath.Join(root, "tests/dataset/slt", name+".test")
		case strings.HasPrefix(base, "slt_lang"):
			in = filepath.Join(root, "tests/dataset/slt/evidence", name+".test")
		case strings.HasPrefix(base, "aggregates_"):
			in = filepath.Join(root, "tests/dataset/slt/test/random/aggregates", strings.TrimPrefix(name, "aggregates_")+".test")
		case strings.HasPrefix(base, "expr_"):
			in = filepath.Join(root, "tests/dataset/slt/test/random/expr", strings.TrimPrefix(name, "expr_")+".test")
		case strings.HasPrefix(base, "groupby_"):
			in = filepath.Join(root, "tests/dataset/slt/test/random/groupby", strings.TrimPrefix(name, "groupby_")+".test")
		case strings.HasPrefix(base, "extra_"):
			in = filepath.Join(root, "tests/dataset/slt/test/extra", strings.TrimPrefix(name, "extra_")+".test")
		default:
			t.Fatalf("unknown golden file %s", base)
		}
		c := struct{ in, out string }{in: in, out: out}
		t.Run(base, func(t *testing.T) {
			sltCases, err := logic.ParseFile(c.in)
			if err != nil {
				t.Fatal(err)
			}
			lines, err := readLines(c.out)
			if err != nil {
				t.Fatal(err)
			}
			if len(lines) != len(sltCases) {
				t.Fatalf("case count mismatch: %d != %d", len(lines), len(sltCases))
			}
			for i, sc := range sltCases {
				i, sc := i, sc
				t.Run(fmt.Sprintf("query%d", i+1), func(t *testing.T) {
					got := Convert(sc.Query)
					want := lines[i]
					if got != want {
						t.Fatalf("convert %q\nwant %q", got, want)
					}
				})
			}
		})
	}
}
