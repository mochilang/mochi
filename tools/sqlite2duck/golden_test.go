package sqlite2duck

import (
	"bufio"
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
	cases := []struct {
		in  string
		out string
	}{
		{filepath.Join(root, "tests/dataset/slt/select1.test"), "testdata/select1.sql"},
		{filepath.Join(root, "tests/dataset/slt/select2.test"), "testdata/select2.sql"},
		{filepath.Join(root, "tests/dataset/slt/select3.test"), "testdata/select3.sql"},
		{filepath.Join(root, "tests/dataset/slt/evidence/slt_lang_aggfunc.test"), "testdata/slt_lang_aggfunc.sql"},
		{filepath.Join(root, "tests/dataset/slt/evidence/slt_lang_update.test"), "testdata/slt_lang_update.sql"},
		{filepath.Join(root, "tests/dataset/slt/test/random/aggregates/slt_good_0.test"), "testdata/aggregates_slt_good_0.sql"},
		{filepath.Join(root, "tests/dataset/slt/test/random/expr/slt_good_0.test"), "testdata/expr_slt_good_0.sql"},
		{filepath.Join(root, "tests/dataset/slt/test/random/groupby/slt_good_0.test"), "testdata/groupby_slt_good_0.sql"},
	}
	for _, c := range cases {
		t.Run(filepath.Base(c.out), func(t *testing.T) {
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
				got := Convert(sc.Query)
				want := lines[i]
				if got != want {
					t.Fatalf("case %d\nconvert %q\nwant %q\n", i+1, got, want)
				}
			}
		})
	}
}
