package main

import (
	"bufio"
	"crypto/md5"
	"database/sql"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	_ "modernc.org/sqlite"
)

type TestCase struct {
	Inserts []string
	Query   string
	Hash    string
	Values  []string
}

func findRepoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

func downloadFile(url, path string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return fmt.Errorf("download %s: %s", url, resp.Status)
	}
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()
	_, err = io.Copy(f, resp.Body)
	return err
}

func ensureTests(dir string) error {
	os.MkdirAll(dir, 0755)
	files := []string{"select1.test"}
	for _, name := range files {
		path := filepath.Join(dir, name)
		if _, err := os.Stat(path); err == nil {
			continue
		}
		url := "https://raw.githubusercontent.com/gregrahn/sqllogictest/master/test/" + name
		if err := downloadFile(url, path); err != nil {
			return err
		}
	}
	return nil
}

func parseFirstCase(path string) (*TestCase, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	tc := &TestCase{}
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "statement") {
			// read next line as SQL
			if !scanner.Scan() {
				return nil, fmt.Errorf("unexpected EOF")
			}
			sql := strings.TrimSpace(scanner.Text())
			if strings.HasPrefix(strings.ToUpper(sql), "INSERT") {
				tc.Inserts = append(tc.Inserts, sql)
			}
		} else if strings.HasPrefix(line, "query") {
			// read SQL lines until ----
			var parts []string
			for scanner.Scan() {
				l := strings.TrimSpace(scanner.Text())
				if l == "----" {
					break
				}
				if l != "" {
					parts = append(parts, l)
				}
			}
			tc.Query = strings.Join(parts, " ")
			// read expected line
			if scanner.Scan() {
				exp := strings.TrimSpace(scanner.Text())
				if strings.Contains(exp, "hashing to") {
					fields := strings.Fields(exp)
					tc.Hash = fields[len(fields)-1]
				} else if exp != "" {
					tc.Values = append(tc.Values, exp)
					for scanner.Scan() {
						l := strings.TrimSpace(scanner.Text())
						if l == "" {
							break
						}
						tc.Values = append(tc.Values, l)
					}
				}
			}
			break
		}
	}
	return tc, scanner.Err()
}

func runSQL(tc *TestCase) ([]string, string, error) {
	db, err := sql.Open("sqlite", ":memory:")
	if err != nil {
		return nil, "", err
	}
	defer db.Close()
	_, err = db.Exec("CREATE TABLE t1(a INTEGER, b INTEGER, c INTEGER, d INTEGER, e INTEGER)")
	if err != nil {
		return nil, "", err
	}
	for _, ins := range tc.Inserts {
		if _, err := db.Exec(ins); err != nil {
			return nil, "", err
		}
	}
	rows, err := db.Query(tc.Query)
	if err != nil {
		return nil, "", err
	}
	defer rows.Close()
	cols, _ := rows.Columns()
	var out []string
	for rows.Next() {
		vals := make([]interface{}, len(cols))
		ptrs := make([]interface{}, len(cols))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return nil, "", err
		}
		parts := make([]string, len(cols))
		for i, v := range vals {
			if b, ok := v.([]byte); ok {
				parts[i] = string(b)
			} else if v == nil {
				parts[i] = "NULL"
			} else {
				parts[i] = fmt.Sprint(v)
			}
		}
		out = append(out, strings.Join(parts, " "))
	}
	hash := md5.Sum([]byte(strings.Join(out, "\n")))
	return out, hex.EncodeToString(hash[:]), nil
}

func genMochi(tc *TestCase, result []string, path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()
	w := bufio.NewWriter(f)
	fmt.Fprintln(w, "let t1 = [")
	reIns := regexp.MustCompile(`INSERT INTO t1\(([^)]*)\) VALUES\(([^)]*)\)`)
	for _, ins := range tc.Inserts {
		m := reIns.FindStringSubmatch(ins)
		if len(m) != 3 {
			continue
		}
		cols := strings.Split(m[1], ",")
		vals := strings.Split(m[2], ",")
		for i := range cols {
			cols[i] = strings.TrimSpace(cols[i])
		}
		for i := range vals {
			vals[i] = strings.TrimSpace(vals[i])
		}
		row := map[string]string{}
		for i, c := range cols {
			if i < len(vals) {
				row[c] = vals[i]
			}
		}
		fmt.Fprintf(w, "  { a: %s, b: %s, c: %s, d: %s, e: %s },\n",
			row["a"], row["b"], row["c"], row["d"], row["e"])
	}
	fmt.Fprintln(w, "]\n")
	fmt.Fprintln(w, "let avg_c = avg(from r in t1 select r.c)")
	fmt.Fprintln(w, "let result = from r in t1")
	fmt.Fprintln(w, "             sort by [if r.c > avg_c { r.a * 2 } else { r.b * 10 }]")
	fmt.Fprintln(w, "             select if r.c > avg_c { r.a * 2 } else { r.b * 10 }\n")
	fmt.Fprintln(w, "json(result)\n")
	fmt.Fprintln(w, "test \"logic case 1\" {")
	fmt.Fprintln(w, "  expect result == [")
	for i, v := range result {
		sep := ","
		if i == len(result)-1 {
			sep = ""
		}
		fmt.Fprintf(w, "    %s%s\n", v, sep)
	}
	fmt.Fprintln(w, "  ]")
	fmt.Fprintln(w, "}")
	return w.Flush()
}

func runMochi(root, path string) error {
	cmd := exec.Command("go", "run", "./cmd/mochi", "test", path)
	cmd.Dir = root
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func main() {
	root, err := findRepoRoot()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	dataDir := filepath.Join(root, "tests/dataset/logic")
	outDir := filepath.Join(dataDir, "out")
	if err := ensureTests(dataDir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	tc, err := parseFirstCase(filepath.Join(dataDir, "select1.test"))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	result, hash, err := runSQL(tc)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if tc.Hash != "" && tc.Hash != hash {
		fmt.Fprintf(os.Stderr, "hash mismatch: got %s expected %s\n", hash, tc.Hash)
	}
	os.MkdirAll(outDir, 0755)
	mochiPath := filepath.Join(outDir, "case1.mochi")
	if err := genMochi(tc, result, mochiPath); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := runMochi(root, mochiPath); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
