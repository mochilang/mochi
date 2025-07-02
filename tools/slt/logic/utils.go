package logic

import (
	"bytes"
	"crypto/md5"
	"database/sql"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	_ "github.com/marcboeker/go-duckdb"
	"mochi/parser"
	mod "mochi/runtime/mod"
	"mochi/runtime/vm"
	"mochi/types"
)

// FindRepoRoot searches parent directories until go.mod is found.
func FindRepoRoot() (string, error) {
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

// DownloadFile retrieves url and stores it at path.
func DownloadFile(url, path string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
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

// RunMochi compiles and executes a Mochi program and returns its output.
func RunMochi(src string) (string, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return "", err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", errs[0]
	}
	modRoot, errRoot := mod.FindRoot(".")
	if errRoot != nil {
		modRoot = "."
	}
	os.Setenv("MOCHI_ROOT", modRoot)
	p, err := vm.Compile(prog, env)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		return "", err
	}
	return strings.TrimSpace(buf.String()), nil
}

// EvalCase executes the SQL query of c using DuckDB and returns the
// flattened result values as strings. Updates stored in c.Updates are
// applied before running the query.
func EvalCase(c Case) ([]string, string, error) {
	db, err := sql.Open("duckdb", "")
	if err != nil {
		return nil, "", err
	}
	defer db.Close()
	for name, t := range c.Tables {
		cols := strings.Join(t.Columns, " INTEGER,") + " INTEGER"
		if _, err := db.Exec(fmt.Sprintf("CREATE TABLE %s(%s)", name, cols)); err != nil {
			return nil, "", err
		}
		placeholders := make([]string, len(t.Columns))
		for i := range placeholders {
			placeholders[i] = "?"
		}
		insertSQL := fmt.Sprintf("INSERT INTO %s VALUES(%s)", name, strings.Join(placeholders, ","))
		for _, row := range t.Rows {
			vals := make([]any, len(t.Columns))
			for i, col := range t.Columns {
				vals[i] = row[col]
			}
			if _, err := db.Exec(insertSQL, vals...); err != nil {
				return nil, "", err
			}
		}
	}
	for _, stmt := range c.Updates {
		if _, err := db.Exec(stmt); err != nil {
			return nil, "", err
		}
	}
	rows, err := db.Query(c.Query)
	if err != nil {
		return nil, "", err
	}
	defer rows.Close()
	cols, err := rows.Columns()
	if err != nil {
		return nil, "", err
	}
	var flat []string
	var buf bytes.Buffer
	for rows.Next() {
		vals := make([]any, len(cols))
		ptrs := make([]any, len(cols))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return nil, "", err
		}
		for i, v := range vals {
			s := fmt.Sprint(v)
			flat = append(flat, s)
			if i > 0 {
				buf.WriteByte(' ')
			}
			buf.WriteString(s)
		}
		buf.WriteByte('\n')
	}
	hash := fmt.Sprintf("%x", md5.Sum(buf.Bytes()))
	return flat, hash, nil
}

// Fetch downloads SQLLogicTest files into the dataset directory.
func Fetch(repo string, files []string, force bool) error {
	root, err := FindRepoRoot()
	if err != nil {
		return err
	}
	dir := filepath.Join(root, "tests/dataset/slt")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	for _, f := range files {
		local := filepath.Join(dir, f)
		if _, err := os.Stat(local); err == nil && !force {
			continue
		}
		if err := os.MkdirAll(filepath.Dir(local), 0o755); err != nil {
			return err
		}
		url := repo + "/" + f
		if err := DownloadFile(url, local); err != nil {
			return err
		}
	}
	return nil
}

// Generate reads SLT files and converts them into Mochi programs.
// If run is true, the generated program is executed and the output
// stored next to the source with a .out extension.
func GenerateFiles(files []string, outDir string, run bool, start, end int) error {
	root, err := FindRepoRoot()
	if err != nil {
		return err
	}
	dir := filepath.Join(root, "tests/dataset/slt")
	if outDir == "" {
		outDir = filepath.Join(dir, "out", "evidence")
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	for _, f := range files {
		local := filepath.Join(dir, f)
		cases, err := ParseFile(local)
		if err != nil {
			return err
		}
		base := filepath.Base(f)
		testDir := filepath.Join(outDir, strings.TrimSuffix(base, ".test"))
		if err := os.MkdirAll(testDir, 0o755); err != nil {
			return err
		}
		for i, c := range cases {
			idx := i + 1
			if start > 0 && idx < start {
				continue
			}
			if end > 0 && idx > end {
				break
			}
			if c.Hash != "" {
				exp, _, err := EvalCase(c)
				if err != nil {
					return err
				}
				c.Expect = exp
				c.Hash = ""
			}
			code := Generate(c)
			if code == "" {
				continue
			}
			srcPath := filepath.Join(testDir, c.Name+".mochi")
			if err := os.WriteFile(srcPath, []byte(code), 0o644); err != nil {
				return err
			}
			if run {
				out, err := RunMochi(code)
				if err != nil {
					return err
				}
				if err := os.WriteFile(filepath.Join(testDir, c.Name+".out"), []byte(out+"\n"), 0o644); err != nil {
					return err
				}
			}
		}
	}
	return nil
}

// DiscoverTests returns all .test files in the given GitHub repository.
// The repo parameter can be the owner/repo pair or a raw.githubusercontent URL.
func DiscoverTests(repo string) ([]string, error) {
	var url string
	if strings.HasPrefix(repo, "http") && !strings.Contains(repo, "raw.githubusercontent.com") {
		url = repo
	} else {
		if strings.HasPrefix(repo, "http") {
			repo = strings.TrimPrefix(repo, "https://raw.githubusercontent.com/")
			repo = strings.TrimPrefix(repo, "http://raw.githubusercontent.com/")
			parts := strings.Split(repo, "/")
			if len(parts) >= 2 {
				repo = parts[0] + "/" + parts[1]
			}
		}
		if !strings.Contains(repo, "/") {
			return nil, fmt.Errorf("invalid repo: %s", repo)
		}
		url = fmt.Sprintf("https://api.github.com/repos/%s/git/trees/master?recursive=1", repo)
	}
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("list tests: %s", resp.Status)
	}
	var data struct {
		Tree []struct {
			Path string `json:"path"`
			Type string `json:"type"`
		} `json:"tree"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&data); err != nil {
		return nil, err
	}
	var files []string
	for _, t := range data.Tree {
		if t.Type == "blob" && strings.HasSuffix(t.Path, ".test") {
			files = append(files, t.Path)
		}
	}
	return files, nil
}
