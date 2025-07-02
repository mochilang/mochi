package logic

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"

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
func GenerateFiles(files []string, outDir string, run bool) error {
	root, err := FindRepoRoot()
	if err != nil {
		return err
	}
	dir := filepath.Join(root, "tests/dataset/slt")
	if outDir == "" {
		outDir = filepath.Join(dir, "out")
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
		testDir := filepath.Join(outDir, strings.TrimSuffix(filepath.Base(f), ".test"))
		if err := os.MkdirAll(testDir, 0o755); err != nil {
			return err
		}
		for _, c := range cases {
			code := Generate(c)
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
