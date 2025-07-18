package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	kotlin "mochi/compiler/x/kotlin"
	"mochi/parser"
	"mochi/types"
)

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		p := filepath.Dir(dir)
		if p == dir {
			break
		}
		dir = p
	}
	return dir
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root := repoRoot()
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "kt")
	_ = os.MkdirAll(outDir, 0o755)

	var queries []int
	if env := os.Getenv("QUERIES"); env != "" {
		for _, part := range strings.Split(env, ",") {
			if n, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 13; i++ {
			queries = append(queries, i)
		}
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		errPath := filepath.Join(outDir, q+".error")
		codeFile := filepath.Join(outDir, q+".kt")
		outFile := filepath.Join(outDir, q+".out")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(codeFile)
			os.Remove(outFile)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			os.Remove(codeFile)
			os.Remove(outFile)
			continue
		}
		c := kotlin.New(env, filepath.Base(src))
		code, err := c.Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(codeFile)
			os.Remove(outFile)
			continue
		}
		if err := os.WriteFile(codeFile, code, 0o644); err != nil {
			os.WriteFile(errPath, []byte("write code: "+err.Error()), 0o644)
			os.Remove(outFile)
			continue
		}

		tmp := filepath.Join(os.TempDir(), q+".kt")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			os.WriteFile(errPath, []byte("tmp write: "+err.Error()), 0o644)
			os.Remove(outFile)
			continue
		}
		jar := filepath.Join(os.TempDir(), q+".jar")
		if out, err := exec.Command("kotlinc", tmp, "-include-runtime", "-d", jar).CombinedOutput(); err != nil {
			os.WriteFile(errPath, append([]byte("kotlinc: "+err.Error()+"\n"), out...), 0o644)
			os.Remove(outFile)
			continue
		}
		out, err := exec.Command("java", "-jar", jar).CombinedOutput()
		if err != nil {
			os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
			os.Remove(outFile)
			continue
		}

		os.Remove(errPath)
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(outFile, cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", q, err)
		}
	}
}
