package main

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	gocode "mochi/compile/go"
	tscode "mochi/compile/ts"
	"mochi/parser"
	"mochi/types"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "error:", err)
		os.Exit(1)
	}
}

func run() error {
	base := "bench/template"
	outDir := "bench/out"
	os.RemoveAll(outDir)
	if err := os.MkdirAll(outDir, 0755); err != nil {
		return err
	}

	err := filepath.WalkDir(base, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		if filepath.Ext(path) != ".mochi" {
			return nil
		}
		parts := strings.Split(path[len(base)+1:], string(os.PathSeparator))
		if len(parts) < 2 {
			return nil
		}
		category := parts[0]
		name := strings.TrimSuffix(parts[1], ".mochi")
		if name == "matrix_mul" {
			return nil
		}
		for _, n := range []int{10, 20, 30} {
			rendered, err := renderTemplate(path, n)
			if err != nil {
				return err
			}
			tmp := filepath.Join(outDir, "tmp.mochi")
			if err := os.WriteFile(tmp, []byte(rendered), 0644); err != nil {
				return err
			}
			goOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.go.out", category, name, n))
			tsOut := filepath.Join(outDir, fmt.Sprintf("%s_%s_%d.ts.out", category, name, n))
			if err := compileToGo(tmp, goOut); err != nil {
				return err
			}
			if err := compileToTs(tmp, tsOut); err != nil {
				return err
			}
			os.Remove(tmp)
		}
		return nil
	})
	return err
}

func renderTemplate(path string, n int) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	code := strings.ReplaceAll(string(data), "{{ .N }}", fmt.Sprintf("%d", n))
	return code, nil
}

func compileToGo(mochiFile, goFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	c := gocode.New(typeEnv)
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	return os.WriteFile(goFile, code, 0644)
}

func compileToTs(mochiFile, tsFile string) error {
	prog, err := parser.Parse(mochiFile)
	if err != nil {
		return err
	}
	typeEnv := types.NewEnv(nil)
	if errs := types.Check(prog, typeEnv); len(errs) > 0 {
		return fmt.Errorf("type error: %v", errs[0])
	}
	c := tscode.New()
	code, err := c.Compile(prog)
	if err != nil {
		return err
	}
	return os.WriteFile(tsFile, code, 0644)
}
