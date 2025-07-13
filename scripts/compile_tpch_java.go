package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	javacode "mochi/compiler/x/java"
	"mochi/parser"
	"mochi/types"
)

func classNameFromVar(s string) string {
	if s == "" {
		return ""
	}
	parts := strings.FieldsFunc(s, func(r rune) bool {
		return r == '_' || r == '-' || r == ' '
	})
	for i, p := range parts {
		if p == "" {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root, _ := os.Getwd()
	for {
		if _, err := os.Stat(filepath.Join(root, "go.mod")); err == nil {
			break
		}
		parent := filepath.Dir(root)
		if parent == root {
			panic("go.mod not found")
		}
		root = parent
	}
	outDir := filepath.Join(root, "tests", "dataset", "tpc-h", "compiler", "java")
	_ = os.MkdirAll(outDir, 0o755)

	for i := 10; i <= 10; i++ {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "tpc-h", q+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			panic(err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			panic(errs[0])
		}
		code, err := javacode.New().Compile(prog)
		if err != nil {
			panic(err)
		}
		codePath := filepath.Join(outDir, q+".java")
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			panic(err)
		}
		tmpDir := os.TempDir()
		className := classNameFromVar(q)
		if className == "" {
			className = "Main"
		}
		srcFile := filepath.Join(tmpDir, className+".java")
		if err := os.WriteFile(srcFile, code, 0o644); err != nil {
			panic(err)
		}
		if out, err := exec.Command("javac", "-d", tmpDir, srcFile).CombinedOutput(); err != nil {
			panic(fmt.Errorf("javac %s: %v\n%s", q, err, out))
		}
		cmd := exec.Command("java", "-cp", tmpDir, className)
		out, err := cmd.CombinedOutput()
		if err != nil {
			panic(fmt.Errorf("run %s: %v\n%s", q, err, out))
		}
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, q+".out"), cleaned, 0o644); err != nil {
			panic(err)
		}
	}
}
