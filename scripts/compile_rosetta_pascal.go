//go:build archive

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	pascode "mochi/compiler/x/pascal"
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

func writeError(dir, name, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	fpc, err := pascode.EnsureFPC()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	root := repoRoot()
	outDir := filepath.Join(root, "tests", "rosetta", "out", "Pascal")
	_ = os.MkdirAll(outDir, 0o755)

	var tasks []string
	if env := os.Getenv("TASKS"); env != "" {
		for _, part := range strings.Split(env, ",") {
			n := strings.TrimSpace(part)
			if n != "" {
				tasks = append(tasks, n)
			}
		}
	} else {
		pattern := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "*.mochi")
		files, _ := filepath.Glob(pattern)
		for _, f := range files {
			tasks = append(tasks, strings.TrimSuffix(filepath.Base(f), ".mochi"))
		}
	}

	for _, name := range tasks {
		src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("parse: %v", err))
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			writeError(outDir, name, fmt.Sprintf("type: %v", errs[0]))
			continue
		}
		code, err := pascode.New(env).Compile(prog)
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("compile: %v", err))
			continue
		}
		codeFile := filepath.Join(outDir, name+".pas")
		if err := os.WriteFile(codeFile, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", name, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), name+".pas")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			writeError(outDir, name, fmt.Sprintf("tmp write: %v", err))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		bin := filepath.Join(os.TempDir(), name)
		if out, err := exec.Command(fpc, tmp, "-o"+bin).CombinedOutput(); err != nil {
			writeError(outDir, name, fmt.Sprintf("fpc: %v\n%s", err, out))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		out, err := exec.Command(bin).CombinedOutput()
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("run: %v\n%s", err, out))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		os.Remove(filepath.Join(outDir, name+".error"))
		cleaned := append(bytes.TrimSpace(out), '\n')
		if err := os.WriteFile(filepath.Join(outDir, name+".out"), cleaned, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", name, err)
		}
	}
}
