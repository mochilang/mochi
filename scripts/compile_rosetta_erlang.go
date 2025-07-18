//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	erlang "mochi/compiler/x/erlang"
	"mochi/parser"
	"mochi/types"
)

func findRoot() string {
	dir, _ := os.Getwd()
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			panic("go.mod not found")
		}
		dir = parent
	}
}

func writeError(dir, name, msg string) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0o644)
}

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	root := findRoot()
	outDir := filepath.Join(root, "tests", "rosetta", "out", "Erlang")
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
		code, err := erlang.New(src).Compile(prog)
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("compile: %v", err))
			continue
		}
		codeFile := filepath.Join(outDir, name+".erl")
		if err := os.WriteFile(codeFile, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", name, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), name+".erl")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			writeError(outDir, name, fmt.Sprintf("tmp write: %v", err))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		outBytes, err := exec.Command("escript", tmp).CombinedOutput()
		if err != nil {
			writeError(outDir, name, fmt.Sprintf("run: %v\n%s", err, outBytes))
			os.Remove(filepath.Join(outDir, name+".out"))
			continue
		}
		cleaned := bytes.Split(outBytes, []byte("\n"))
		var buf bytes.Buffer
		for _, line := range cleaned {
			if bytes.HasPrefix(line, []byte("/tmp/")) {
				continue
			}
			if bytes.Contains(line, []byte(".erl:")) && bytes.Contains(line, []byte("Warning:")) {
				continue
			}
			if len(line) > 0 {
				buf.Write(line)
				buf.WriteByte('\n')
			}
		}
		res := buf.Bytes()
		if len(res) == 0 {
			res = []byte{'\n'}
		}
		os.Remove(filepath.Join(outDir, name+".error"))
		if err := os.WriteFile(filepath.Join(outDir, name+".out"), res, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write out", name, err)
		}
	}
}
