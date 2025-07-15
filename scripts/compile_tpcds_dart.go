package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	dartcode "mochi/compiler/x/dart"
	"mochi/parser"
	"mochi/types"
)

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")

	outDir := filepath.Join("tests", "dataset", "tpc-ds", "compiler", "dart")
	_ = os.MkdirAll(outDir, 0o755)

	queries := []int{}
	if env := os.Getenv("QUERIES"); env != "" {
		for _, part := range strings.Split(env, ",") {
			if n, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
				queries = append(queries, n)
			}
		}
	} else {
		for i := 1; i <= 99; i++ {
			queries = append(queries, i)
		}
	}

	for _, i := range queries {
		q := fmt.Sprintf("q%d", i)
		src := filepath.Join("tests", "dataset", "tpc-ds", q+".mochi")
		if _, err := os.Stat(src); err != nil {
			continue
		}
		codeOut := filepath.Join(outDir, q+".dart.out")
		outPath := filepath.Join(outDir, q+".out")
		errPath := filepath.Join(outDir, q+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(codeOut)
			os.Remove(outPath)
			fmt.Fprintln(os.Stderr, "parse", q, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			os.Remove(codeOut)
			os.Remove(outPath)
			fmt.Fprintln(os.Stderr, "type", q, errs[0])
			continue
		}
		code, err := dartcode.New(env).Compile(prog)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(codeOut)
			os.Remove(outPath)
			fmt.Fprintln(os.Stderr, "compile", q, err)
			continue
		}
		if err := os.WriteFile(codeOut, code, 0o644); err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(outPath)
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}

		tmp := filepath.Join(os.TempDir(), q+".dart")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			os.Remove(outPath)
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		if dartExe, err := exec.LookPath("dart"); err == nil {
			cmd := exec.Command(dartExe, tmp)
			out, err := cmd.CombinedOutput()
			if err != nil {
				os.WriteFile(errPath, []byte(err.Error()+"\n"+string(out)), 0o644)
				os.Remove(outPath)
				fmt.Fprintf(os.Stderr, "run %s: %v\n", q, err)
				continue
			}
			os.WriteFile(outPath, bytes.TrimSpace(out), 0o644)
			os.Remove(errPath)
		}
		fmt.Println("generated", codeOut)
	}
}
