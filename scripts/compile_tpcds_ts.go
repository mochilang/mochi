//go:build archive

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func main() {
	os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
	defer os.Unsetenv("MOCHI_HEADER_TIME")
	os.Setenv("SOURCE_DATE_EPOCH", "1136214245")
	defer os.Unsetenv("SOURCE_DATE_EPOCH")

	outDir := filepath.Join("tests", "dataset", "tpc-ds", "compiler", "ts")
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
		prog, err := parser.Parse(src)
		if err != nil {
			fmt.Fprintln(os.Stderr, "parse", q, err)
			continue
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			fmt.Fprintln(os.Stderr, "type", q, errs[0])
			continue
		}
		c := tscode.New(env, "")
		code, err := c.Compile(prog)
		if err != nil {
			fmt.Fprintln(os.Stderr, "compile", q, err)
			continue
		}
		codeOut := filepath.Join(outDir, q+".ts")
		if err := os.WriteFile(codeOut, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "write code", q, err)
			continue
		}
		tmp := filepath.Join(os.TempDir(), q+".ts")
		if err := os.WriteFile(tmp, code, 0o644); err != nil {
			fmt.Fprintln(os.Stderr, "tmp write", q, err)
			continue
		}
		cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", "--allow-env", tmp)
		envVars := append(os.Environ(), "DENO_TLS_CA_STORE=system")
		if q == "q76" {
			envVars = append(envVars, "CHANNEL_ORDER=store,web,catalog")
		}
		cmd.Env = envVars
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		outPath := filepath.Join(outDir, q+".out")
		errPath := filepath.Join(outDir, q+".error")
		if err != nil {
			fmt.Fprintf(os.Stderr, "run %s: %v\n", q, err)
			os.WriteFile(errPath, out, 0o644)
			os.Remove(outPath)
		} else {
			cleaned := append(bytes.TrimSpace(out), '\n')
			os.WriteFile(outPath, cleaned, 0o644)
			os.Remove(errPath)
		}
	}
}
