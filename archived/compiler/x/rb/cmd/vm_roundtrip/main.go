//go:build archive

package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	rubyconv "mochi/archived/tools/any2mochi/x/ruby"
	rbcode "mochi/archived/x/rb"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func fileExists(path string) bool {
	if _, err := os.Stat(path); err == nil {
		return true
	}
	return false
}

func main() {
	files, err := filepath.Glob("tests/vm/valid/*.mochi")
	if err != nil {
		fmt.Fprintln(os.Stderr, "glob error:", err)
		os.Exit(1)
	}

	var report strings.Builder
	report.WriteString("# Ruby roundtrip VM test failures\n\n")
	for _, src := range files {
		if err := process(src); err != nil {
			report.WriteString(fmt.Sprintf("## %s\n\n```\n%s\n```\n\n", src, err))
		}
	}
	if report.Len() == 0 {
		report.WriteString("All Ruby roundtrip VM tests passed.\n")
	}
	os.WriteFile("compile/x/rb/ERRORS.md", []byte(report.String()), 0644)
}

func process(src string) error {
	base := strings.TrimSuffix(src, ".mochi")
	rbPath := base + ".rb.out"
	mochiPath := base + ".mochi.out"

	prog, err := parser.Parse(src)
	if err != nil {
		writeErr(base+".rb.error", err)
		return fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeErr(base+".rb.error", errs[0])
		return fmt.Errorf("type error: %v", errs[0])
	}
	code, err := rbcode.New(env).Compile(prog)
	if err != nil {
		writeErr(base+".rb.error", err)
		return fmt.Errorf("compile error: %w", err)
	}
	os.WriteFile(rbPath, code, 0644)

	conv, err := rubyconv.ConvertFile(rbPath)
	if err != nil {
		writeErr(base+".mochi.error", err)
		return fmt.Errorf("ruby2mochi error: %w", err)
	}
	os.WriteFile(mochiPath, conv, 0644)

	rtProg, err := parser.ParseString(string(conv))
	if err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("parse roundtrip error: %w", err)
	}
	if errs := types.Check(rtProg, env); len(errs) > 0 {
		writeErr(base+".vm.error", errs[0])
		return fmt.Errorf("type roundtrip error: %v", errs[0])
	}
	p, err := vm.Compile(rtProg, env)
	if err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("compile roundtrip error: %w", err)
	}
	var in io.Reader = os.Stdin
	if fileExists(base + ".in") {
		f, err := os.Open(base + ".in")
		if err == nil {
			defer f.Close()
			in = f
		}
	}
	var out bytes.Buffer
	m := vm.NewWithIO(p, in, &out)
	if err := m.Run(); err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("run error: %w", err)
	}
	want, err := os.ReadFile(base + ".out")
	if err != nil {
		writeErr(base+".vm.error", err)
		return fmt.Errorf("missing golden output: %v", err)
	}
	got := strings.TrimSpace(out.String())
	if got != strings.TrimSpace(string(want)) {
		writeErr(base+".vm.error", fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, strings.TrimSpace(string(want))))
		return fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, strings.TrimSpace(string(want)))
	}
	os.Remove(base + ".rb.error")
	os.Remove(base + ".mochi.error")
	os.Remove(base + ".vm.error")
	return nil
}

func writeErr(path string, err error) {
	os.WriteFile(path, []byte(err.Error()), 0644)
}
