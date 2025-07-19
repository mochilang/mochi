//go:build slow

package interpreter_test

import (
	"os"
	"sort"
	"strings"
	"testing"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func runProgram(t *testing.T, src string, plan string) string {
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	modRoot, _ := mod.FindRoot(".")
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	out := &strings.Builder{}
	interp := interpreter.New(prog, env, modRoot)
	interp.SetDataPlan(plan)
	interp.Env().SetWriter(out)
	if err := interp.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}
	return out.String()
}

func TestDuckDBMatchesMemory(t *testing.T) {
	files := []string{
		"../tests/interpreter/valid/dataset_sort_take_limit.mochi",
		"../tests/interpreter/valid/inner_join.mochi",
	}
	for _, f := range files {
		src, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		mem := runProgram(t, string(src), "memory")
		duck := runProgram(t, string(src), "duckdb")
		if normalize(mem) != normalize(duck) {
			t.Fatalf("mismatch for %s\nmem:\n%s\nduck:\n%s", f, mem, duck)
		}
	}
}

func normalize(s string) string {
	lines := strings.Split(strings.TrimSpace(s), "\n")
	if len(lines) <= 1 {
		return s
	}
	header := lines[0]
	rest := lines[1:]
	sort.Strings(rest)
	return header + "\n" + strings.Join(rest, "\n")
}
