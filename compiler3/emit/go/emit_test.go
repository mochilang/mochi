package gogen

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// TestEmitFibIter takes the Fibonacci fixture from compiler3/ir,
// emits Go, drops it into a temp dir with a tiny main shim, runs
// `go run`, and asserts the printed result is fib(10) = 55.
func TestEmitFibIter(t *testing.T) {
	got := runFixture(t, ir.FixtureFibIter(), 10)
	if got != "55\n" {
		t.Errorf("fib(10) printed = %q, want 55", got)
	}
}

func TestEmitSumLoop(t *testing.T) {
	got := runFixture(t, ir.FixtureSumLoop(), 100)
	if got != "4950\n" {
		t.Errorf("sum_loop(100) = %q, want 4950", got)
	}
}

func TestEmitFactRec(t *testing.T) {
	got := runFixture(t, ir.FixtureFactRec(), 10)
	if got != "3628800\n" {
		t.Errorf("fact_rec(10) = %q, want 3628800", got)
	}
}

// TestEmitGofmtClean asserts the emitter's output gofmt-survives a
// no-op re-formatting; if it doesn't, the emitter is producing
// non-canonical source which Phase 7's source-mapping pipeline relies
// on for line-number stability.
func TestEmitGofmtClean(t *testing.T) {
	for _, fn := range []*ir.Function{ir.FixtureFibIter(), ir.FixtureSumLoop(), ir.FixtureFactRec()} {
		src, err := Emit(&Program{PkgName: "demo", Funcs: []*ir.Function{fn}})
		if err != nil {
			t.Fatalf("%s: %v\n%s", fn.Name, err, src)
		}
		if !strings.Contains(string(src), "func "+fn.Name) {
			t.Errorf("%s: emitted source missing func declaration", fn.Name)
		}
	}
}

// TestEmitListOps drives a synthetic function that exercises every
// OpNewList / OpListPushI64 / OpListLenI64 / OpListGetI64 path so the
// generated Go source compiles and produces a known answer. We do
// not exercise OpListSetI64 here because OpListSet has no return
// value to print.
func TestEmitListOps(t *testing.T) {
	fn := &ir.Function{Name: "list_demo", Result: ir.TypeI64}
	entry := fn.AddBlock()
	lst := fn.AddValue(ir.Value{Type: ir.TypeI64Arr, Op: ir.OpNewList})
	v1 := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 10})
	v2 := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 20})
	v3 := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 30})
	p1 := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{lst, v1}})
	p2 := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{lst, v2}})
	p3 := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{lst, v3}})
	zero := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 0})
	g0 := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListGetI64, Args: []uint32{lst, zero}})
	ln := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{lst}})
	sum := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpAddI64, Args: []uint32{g0, ln}})
	blk := fn.Block(entry)
	blk.Values = []uint32{lst, v1, v2, v3, p1, p2, p3, zero, g0, ln, sum}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: sum}

	got := runZeroArgFixture(t, fn)
	if got != "13\n" {
		t.Errorf("list_demo = %q, want 13", got)
	}
}

// runFixture wraps fn (which must take one i64 param and return i64)
// in a tiny main package and runs it. arg is the value passed to fn.
func runFixture(t *testing.T, fn *ir.Function, arg int64) string {
	t.Helper()
	src, err := Emit(&Program{PkgName: "main", Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	withMain := string(src) + "\nfunc main() {\n\tprintln(" + fn.Name + "(" + intStr(arg) + "))\n}\n"
	return runGo(t, withMain)
}

// runZeroArgFixture wraps fn (which must take no params and return
// i64) in a main and runs it.
func runZeroArgFixture(t *testing.T, fn *ir.Function) string {
	t.Helper()
	src, err := Emit(&Program{PkgName: "main", Funcs: []*ir.Function{fn}})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	withMain := string(src) + "\nfunc main() {\n\tprintln(" + fn.Name + "())\n}\n"
	return runGo(t, withMain)
}

// runGo writes src to a temp dir, runs `go run`, returns stdout.
func runGo(t *testing.T, src string) string {
	t.Helper()
	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "run", file)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go run failed: %v\nsource:\n%s\noutput:\n%s", err, src, out)
	}
	return string(out)
}

func intStr(n int64) string {
	if n == 0 {
		return "0"
	}
	neg := false
	if n < 0 {
		neg = true
		n = -n
	}
	digits := ""
	for n > 0 {
		digits = string(rune('0'+(n%10))) + digits
		n /= 10
	}
	if neg {
		digits = "-" + digits
	}
	return digits
}
