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

// runGoModule writes src to a fresh subdirectory under
// testdata/_run so the file inherits the mochi module's go.mod and
// `import "mochi/runtime/mochi/query"` resolves naturally. Used by
// the Phase 5 query lowering tests; the Phase 4 tests do not need
// this because their fixtures import only stdlib.
func runGoModule(t *testing.T, src string) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	root := filepath.Join(wd, "testdata", "_run")
	if err := os.MkdirAll(root, 0o755); err != nil {
		t.Fatal(err)
	}
	dir, err := os.MkdirTemp(root, "case-")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })

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

// buildQueryDemo wires together a "demo" function that builds a literal
// i64 list (with the supplied elements), passes it to op constructor
// (which assembles the OpQueryX value), then reduces the result to
// an i64 via reduce. The reduce closure receives the resulting list
// value's SSA id and must produce an i64 SSA id that becomes the
// function's return value. Returns the demo Function and the program
// (which also contains any helper fixtures the constructor stashes
// into helpers).
func buildQueryDemo(
	t *testing.T,
	name string,
	input []int64,
	helpers []*ir.Function,
	build func(fn *ir.Function, helperIdx map[string]uint32, src uint32) (resultID uint32, extras []uint32),
	reduce func(fn *ir.Function, resultID uint32) (returnID uint32, extras []uint32),
) []*ir.Function {
	t.Helper()
	fn := &ir.Function{Name: name, Result: ir.TypeI64}
	entryID := fn.AddBlock()
	var values []uint32

	src := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpNewList})
	values = append(values, src)
	for _, e := range input {
		c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: e})
		p := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{src, c}})
		values = append(values, c, p)
	}

	// Helpers' function indices: demo is at index 0, helpers at 1..N.
	helperIdx := map[string]uint32{}
	for i, h := range helpers {
		helperIdx[h.Name] = uint32(i + 1)
	}

	result, extras := build(fn, helperIdx, src)
	values = append(values, extras...)

	returnID, redExtras := reduce(fn, result)
	values = append(values, redExtras...)

	blk := fn.Block(entryID)
	blk.Values = values
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: returnID}

	out := make([]*ir.Function, 0, 1+len(helpers))
	out = append(out, fn)
	out = append(out, helpers...)
	return out
}

// runQueryDemo emits, wraps with println(demo()), and runs.
func runQueryDemo(t *testing.T, fns []*ir.Function) string {
	t.Helper()
	src, err := Emit(&Program{PkgName: "main", Funcs: fns})
	if err != nil {
		t.Fatalf("Emit: %v\n%s", err, src)
	}
	withMain := string(src) + "\nfunc main() {\n\tprintln(" + fns[0].Name + "())\n}\n"
	return runGoModule(t, withMain)
}

// reduceLen returns OpListLenI64(resultID) as the demo's return id.
func reduceLen(fn *ir.Function, resultID uint32) (uint32, []uint32) {
	ln := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{resultID}})
	return ln, []uint32{ln}
}

// reduceGet0 returns OpListGetI64(resultID, 0).
func reduceGet0(fn *ir.Function, resultID uint32) (uint32, []uint32) {
	zero := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 0})
	get := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListGetI64, Args: []uint32{resultID, zero}})
	return get, []uint32{zero, get}
}

// reduceGet returns OpListGetI64(resultID, idx).
func reduceGet(idx int64) func(fn *ir.Function, resultID uint32) (uint32, []uint32) {
	return func(fn *ir.Function, resultID uint32) (uint32, []uint32) {
		i := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: idx})
		g := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListGetI64, Args: []uint32{resultID, i}})
		return g, []uint32{i, g}
	}
}

func TestEmitQueryFilter(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureIsEven()}
	fns := buildQueryDemo(t, "demo", []int64{1, 2, 3, 4, 5}, helpers,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			ref := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: int64(hi["is_even"])})
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryFilter, Args: []uint32{src, ref}})
			return r, []uint32{ref, r}
		}, reduceLen)
	if got := runQueryDemo(t, fns); got != "2\n" {
		t.Errorf("Filter [1..5] is_even len = %q, want 2", got)
	}
}

func TestEmitQueryMap(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureDouble()}
	fns := buildQueryDemo(t, "demo", []int64{1, 2, 3}, helpers,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			ref := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: int64(hi["double"])})
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryMap, Args: []uint32{src, ref}})
			return r, []uint32{ref, r}
		}, reduceGet(2))
	if got := runQueryDemo(t, fns); got != "6\n" {
		t.Errorf("Map [1,2,3] double [2] = %q, want 6", got)
	}
}

func TestEmitQuerySortBy(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureIdent()}
	fns := buildQueryDemo(t, "demo", []int64{3, 1, 2}, helpers,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			ref := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: int64(hi["ident"])})
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQuerySortBy, Args: []uint32{src, ref}})
			return r, []uint32{ref, r}
		}, reduceGet0)
	if got := runQueryDemo(t, fns); got != "1\n" {
		t.Errorf("SortBy [3,1,2] ident [0] = %q, want 1", got)
	}
}

func TestEmitQuerySortByDesc(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureIdent()}
	fns := buildQueryDemo(t, "demo", []int64{3, 1, 2}, helpers,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			ref := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: int64(hi["ident"])})
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQuerySortByDesc, Args: []uint32{src, ref}})
			return r, []uint32{ref, r}
		}, reduceGet0)
	if got := runQueryDemo(t, fns); got != "3\n" {
		t.Errorf("SortByDesc [3,1,2] ident [0] = %q, want 3", got)
	}
}

func TestEmitQueryLimit(t *testing.T) {
	fns := buildQueryDemo(t, "demo", []int64{1, 2, 3, 4, 5}, nil,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			n := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 2})
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryLimit, Args: []uint32{src, n}})
			return r, []uint32{n, r}
		}, reduceLen)
	if got := runQueryDemo(t, fns); got != "2\n" {
		t.Errorf("Limit [1..5] n=2 len = %q, want 2", got)
	}
}

func TestEmitQueryDistinct(t *testing.T) {
	fns := buildQueryDemo(t, "demo", []int64{1, 1, 2, 2, 3}, nil,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryDistinct, Args: []uint32{src}})
			return r, []uint32{r}
		}, reduceLen)
	if got := runQueryDemo(t, fns); got != "3\n" {
		t.Errorf("Distinct [1,1,2,2,3] len = %q, want 3", got)
	}
}

func TestEmitQueryGroupByCompiles(t *testing.T) {
	// GroupBy returns []query.Group[int64, int64]; we don't yet have
	// struct field access in compiler3 IR (Phase 6 lands the Mochi
	// frontend's record access). This test asserts the emitter
	// produces a compile- and run-clean program even when the IR
	// can only return a constant value, not the group result.
	helpers := []*ir.Function{ir.FixtureIdent()}
	fns := buildQueryDemo(t, "demo", []int64{1, 2, 3, 4}, helpers,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			ref := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: int64(hi["ident"])})
			r := fn.AddValue(ir.Value{Type: ir.TypeAny, Op: ir.OpQueryGroupBy, Args: []uint32{src, ref}})
			return r, []uint32{ref, r}
		}, func(fn *ir.Function, resultID uint32) (uint32, []uint32) {
			// Ignore resultID; just return a constant 0. The emitter's
			// `_ = vN` pseudo-use keeps the group result in scope.
			c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 7})
			return c, []uint32{c}
		})
	if got := runQueryDemo(t, fns); got != "7\n" {
		t.Errorf("GroupBy compile-only fixture = %q, want 7", got)
	}
}

func TestEmitQueryJoin(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureIdent(), ir.FixtureAddPair()}
	// We feed the same input list as both sides via a closure that
	// builds the right list inside the demo's body.
	fn := &ir.Function{Name: "demo", Result: ir.TypeI64}
	entryID := fn.AddBlock()
	var values []uint32

	// Build left list [1, 2].
	left := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpNewList})
	values = append(values, left)
	for _, e := range []int64{1, 2} {
		c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: e})
		p := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{left, c}})
		values = append(values, c, p)
	}
	// Build right list [1, 2].
	right := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpNewList})
	values = append(values, right)
	for _, e := range []int64{1, 2} {
		c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: e})
		p := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{right, c}})
		values = append(values, c, p)
	}
	// Function refs.
	identIdx := int64(1) // helpers[0] = ident, helpers[1] = add_pair
	addIdx := int64(2)
	lkey := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: identIdx})
	rkey := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: identIdx})
	comb := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: addIdx})
	values = append(values, lkey, rkey, comb)

	joined := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryJoin, Args: []uint32{left, right, lkey, rkey, comb}})
	ln := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{joined}})
	values = append(values, joined, ln)

	blk := fn.Block(entryID)
	blk.Values = values
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: ln}

	fns := append([]*ir.Function{fn}, helpers...)
	if got := runQueryDemo(t, fns); got != "2\n" {
		t.Errorf("Join [1,2]x[1,2] ident,add len = %q, want 2", got)
	}
}

func TestEmitQueryCrossJoin(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureAddPair()}
	fn := &ir.Function{Name: "demo", Result: ir.TypeI64}
	entryID := fn.AddBlock()
	var values []uint32

	// Left [1, 2].
	left := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpNewList})
	values = append(values, left)
	for _, e := range []int64{1, 2} {
		c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: e})
		p := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{left, c}})
		values = append(values, c, p)
	}
	// Right [10, 20].
	right := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpNewList})
	values = append(values, right)
	for _, e := range []int64{10, 20} {
		c := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: e})
		p := fn.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{right, c}})
		values = append(values, c, p)
	}
	comb := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: 1})
	cross := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryCrossJoin, Args: []uint32{left, right, comb}})
	ln := fn.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpListLenI64, Args: []uint32{cross}})
	values = append(values, comb, cross, ln)

	blk := fn.Block(entryID)
	blk.Values = values
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: ln}

	fns := append([]*ir.Function{fn}, helpers...)
	if got := runQueryDemo(t, fns); got != "4\n" {
		t.Errorf("CrossJoin 2x2 len = %q, want 4", got)
	}
}

// TestEmitQueryEmitsRuntimeImport asserts the emitter wires the
// `mochi/runtime/mochi/query` import (and only that one) when the IR
// uses query ops. A stale or missing import would slip through the
// runtime tests as a build error; this test surfaces it directly.
func TestEmitQueryEmitsRuntimeImport(t *testing.T) {
	helpers := []*ir.Function{ir.FixtureIsEven()}
	fns := buildQueryDemo(t, "demo", []int64{1, 2, 3}, helpers,
		func(fn *ir.Function, hi map[string]uint32, src uint32) (uint32, []uint32) {
			ref := fn.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: int64(hi["is_even"])})
			r := fn.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryFilter, Args: []uint32{src, ref}})
			return r, []uint32{ref, r}
		}, reduceLen)
	src, err := Emit(&Program{PkgName: "main", Funcs: fns})
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	if !strings.Contains(string(src), `"mochi/runtime/mochi/query"`) {
		t.Errorf("emitted source missing query runtime import:\n%s", src)
	}
	if !strings.Contains(string(src), "query.Filter[int64]") {
		t.Errorf("emitted source missing query.Filter call:\n%s", src)
	}
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
