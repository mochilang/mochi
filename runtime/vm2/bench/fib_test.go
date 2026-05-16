package bench

import (
	"testing"

	vm2 "mochi/runtime/vm2"
)

// buildFibProgram builds a vm2 Program for:
//
//	func fib(n) {
//	    if n < 2 { return n }
//	    return fib(n-1) + fib(n-2)
//	}
//	main() = fib(N)
func buildFibProgram(n int64) *vm2.Program {
	const fibIdx = 1

	// fib: NumParams=1 (r0=n), NumRegs=8
	// r0: n
	// r1: scratch (const / less result)
	// r2: arg for fib(n-1)
	// r3: result of fib(n-1)
	// r4: arg for fib(n-2)
	// r5: result of fib(n-2)
	// r6: sum
	fib := &vm2.Function{
		Name:      "fib",
		NumParams: 1,
		NumRegs:   8,
		Consts:    []vm2.Cell{vm2.CInt(2), vm2.CInt(1)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 1, B: 0},               // r1 = 2
			{Op: vm2.OpLessI64, A: 1, B: 0, C: 1},            // r1 = n < 2
			{Op: vm2.OpJumpIfFalse, A: 1, B: 4},              // if !(n<2) goto 4
			{Op: vm2.OpReturn, A: 0},                         // return n
			{Op: vm2.OpLoadConstI, A: 1, B: 1},               // r1 = 1
			{Op: vm2.OpSubI64, A: 2, B: 0, C: 1},             // r2 = n - 1
			{Op: vm2.OpCall, A: 3, B: fibIdx, C: 2, D: 1},    // r3 = fib(r2)
			{Op: vm2.OpLoadConstI, A: 1, B: 0},               // r1 = 2
			{Op: vm2.OpSubI64, A: 4, B: 0, C: 1},             // r4 = n - 2
			{Op: vm2.OpCall, A: 5, B: fibIdx, C: 4, D: 1},    // r5 = fib(r4)
			{Op: vm2.OpAddI64, A: 6, B: 3, C: 5},             // r6 = r3 + r5
			{Op: vm2.OpReturn, A: 6},                         // return r6
		},
	}

	main := &vm2.Function{
		Name:      "main",
		NumParams: 0,
		NumRegs:   4,
		Consts:    []vm2.Cell{vm2.CInt(n)},
		Code: []vm2.Instr{
			{Op: vm2.OpLoadConstI, A: 0, B: 0},            // r0 = n
			{Op: vm2.OpCall, A: 1, B: fibIdx, C: 0, D: 1}, // r1 = fib(r0)
			{Op: vm2.OpReturn, A: 1},                      // return r1
		},
	}

	return &vm2.Program{Funcs: []*vm2.Function{main, fib}, Main: 0}
}

func fibGold(n int64) int64 {
	if n < 2 {
		return n
	}
	return fibGold(n-1) + fibGold(n-2)
}

func TestFib10(t *testing.T) {
	p := buildFibProgram(10)
	v := vm2.New(p)
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if !got.IsInt() {
		t.Fatalf("result not int: %#v", got)
	}
	if got.Int() != fibGold(10) {
		t.Fatalf("fib(10) = %d, want %d", got.Int(), fibGold(10))
	}
}

func TestFib20(t *testing.T) {
	p := buildFibProgram(20)
	v := vm2.New(p)
	got, err := v.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if got.Int() != fibGold(20) {
		t.Fatalf("fib(20) = %d, want %d", got.Int(), fibGold(20))
	}
}

func BenchmarkFib25(b *testing.B) {
	p := buildFibProgram(25)
	v := vm2.New(p)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = v.Run()
	}
}
