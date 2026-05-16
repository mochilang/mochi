package bench

import (
	"testing"

	"mochi/runtime/vm2"
)

// fibProgram hand-builds the bytecode for:
//
//	fn fib(n int) int {
//	  if n < 2 { return n }
//	  return fib(n-1) + fib(n-2)
//	}
//	fn main() int { return fib(N) }
//
// Layout (no globals):
//   fib regs: r0=n(param), r1=2, r2=cmp, r3=1, r4=n-1, r5=fib(n-1),
//             r6=2(const), r7=n-2, r8=fib(n-2), r9=sum
//   main regs: r0=N, r1=result
func fibProgram(n int) *vm2.Program {
	fib := vm2.Function{
		Name:      "fib",
		NumParams: 1,
		NumRegs:   10,
		Code: []vm2.Instr{
			{Op: vm2.OpConst, A: 1, Val: vm2.VInt(2)},          // r1 = 2
			{Op: vm2.OpLess, A: 2, B: 0, C: 1},                 // r2 = n < 2
			{Op: vm2.OpJumpIfFalse, A: 4, B: 2},                // if !r2 jump 4
			{Op: vm2.OpReturn, A: 0},                           // return n
			{Op: vm2.OpConst, A: 3, Val: vm2.VInt(1)},          // r3 = 1
			{Op: vm2.OpSub, A: 4, B: 0, C: 3},                  // r4 = n - 1
			{Op: vm2.OpCall, A: 5, B: 1, C: 1, D: 4},           // r5 = fib(r4)
			{Op: vm2.OpConst, A: 6, Val: vm2.VInt(2)},          // r6 = 2
			{Op: vm2.OpSub, A: 7, B: 0, C: 6},                  // r7 = n - 2
			{Op: vm2.OpCall, A: 8, B: 1, C: 1, D: 7},           // r8 = fib(r7)
			{Op: vm2.OpAdd, A: 9, B: 5, C: 8},                  // r9 = r5 + r8
			{Op: vm2.OpReturn, A: 9},                           // return r9
		},
	}
	main := vm2.Function{
		Name:    "main",
		NumRegs: 2,
		Code: []vm2.Instr{
			{Op: vm2.OpConst, A: 0, Val: vm2.VInt(n)}, // r0 = N
			{Op: vm2.OpCall, A: 1, B: 1, C: 1, D: 0},  // r1 = fib(r0)
			{Op: vm2.OpReturn, A: 1},
		},
	}
	return &vm2.Program{Funcs: []vm2.Function{main, fib}}
}

func TestFib10(t *testing.T) {
	m := vm2.New(fibProgram(10), vm2.Discard())
	v, err := m.Run()
	if err != nil {
		t.Fatal(err)
	}
	if v.AsInt() != 55 {
		t.Fatalf("fib(10)=%d, want 55", v.AsInt())
	}
}

func BenchmarkFib25(b *testing.B) {
	prog := fibProgram(25)
	m := vm2.New(prog, vm2.Discard())
	// warm quickening + IC
	if _, err := m.Run(); err != nil {
		b.Fatal(err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := m.Run(); err != nil {
			b.Fatal(err)
		}
	}
}
