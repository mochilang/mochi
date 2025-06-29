package stackvm

import (
	"bytes"
	"testing"

	"mochi/parser"
	vm "mochi/runtime/vm"
	"mochi/types"
)

const fibRecSource = `fun fib(n: int): int {
  if n <= 1 { return n }
  return fib(n - 1) + fib(n - 2)
}

print(fib(20))
`

func BenchmarkFibRecStackVM(b *testing.B) {
	prog, err := parser.ParseString(fibRecSource)
	if err != nil {
		b.Fatal(err)
	}
	p, err := Compile(prog)
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var out bytes.Buffer
		v := New(p, &out)
		if err := v.Run(); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkFibRecVM(b *testing.B) {
	prog, err := parser.ParseString(fibRecSource)
	if err != nil {
		b.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		b.Fatalf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		var out bytes.Buffer
		m := vm.New(p, &out)
		if err := m.Run(); err != nil {
			b.Fatal(err)
		}
	}
}
