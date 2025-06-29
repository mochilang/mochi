package vm

import (
	"io"
	"mochi/parser"
	"mochi/types"
	"testing"
)

const joinEqualSrc = `
var customers = []
for i in 1..1000 {
  customers = append(customers, { id: i, name: "C" + str(i) })
}
var orders = []
for i in 1..1000 {
  orders = append(orders, { id: i, customerId: i })
}
let repeat = 50
var last = 0
for r in 0..repeat {
  let result = from o in orders
               join from c in customers on o.customerId == c.id
               select o.id + c.id
  last = count(result)
}
`

const joinAddSubSrc = `
var customers = []
for i in 1..1000 {
  customers = append(customers, { id: i, name: "C" + str(i) })
}
var orders = []
for i in 1..1000 {
  orders = append(orders, { id: i, customerId: i })
}
let repeat = 50
var last = 0
for r in 0..repeat {
  let result = from o in orders
               join from c in customers on o.customerId + 1 - 1 == c.id
               select o.id + c.id
  last = count(result)
}
`

const joinShiftSrc = `
var customers = []
for i in 1..1000 {
  customers = append(customers, { id: i, name: "C" + str(i) })
}
var orders = []
for i in 1..1000 {
  orders = append(orders, { id: i, customerId: i })
}
let repeat = 50
var last = 0
for r in 0..repeat {
  let result = from o in orders
               join from c in customers on o.customerId + 1 == c.id
               select o.id + c.id
  last = count(result)
}
`

func benchJoin(b *testing.B, src string) {
	prog, err := parser.ParseString(src)
	if err != nil {
		b.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		b.Fatalf("type error: %v", errs[0])
	}
	p, err := Compile(prog, env)
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		m := New(p, io.Discard)
		if err := m.Run(); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkJoinEqual(b *testing.B)  { benchJoin(b, joinEqualSrc) }
func BenchmarkJoinAddSub(b *testing.B) { benchJoin(b, joinAddSubSrc) }
func BenchmarkJoinShift(b *testing.B)  { benchJoin(b, joinShiftSrc) }
