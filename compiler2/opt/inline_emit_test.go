package opt_test

import (
	"testing"

	"mochi/compiler2/emit"
	"mochi/compiler2/ir"
	"mochi/compiler2/opt"
	"mochi/runtime/vm2"
)

// Reuse the branchy-callee module from inline_test.go via copy here
// so we can run it end-to-end through emit + vm2.
func buildSignViaCall() *ir.Module {
	const signIdx = 1

	bMain := ir.NewBuilder("main", nil, ir.TI64)
	x := bMain.ConstI64(-7)
	r := bMain.Call(signIdx, ir.TI64, x)
	bMain.Ret(r)

	bSign := ir.NewBuilder("sign", []ir.Type{ir.TI64}, ir.TI64)
	sx := bSign.Param(0)
	neg := bSign.NewBlock()
	chkZero := bSign.NewBlock()
	zero := bSign.NewBlock()
	pos := bSign.NewBlock()
	bSign.CondBr(bSign.LessI64(sx, bSign.ConstI64(0)), neg, chkZero)
	bSign.SwitchTo(neg)
	bSign.Ret(bSign.ConstI64(-1))
	bSign.SwitchTo(chkZero)
	bSign.CondBr(bSign.EqualI64(sx, bSign.ConstI64(0)), zero, pos)
	bSign.SwitchTo(zero)
	bSign.Ret(bSign.ConstI64(0))
	bSign.SwitchTo(pos)
	bSign.Ret(bSign.ConstI64(1))

	return &ir.Module{Funcs: []*ir.Function{bMain.Function(), bSign.Function()}, Main: 0}
}

func TestInlineEndToEnd_Negative(t *testing.T) {
	m := buildSignViaCall()
	if n := opt.LeafInline(m); n != 1 {
		t.Fatalf("LeafInline count = %d", n)
	}
	for _, f := range m.Funcs {
		opt.DCE(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	got, err := vm2.New(prog).Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if !got.IsInt() || got.Int() != -1 {
		t.Fatalf("sign(-7) = %v, want -1", got)
	}
}
