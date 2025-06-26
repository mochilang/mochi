//go:build tinygo

package main

import (
	"os"
	"strings"
	"syscall/js"

	"mochi/parser"
	"mochi/runtime/mod"
	vm "mochi/runtime/vm"
	"mochi/types"
)

func runMochi(this js.Value, args []js.Value) any {
	if len(args) < 1 {
		return js.ValueOf("missing source")
	}
	src := args[0].String()
	prog, err := parser.ParseString(src)
	if err != nil {
		return js.ValueOf(err.Error())
	}
	env := types.NewEnv(nil)
	modRoot, errRoot := mod.FindRoot(".")
	if errRoot != nil {
		modRoot = "."
	}
	if errs := types.Check(prog, env); len(errs) > 0 {
		var sb strings.Builder
		for _, e := range errs {
			sb.WriteString(e.Error())
			sb.WriteByte('\n')
		}
		return js.ValueOf(sb.String())
	}
	os.Setenv("MOCHI_ROOT", modRoot)
	p, errc := vm.Compile(prog, env)
	if errc != nil {
		return js.ValueOf(errc.Error())
	}
	m := vm.New(p, nil)
	if err := m.Run(); err != nil {
		return js.ValueOf(err.Error())
	}
	return js.ValueOf("ok")
}

func main() {
	js.Global().Set("runMochi", js.FuncOf(runMochi))
	select {}
}
