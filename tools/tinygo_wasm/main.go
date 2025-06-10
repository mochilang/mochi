//go:build tinygo

package main

import (
	"strings"
	"syscall/js"

	"mochi/interpreter"
	"mochi/parser"
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
	if errs := types.Check(prog, env); len(errs) > 0 {
		var sb strings.Builder
		for _, e := range errs {
			sb.WriteString(e.Error())
			sb.WriteByte('\n')
		}
		return js.ValueOf(sb.String())
	}
	interp := interpreter.New(prog, env)
	if err := interp.Run(); err != nil {
		return js.ValueOf(err.Error())
	}
	return js.ValueOf("ok")
}

func main() {
	js.Global().Set("runMochi", js.FuncOf(runMochi))
	select {}
}
