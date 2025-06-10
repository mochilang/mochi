//go:build js && wasm

package main

import (
	"bytes"
	"strings"
	"syscall/js"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

func runMochi(this js.Value, args []js.Value) any {
	if len(args) < 1 {
		return js.ValueOf("missing code")
	}
	code := args[0].String()

	prog, err := parser.ParseString(code)
	if err != nil {
		return js.ValueOf(err.Error())
	}

	env := types.NewEnv(nil)
	var buf bytes.Buffer
	env.SetWriter(&buf)

	if errs := types.Check(prog, env); len(errs) > 0 {
		var sb strings.Builder
		for _, e := range errs {
			sb.WriteString(e.Error())
			sb.WriteString("\n")
		}
		return js.ValueOf(sb.String())
	}

	i := interpreter.New(prog, env)
	if err := i.Run(); err != nil {
		return js.ValueOf(err.Error())
	}

	return js.ValueOf(buf.String())
}

func main() {
	js.Global().Set("runMochi", js.FuncOf(runMochi))
	select {}
}
