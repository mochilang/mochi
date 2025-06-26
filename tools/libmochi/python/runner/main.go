package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"mochi/runtime/mod"
	"os"
	"strings"

	"mochi/parser"
	vm "mochi/runtime/vm"
	"mochi/types"
)

type result struct {
	Output string `json:"output"`
	Error  string `json:"error,omitempty"`
}

func main() {
	src := os.Getenv("MOCHI_CODE")
	if src == "" {
		fmt.Fprintln(os.Stderr, "MOCHI_CODE not set")
		os.Exit(1)
	}

	res := result{}
	prog, err := parser.ParseString(src)
	if err != nil {
		res.Error = err.Error()
		json.NewEncoder(os.Stdout).Encode(res)
		return
	}

	env := types.NewEnv(nil)
	var out bytes.Buffer
	env.SetWriter(&out)
	modRoot, errRoot := mod.FindRoot(".")
	if errRoot != nil {
		modRoot, _ = os.Getwd()
	}

	if errs := types.Check(prog, env); len(errs) > 0 {
		var sb strings.Builder
		for _, e := range errs {
			sb.WriteString(e.Error())
			sb.WriteByte('\n')
		}
		res.Error = sb.String()
		json.NewEncoder(os.Stdout).Encode(res)
		return
	}

	os.Setenv("MOCHI_ROOT", modRoot)
	p, errc := vm.Compile(prog, env)
	if errc != nil {
		res.Error = errc.Error()
		json.NewEncoder(os.Stdout).Encode(res)
		return
	}
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		res.Error = err.Error()
		json.NewEncoder(os.Stdout).Encode(res)
		return
	}

	res.Output = out.String()
	json.NewEncoder(os.Stdout).Encode(res)
}
