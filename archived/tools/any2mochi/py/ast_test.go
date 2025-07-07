//go:build archived && slow

package py

import (
	"bytes"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func TestConvertAST(t *testing.T) {
	pySrc := "def main():\n    print('hi')\n\nmain()"
	out, err := ConvertAST(pySrc)
	if err != nil {
		t.Fatalf("convert: %v", err)
	}
	expected := "print(\"hi\")\n"
	if string(out) != expected {
		t.Fatalf("unexpected output:\n%s", out)
	}
	prog, err := parser.ParseString(string(out))
	if err != nil {
		t.Fatalf("parse mochi: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}
	if strings.TrimSpace(buf.String()) != "hi" {
		t.Fatalf("unexpected runtime output: %s", buf.String())
	}
}
