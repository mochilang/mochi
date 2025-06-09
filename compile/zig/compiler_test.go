package zigcode_test

import (
	"strings"
	"testing"

	zigcode "mochi/compile/zig"
	"mochi/parser"
	"mochi/types"
)

func TestZigCompiler_PrintHello(t *testing.T) {
	prog, err := parser.ParseString("print(\"hello\")")
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	c := zigcode.New(env)
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatal(err)
	}
	want := "const std = @import(\"std\");\n\npub fn main() !void {\n\tstd.debug.print(\"{}\\n\", .{\"hello\"});\n}\n"
	if strings.TrimSpace(string(code)) != strings.TrimSpace(want) {
		t.Errorf("unexpected output:\n%s\nwant:\n%s", code, want)
	}
}
