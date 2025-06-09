package cobolcode_test

import (
	"bytes"
	"testing"

	cobolcode "mochi/compile/cobol"
	"mochi/parser"
)

func TestCobolCompiler_Basic(t *testing.T) {
	src := "print(\"hello\")\nprint(1 + 2)"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	c := cobolcode.New()
	code, err := c.Compile(prog)
	if err != nil {
		t.Fatalf("compile error: %v", err)
	}
	expected := "IDENTIFICATION DIVISION.\n" +
		"PROGRAM-ID. MAIN.\n" +
		"PROCEDURE DIVISION.\n" +
		"    DISPLAY \"hello\".\n" +
		"    DISPLAY \"3\".\n" +
		"    STOP RUN.\n"
	if !bytes.Equal(bytes.TrimSpace(code), bytes.TrimSpace([]byte(expected))) {
		t.Fatalf("unexpected code:\n%s", code)
	}
}
