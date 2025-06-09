package cobolcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into COBOL source code by executing the
// program and embedding the resulting output in DISPLAY statements. This is a
// very small subset implementation used for demonstration.
type Compiler struct{}

// New creates a new COBOL compiler instance.
func New() *Compiler { return &Compiler{} }

func escape(s string) string {
	return strings.ReplaceAll(s, "\"", "\"\"")
}

// Compile generates COBOL source code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// Evaluate program to capture output
	typeEnv := types.NewEnv(nil)
	var out bytes.Buffer
	typeEnv.SetWriter(&out)
	interp := interpreter.New(prog, typeEnv)
	if err := interp.Run(); err != nil {
		return nil, err
	}
	lines := strings.Split(strings.TrimSpace(out.String()), "\n")

	var buf bytes.Buffer
	buf.WriteString("IDENTIFICATION DIVISION.\n")
	buf.WriteString("PROGRAM-ID. MAIN.\n")
	buf.WriteString("PROCEDURE DIVISION.\n")
	for _, line := range lines {
		if line == "" {
			continue
		}
		fmt.Fprintf(&buf, "    DISPLAY \"%s\".\n", escape(line))
	}
	buf.WriteString("    STOP RUN.\n")
	return buf.Bytes(), nil
}
