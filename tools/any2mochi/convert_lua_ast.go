package any2mochi

import (
	"fmt"
	"strings"
)

// ConvertLuaAST converts a luaChunk (parsed Lua AST) to Mochi source.
func ConvertLuaAST(c *luaChunk) ([]byte, error) {
	var out strings.Builder
	for _, st := range c.Stmts {
		writeLuaStmtAST(&out, st, 0)
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols")
	}
	return []byte(out.String()), nil
}

func writeLuaStmtAST(out *strings.Builder, st luaStmt, indent int) {
	ind := strings.Repeat("  ", indent)
	switch st.Type {
	case "funcdef", "localfunc":
		out.WriteString(ind)
		out.WriteString("fun ")
		out.WriteString(st.Name)
		out.WriteByte('(')
		for i, p := range st.Func.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteByte(')')
		out.WriteString(" {\n")
		for _, b := range st.Func.Body {
			writeLuaStmtAST(out, b, indent+1)
		}
		out.WriteString(ind)
		out.WriteString("}\n")
	case "call":
		out.WriteString(ind)
		out.WriteString(st.Value)
		out.WriteByte('\n')
	case "return":
		out.WriteString(ind)
		out.WriteString("return ")
		out.WriteString(st.Value)
		out.WriteByte('\n')
	case "block":
		for _, b := range st.Stmts {
			writeLuaStmtAST(out, b, indent)
		}
	case "local":
		out.WriteString(ind)
		out.WriteString("let ")
		out.WriteString(st.Name)
		if st.Value != "" {
			out.WriteString(" = ")
			out.WriteString(st.Value)
		}
		out.WriteByte('\n')
	}
}
