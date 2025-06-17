package dartcode

import (
	"fmt"
	"strings"

	"mochi/parser"
)

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

var dartReserved = map[string]struct{}{
	"import": {}, "class": {}, "void": {}, "int": {}, "double": {}, "var": {}, "for": {}, "if": {}, "else": {}, "return": {}, "true": {}, "false": {},
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	res := b.String()
	if _, ok := dartReserved[res]; ok {
		return "_" + res
	}
	return res
}

func (c *Compiler) newVar() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}

func dartType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "bool":
			return "bool"
		case "string":
			return "String"
		default:
			return sanitizeName(*t.Simple)
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem := dartType(t.Generic.Args[0])
			if elem == "int" || elem == "double" || elem == "bool" || elem == "String" {
				return "List<" + elem + ">"
			}
		}
		return ""
	}
	return ""
}
