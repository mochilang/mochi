//go:build archived

package cobolcode

import (
	"fmt"
	"reflect"
	"strings"

	"mochi/ast"
	"mochi/types"
)

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	if isInt(a) && isInt(b) {
		return true
	}
	if isFloat(a) && isFloat(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

func isInt(t types.Type) bool {
	_, ok := t.(types.IntType)
	return ok
}

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
}

// cobolName converts a Mochi identifier to a COBOL-friendly name.
func cobolName(name string) string {
	up := strings.ToUpper(name)
	switch up {
	case "END", "START":
		return "V_" + up
	}
	return up
}

// writeIndent writes indentation spaces according to c.indent.
func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

// writeln writes a line with indentation and a trailing newline.
func (c *Compiler) writeln(s string) {
	line := strings.TrimRight(s, " \t")
	c.writeIndent()
	c.buf.WriteString(line)
	c.buf.WriteByte('\n')
}

// newTemp returns a new temporary variable name.
func (c *Compiler) newTemp() string {
	name := fmt.Sprintf("TMP%d", c.tmpCounter)
	c.tmpCounter++
	return name
}

func isSimpleExpr(n *ast.Node) bool {
	switch n.Kind {
	case "int", "float", "selector", "string", "bool", "call":
		return true
	case "unary":
		if n.Value == "-" {
			return isSimpleExpr(n.Children[0])
		}
	case "group":
		return isSimpleExpr(n.Children[0])
	}
	return false
}

// picForType returns a picture clause for the given static type.
func (c *Compiler) picForType(t types.Type) string {
	switch t.(type) {
	case types.StringType:
		return "PIC X(100)."
	case types.FloatType:
		return "PIC 9(4)V9(4)."
	}
	return "PIC 9."
}

// declare records a WORKING-STORAGE declaration.
func (c *Compiler) declare(line string) {
	for _, d := range c.decls {
		if d == line {
			return
		}
	}
	c.decls = append(c.decls, line)
}

func extractInt(n *ast.Node) int {
	if n.Kind == "int" {
		switch v := n.Value.(type) {
		case int:
			return v
		case float64:
			return int(v)
		}
	}
	return 0
}

func extractIntList(n *ast.Node) []int {
	if n.Kind != "list" {
		return nil
	}
	res := make([]int, 0, len(n.Children))
	for _, ch := range n.Children {
		res = append(res, extractInt(ch))
	}
	return res
}

// selectorName returns a COBOL-friendly variable name for a selector
// expression. Nested fields are joined using an underscore.
func selectorName(n *ast.Node) string {
	name := cobolName(n.Value.(string))
	if len(n.Children) > 0 && n.Children[0].Kind == "selector" {
		return selectorName(n.Children[0]) + "_" + name
	}
	return name
}

// rootSelector returns the base identifier for a selector expression.
func rootSelector(n *ast.Node) string {
	if n.Kind != "selector" {
		return ""
	}
	if len(n.Children) > 0 && n.Children[0].Kind == "selector" {
		return rootSelector(n.Children[0])
	}
	if s, ok := n.Value.(string); ok {
		return s
	}
	return ""
}

// exprUsesOnly reports whether expression n references only the identifiers in allowed.
func exprUsesOnly(n *ast.Node, allowed map[string]bool) bool {
	if n == nil {
		return true
	}
	if n.Kind == "selector" {
		root := rootSelector(n)
		if root != "" && !allowed[root] {
			return false
		}
	}
	for _, ch := range n.Children {
		if !exprUsesOnly(ch, allowed) {
			return false
		}
	}
	return true
}
