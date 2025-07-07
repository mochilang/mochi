//go:build archived

package cobolcode

import (
	"mochi/ast"
	"mochi/types"
)

// nodeType delegates to types.NodeType.
func (c *Compiler) nodeType(n *ast.Node) types.Type { return types.NodeType(n, c.env) }

// isString reports whether the node has static string type.
func (c *Compiler) isString(n *ast.Node) bool { return types.IsString(n, c.env) }

// isFloat reports whether the node has static float type.
func (c *Compiler) isFloat(n *ast.Node) bool { return types.IsFloat(n, c.env) }

// isList reports whether the node has static list type.
func (c *Compiler) isList(n *ast.Node) bool { return types.IsList(n, c.env) }

func (c *Compiler) picForExpr(n *ast.Node) string {
	switch c.nodeType(n).(type) {
	case types.StringType:
		return "PIC X(100)."
	case types.FloatType:
		return "PIC 9(4)V9(4)."
	default:
		return "PIC 9."
	}
}

func (c *Compiler) picForVar(name string) string {
	if c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			switch t.(type) {
			case types.StringType:
				return "PIC X(100)."
			case types.FloatType:
				return "PIC 9(4)V9(4)."
			}
		}
	}
	return "PIC 9."
}
