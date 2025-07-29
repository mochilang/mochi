//go:build slow

package cpp

import "mochi/ast"

func ConvertTest(s string) string             { return convertExpression(s) }
func ParseSingle(s string) (*ast.Node, error) { return parseSingle(s) }
func ParseGlobal(s string) Global             { return parseGlobalDecl(s) }
