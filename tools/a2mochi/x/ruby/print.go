//go:build slow

package ruby

import (
	"bytes"
	"fmt"
	"mochi/ast"
	"mochi/transpiler/meta"
)

// Print returns Mochi source code for the given AST node.
func Print(node *ast.Node) (string, error) {
	if node == nil {
		return "", fmt.Errorf("nil node")
	}
	var srcBuf bytes.Buffer
	if err := ast.Fprint(&srcBuf, node); err != nil {
		return "", err
	}
	var b bytes.Buffer
       b.Write(meta.Header("//"))
       b.Write(srcBuf.Bytes())
       if b.Len() == 0 || b.Bytes()[b.Len()-1] != '\n' {
               b.WriteByte('\n')
       }
       return b.String(), nil
}
