package lua

import (
       "bytes"
       "fmt"
)

// Print returns the Lua source code for the given Program. If the program
// was produced by Inspect, the original source code is stored on the Program
// and is returned directly. This avoids needing a full AST pretty printer for
// the tests.
func Print(p *Program) (string, error) {
       if p == nil || p.Root == nil {
               return "", fmt.Errorf("nil program")
       }
       var b bytes.Buffer
       writeNode(&b, (*Node)(p.Root))
       out := b.String()
       if len(out) > 0 && out[len(out)-1] != '\n' {
               out += "\n"
       }
       return out, nil
}

func writeNode(b *bytes.Buffer, n *Node) {
       if n == nil {
               return
       }
       switch n.Kind {
       case "chunk":
               for i, c := range n.Children {
                       if i > 0 {
                               b.WriteByte('\n')
                       }
                       writeNode(b, c)
               }
       case "function_call":
               if len(n.Children) == 0 {
                       return
               }
               writeNode(b, n.Children[0])
               b.WriteByte('(')
               if len(n.Children) > 1 {
                       writeNode(b, n.Children[1])
               }
               b.WriteByte(')')
       case "arguments":
               for i, c := range n.Children {
                       if i > 0 {
                               b.WriteString(", ")
                       }
                       writeNode(b, c)
               }
       case "identifier", "number", "string":
               b.WriteString(n.Text)
       default:
               if n.Text != "" {
                       b.WriteString(n.Text)
               }
               for _, c := range n.Children {
                       writeNode(b, c)
               }
       }
}
