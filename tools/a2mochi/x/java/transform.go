//go:build slow

package java

import (
	"fmt"
	"strconv"
	"strings"

	mochias "mochi/ast"
)

// Transform converts the parsed Java Node into a Mochi AST node.
func Transform(n *Node) (*mochias.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}
	mutated := map[string]bool{}
	scanMutations(n.Body, mutated)
	prog := &mochias.Node{Kind: "program"}
	for _, st := range n.Body {
		prog.Children = append(prog.Children, stmtNode(st, mutated))
	}
	return prog, nil
}

// TransformFile reads a Java file and converts it to a Mochi AST node.
func TransformFile(path string) (*mochias.Node, error) {
	n, err := ParseFile(path)
	if err != nil {
		return nil, err
	}
	return Transform(n)
}

func stmtNode(s Stmt, mutated map[string]bool) *mochias.Node {
	switch s.Kind {
	case "VarDecl":
		kind := "var"
		if s.Expr != nil && !mutated[s.Name] {
			kind = "let"
		}
		n := &mochias.Node{Kind: kind, Value: s.Name}
		if s.Expr != nil {
			n.Children = append(n.Children, exprNode(*s.Expr))
		} else if s.Type != "" {
			n.Children = append(n.Children, &mochias.Node{Kind: "type", Value: normalizeType(s.Type)})
		} else {
			n.Children = append(n.Children, &mochias.Node{Kind: "int", Value: 0})
		}
		return n
	case "Assign":
		if s.Target != nil {
			lhs := exprNode(*s.Target)
			return &mochias.Node{Kind: "assign", Children: []*mochias.Node{lhs, exprNode(*s.Expr)}}
		}
		return &mochias.Node{Kind: "assign", Value: s.Name, Children: []*mochias.Node{exprNode(*s.Expr)}}
	case "Print":
		call := &mochias.Node{Kind: "call", Value: "print"}
		if s.Expr != nil {
			if args, ok := splitConcat(*s.Expr); ok && len(args) > 0 {
				merged := mergeStrings(args)
				call.Children = append(call.Children, merged...)
			} else {
				call.Children = append(call.Children, exprNode(*s.Expr))
			}
		}
		return call
	case "FnDecl":
		fn := &mochias.Node{Kind: "fun", Value: s.Name}
		for _, p := range s.Params {
			param := &mochias.Node{Kind: "param", Value: p.Name}
			if p.Type != "" {
				param.Children = append(param.Children, &mochias.Node{Kind: "type", Value: normalizeType(p.Type)})
			}
			fn.Children = append(fn.Children, param)
		}
		if s.Type != "" && s.Type != "void" {
			fn.Children = append(fn.Children, &mochias.Node{Kind: "type", Value: normalizeType(s.Type)})
		}
		fn.Children = append(fn.Children, blockNode(s.Body, mutated))
		return fn
	case "Return":
		n := &mochias.Node{Kind: "return"}
		if s.Expr != nil {
			n.Children = append(n.Children, exprNode(*s.Expr))
		}
		return n
	case "ForRange":
		r := &mochias.Node{Kind: "range", Children: []*mochias.Node{exprNode(*s.Start), exprNode(*s.End)}}
		b := blockNode(s.Body, mutated)
		return &mochias.Node{Kind: "for", Value: s.Name, Children: []*mochias.Node{r, b}}
	case "ForEach":
		in := &mochias.Node{Kind: "in", Children: []*mochias.Node{exprNode(*s.Expr)}}
		b := blockNode(s.Body, mutated)
		return &mochias.Node{Kind: "for", Value: s.Name, Children: []*mochias.Node{in, b}}
	case "While":
		b := blockNode(s.Body, mutated)
		if s.Cond != nil {
			return &mochias.Node{Kind: "while", Children: []*mochias.Node{exprNode(*s.Cond), b}}
		}
		return &mochias.Node{Kind: "while", Children: []*mochias.Node{exprNode(*s.Expr), b}}
	case "Break":
		return &mochias.Node{Kind: "break"}
	case "Continue":
		return &mochias.Node{Kind: "continue"}
	case "If":
		thenN := blockNode(s.Then, mutated)
		cond := s.Expr
		if s.Cond != nil {
			cond = s.Cond
		}
		n := &mochias.Node{Kind: "if", Children: []*mochias.Node{exprNode(*cond), thenN}}
		if len(s.Else) > 0 {
			n.Children = append(n.Children, blockNode(s.Else, mutated))
		}
		return n
	default:
		return &mochias.Node{Kind: "unknown"}
	}
}

func blockNode(body []Stmt, mutated map[string]bool) *mochias.Node {
	blk := &mochias.Node{Kind: "block"}
	for _, st := range body {
		blk.Children = append(blk.Children, stmtNode(st, mutated))
	}
	return blk
}

func exprNode(e Expr) *mochias.Node {
	switch e.Kind {
	case "Literal":
		if i, err := strconv.Atoi(e.Value); err == nil {
			return &mochias.Node{Kind: "int", Value: i}
		}
		if f, err := strconv.ParseFloat(e.Value, 64); err == nil {
			return &mochias.Node{Kind: "float", Value: f}
		}
		if e.Value == "true" || e.Value == "false" {
			return &mochias.Node{Kind: "bool", Value: e.Value == "true"}
		}
		return &mochias.Node{Kind: "string", Value: e.Value}
	case "String":
		return &mochias.Node{Kind: "string", Value: e.Value}
	case "Ident":
		return &mochias.Node{Kind: "selector", Value: e.Name}
	case "Binary":
		// special case: string compare via compareTo
		if e.Left != nil && e.Left.Kind == "Call" &&
			e.Right != nil && e.Right.Kind == "Literal" && e.Right.Value == "0" {
			c := e.Left
			if c.Target != nil && c.Target.Kind == "Member" && c.Target.Name == "compareTo" && len(c.Args) == 1 {
				l := exprNode(*c.Target.Expr)
				r := exprNode(c.Args[0])
				return &mochias.Node{Kind: "binary", Value: op(e.Op), Children: []*mochias.Node{l, r}}
			}
		}
		l := exprNode(*e.Left)
		r := exprNode(*e.Right)
		if e.Op == "DIVIDE" && e.Left.Kind == "Literal" && e.Right.Kind == "Literal" &&
			!strings.Contains(e.Left.Value, ".") && !strings.Contains(e.Right.Value, ".") {
			div := &mochias.Node{Kind: "binary", Value: "/", Children: []*mochias.Node{l, r}}
			t := &mochias.Node{Kind: "type", Value: "int"}
			return &mochias.Node{Kind: "cast", Children: []*mochias.Node{div, t}}
		}
		// avoid constant folding to preserve original operation
		return &mochias.Node{Kind: "binary", Value: op(e.Op), Children: []*mochias.Node{l, r}}
	case "Call":
		if n := tryAppend(e); n != nil {
			return n
		}
		if n := tryAvg(e); n != nil {
			return n
		}
		if n := tryParseInt(e); n != nil {
			return n
		}
		if n := trySumBuiltin(e); n != nil {
			return n
		}
		if n := tryMinMaxBuiltin(e); n != nil {
			return n
		}
		// special handling for method calls on an object
		if e.Target != nil && e.Target.Kind == "Member" {
			if p := memberPath(e.Target); p == "java.util.Arrays.copyOfRange" && len(e.Args) == 3 {
				return &mochias.Node{Kind: "index", Children: []*mochias.Node{
					exprNode(e.Args[0]),
					&mochias.Node{Kind: "start", Children: []*mochias.Node{exprNode(e.Args[1])}},
					&mochias.Node{Kind: "end", Children: []*mochias.Node{exprNode(e.Args[2])}},
				}}
			}
			if p := memberPath(e.Target); p == "java.util.Arrays.toString" && len(e.Args) == 1 {
				arg := exprNode(e.Args[0])
				if arg.Kind == "index" {
					return arg
				}
				return &mochias.Node{Kind: "call", Value: "str", Children: []*mochias.Node{arg}}
			}
			switch e.Target.Name {
			case "length":
				if len(e.Args) == 0 {
					return &mochias.Node{Kind: "call", Value: "len", Children: []*mochias.Node{exprNode(*e.Target.Expr)}}
				}
			case "charAt":
				if len(e.Args) == 1 {
					return &mochias.Node{Kind: "index", Children: []*mochias.Node{exprNode(*e.Target.Expr), exprNode(e.Args[0])}}
				}
			case "substring":
				if len(e.Args) == 2 {
					return &mochias.Node{Kind: "call", Value: "substring", Children: []*mochias.Node{exprNode(*e.Target.Expr), exprNode(e.Args[0]), exprNode(e.Args[1])}}
				}
			case "equals":
				if len(e.Args) == 1 {
					l := exprNode(*e.Target.Expr)
					r := exprNode(e.Args[0])
					return &mochias.Node{Kind: "binary", Value: "==", Children: []*mochias.Node{l, r}}
				}
			case "size":
				if len(e.Args) == 0 {
					return &mochias.Node{Kind: "call", Value: "len", Children: []*mochias.Node{exprNode(*e.Target.Expr)}}
				}
			case "valueOf":
				if len(e.Args) == 1 && e.Target.Expr != nil && e.Target.Expr.Kind == "Ident" && e.Target.Expr.Name == "String" {
					return &mochias.Node{Kind: "call", Value: "str", Children: []*mochias.Node{exprNode(e.Args[0])}}
				}
			case "get":
				if len(e.Args) == 1 {
					return &mochias.Node{Kind: "index", Children: []*mochias.Node{exprNode(*e.Target.Expr), exprNode(e.Args[0])}}
				}
			}
		}
		n := &mochias.Node{Kind: "call"}
		n.Children = append(n.Children, exprNode(*e.Target))
		for _, a := range e.Args {
			n.Children = append(n.Children, exprNode(a))
		}
		return n
	case "Member":
		// special case: <expr>.length
		if e.Name == "length" && e.Expr != nil {
			return &mochias.Node{Kind: "call", Value: "len", Children: []*mochias.Node{exprNode(*e.Expr)}}
		}
		return &mochias.Node{Kind: "selector", Value: e.Name, Children: []*mochias.Node{exprNode(*e.Expr)}}
	case "Array":
		arr := &mochias.Node{Kind: "list"}
		for _, el := range e.Elems {
			arr.Children = append(arr.Children, exprNode(el))
		}
		return arr
       case "Index":
               if e.Expr != nil && e.Index != nil {
                       return &mochias.Node{Kind: "index", Children: []*mochias.Node{exprNode(*e.Expr), exprNode(*e.Index)}}
               }
               return &mochias.Node{Kind: "unknown"}
       case "Unary":
               if e.Expr != nil {
                       return &mochias.Node{Kind: "unary", Value: unaryOp(e.Op), Children: []*mochias.Node{exprNode(*e.Expr)}}
               }
               return &mochias.Node{Kind: "unknown"}
       case "Cast":
               if e.Expr != nil {
                       t := &mochias.Node{Kind: "type", Value: normalizeType(e.Value)}
                       return &mochias.Node{Kind: "cast", Children: []*mochias.Node{exprNode(*e.Expr), t}}
               }
               return &mochias.Node{Kind: "unknown"}
       case "Cond":
               if n := trySumWrapper(e); n != nil {
                       return n
               }
		if e.Then != nil && e.Else != nil {
			// special case for boolean expression encoded as 1/0
			if e.Then.Kind == "Literal" && e.Then.Value == "1" &&
				e.Else.Kind == "Literal" && e.Else.Value == "0" {
				return exprNode(*e.Cond)
			}
			return &mochias.Node{Kind: "if_expr", Children: []*mochias.Node{
				exprNode(*e.Cond),
				exprNode(*e.Then),
				exprNode(*e.Else),
			}}
		}
		// fallback: just return condition
		return exprNode(*e.Cond)
	default:
		return &mochias.Node{Kind: "unknown"}
	}
}

func op(k string) string {
	switch k {
	case "PLUS":
		return "+"
	case "MINUS":
		return "-"
	case "MULTIPLY":
		return "*"
	case "DIVIDE":
		return "/"
	case "REMAINDER":
		return "%"
	case "LESS_THAN":
		return "<"
	case "GREATER_THAN":
		return ">"
	case "EQUAL_TO":
		return "=="
	case "NOT_EQUAL_TO":
		return "!="
	case "LESS_THAN_EQUAL":
		return "<="
	case "GREATER_THAN_EQUAL":
		return ">="
	case "CONDITIONAL_AND":
		return "&&"
	case "CONDITIONAL_OR":
		return "||"
	}
       return k
}

func unaryOp(k string) string {
       switch k {
       case "UNARY_MINUS":
               return "-"
       case "UNARY_PLUS":
               return "+"
       case "LOGICAL_COMPLEMENT":
               return "!"
       }
       return k
}

func normalizeType(t string) string {
	switch strings.ToLower(t) {
	case "boolean":
		return "bool"
	case "int", "integer":
		return "int"
	case "float", "double":
		return "float"
	case "string":
		return "string"
	default:
		return t
	}
}

func isNumericLiteral(v string) bool {
	if _, err := strconv.Atoi(v); err == nil {
		return true
	}
	if _, err := strconv.ParseFloat(v, 64); err == nil {
		return true
	}
	return false
}

func splitConcat(e Expr) ([]*mochias.Node, bool) {
	if e.Kind == "Binary" && e.Op == "PLUS" && e.Left != nil && e.Right != nil {
		left, lstr := splitConcat(*e.Left)
		right, rstr := splitConcat(*e.Right)
		if lstr || rstr {
			return append(left, right...), true
		}
	}
	if e.Kind == "String" || (e.Kind == "Literal" && !isNumericLiteral(e.Value)) {
		return []*mochias.Node{exprNode(e)}, true
	}
	return []*mochias.Node{exprNode(e)}, false
}

func mergeStrings(nodes []*mochias.Node) []*mochias.Node {
	if len(nodes) == 0 {
		return nodes
	}
	merged := []*mochias.Node{nodes[0]}
	for _, n := range nodes[1:] {
		if n.Kind == "string" && n.Value == " " {
			continue
		}
		last := merged[len(merged)-1]
		if last.Kind == "string" && n.Kind == "string" {
			last.Value = last.Value.(string) + n.Value.(string)
		} else {
			merged = append(merged, n)
		}
	}
	if len(merged) > 1 && merged[0].Kind == "string" {
		if s, ok := merged[0].Value.(string); ok {
			merged[0].Value = strings.TrimRight(s, " ")
		}
	}
	return merged
}

func memberPath(e *Expr) string {
	if e == nil {
		return ""
	}
	switch e.Kind {
	case "Member":
		if e.Expr != nil {
			p := memberPath(e.Expr)
			if p != "" {
				return p + "." + e.Name
			}
		}
		return e.Name
	case "Ident":
		return e.Name
	default:
		return ""
	}
}

func tryAppend(e Expr) *mochias.Node {
	if e.Kind != "Call" || e.Target == nil || e.Target.Kind != "Member" {
		return nil
	}
	if p := memberPath(e.Target); p != "java.util.Arrays.toString" || len(e.Args) != 1 {
		return nil
	}
	arg := e.Args[0]
	if arg.Kind != "Call" || arg.Target == nil || arg.Target.Kind != "Member" || arg.Target.Name != "toArray" {
		return nil
	}
	inner := arg.Target.Expr
	if inner == nil || inner.Kind != "Call" || inner.Target == nil || inner.Target.Kind != "Member" {
		return nil
	}
	if memberPath(inner.Target) != "java.util.stream.IntStream.concat" || len(inner.Args) != 2 {
		return nil
	}
	first := inner.Args[0]
	second := inner.Args[1]
	if first.Kind != "Call" || memberPath(first.Target) != "java.util.Arrays.stream" || len(first.Args) != 1 {
		return nil
	}
	if second.Kind != "Call" || memberPath(second.Target) != "java.util.stream.IntStream.of" || len(second.Args) != 1 {
		return nil
	}
	arr := exprNode(first.Args[0])
	val := exprNode(second.Args[0])
	return &mochias.Node{Kind: "call", Value: "append", Children: []*mochias.Node{arr, val}}
}

func tryAvg(e Expr) *mochias.Node {
	if e.Kind != "Call" || e.Target == nil || e.Target.Kind != "Member" || e.Target.Name != "orElse" || len(e.Args) != 1 {
		return nil
	}
	mid := e.Target.Expr
	if mid == nil || mid.Kind != "Call" || mid.Target == nil || mid.Target.Kind != "Member" || mid.Target.Name != "average" {
		return nil
	}
	streamCall := mid.Target.Expr
	if streamCall == nil || streamCall.Kind != "Call" || streamCall.Target == nil || streamCall.Target.Kind != "Member" {
		return nil
	}
	if memberPath(streamCall.Target) != "java.util.Arrays.stream" || len(streamCall.Args) != 1 {
		return nil
	}
	arr := exprNode(streamCall.Args[0])
	return &mochias.Node{Kind: "call", Value: "avg", Children: []*mochias.Node{arr}}
}

func tryParseInt(e Expr) *mochias.Node {
	if e.Kind != "Call" || e.Target == nil {
		return nil
	}
	if p := memberPath(e.Target); (p == "java.lang.Integer.parseInt" || p == "Integer.parseInt") && len(e.Args) == 1 {
		val := exprNode(e.Args[0])
		t := &mochias.Node{Kind: "type", Value: "int"}
		return &mochias.Node{Kind: "cast", Children: []*mochias.Node{val, t}}
	}
	return nil
}

func trySumBuiltin(e Expr) *mochias.Node {
	if e.Kind != "Call" || e.Target == nil {
		return nil
	}
	if e.Target.Kind == "Ident" && e.Target.Name == "sum" && len(e.Args) == 1 {
		return &mochias.Node{Kind: "call", Value: "sum", Children: []*mochias.Node{exprNode(e.Args[0])}}
	}
	if e.Target.Kind == "Member" && e.Target.Name == "sum" && len(e.Args) == 0 {
		base := e.Target.Expr
		if base != nil && base.Kind == "Call" && base.Target != nil && base.Target.Kind == "Member" {
			if memberPath(base.Target) == "java.util.Arrays.stream" && len(base.Args) == 1 {
				return &mochias.Node{Kind: "call", Value: "sum", Children: []*mochias.Node{exprNode(base.Args[0])}}
			}
		}
	}
	return nil
}

func tryMinMaxBuiltin(e Expr) *mochias.Node {
	if e.Kind != "Call" || e.Target == nil || len(e.Args) != 1 {
		return nil
	}
	if e.Target.Kind == "Ident" && (e.Target.Name == "min" || e.Target.Name == "max") {
		return &mochias.Node{Kind: "call", Value: e.Target.Name, Children: []*mochias.Node{exprNode(e.Args[0])}}
	}
	return nil
}

func trySumWrapper(e Expr) *mochias.Node {
	if e.Kind != "Cond" || e.Cond == nil {
		return nil
	}
	cond := e.Cond
	if cond.Kind != "Binary" || cond.Op != "EQUAL_TO" || cond.Left == nil || cond.Right == nil {
		return nil
	}
	if cond.Right.Kind != "Literal" || cond.Right.Value != "0" {
		return nil
	}
	left := cond.Left
	if left.Kind != "Binary" || left.Op != "REMAINDER" || left.Left == nil || left.Right == nil {
		return nil
	}
	if left.Right.Kind != "Literal" || left.Right.Value != "1" {
		return nil
	}
	if n := trySumBuiltin(*left.Left); n != nil {
		return n
	}
	return nil
}

func scanMutations(stmts []Stmt, m map[string]bool) {
	for _, st := range stmts {
		switch st.Kind {
		case "Assign":
			if st.Target != nil {
				if n := targetBaseName(st.Target); n != "" {
					m[n] = true
				}
			} else {
				m[st.Name] = true
			}
		case "FnDecl":
			scanMutations(st.Body, m)
		case "While", "ForRange", "ForEach":
			scanMutations(st.Body, m)
		case "If":
			scanMutations(st.Then, m)
			scanMutations(st.Else, m)
		}
	}
}

func targetBaseName(e *Expr) string {
	if e == nil {
		return ""
	}
	switch e.Kind {
	case "Ident":
		return e.Name
	case "Member", "Index":
		return targetBaseName(e.Expr)
	default:
		return ""
	}
}
