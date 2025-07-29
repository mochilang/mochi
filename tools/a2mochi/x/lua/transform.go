//go:build slow

package lua

import (
	"fmt"
	"strconv"
	"strings"

	luaast "github.com/yuin/gopher-lua/ast"
	"mochi/ast"
)

// Transform converts a parsed Program into a Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, nil
	}
	mut := collectMutables(p.Stmts)
	root := node("program", nil)
	root.Children = append(root.Children, convertStmtsToNodes(p.Stmts, map[string]bool{}, mut)...)
	return root, nil
}

func node(kind string, value any, children ...*ast.Node) *ast.Node {
	return &ast.Node{Kind: kind, Value: value, Children: children}
}

func convertStmtsToNodes(stmts []luaast.Stmt, vars map[string]bool, mut map[string]bool) []*ast.Node {
	if vars == nil {
		vars = make(map[string]bool)
	}
	var out []*ast.Node
	for _, st := range stmts {
		switch s := st.(type) {
		case *luaast.LocalAssignStmt:
			for i, n := range s.Names {
				if i < len(s.Exprs) {
					if fn, ok := s.Exprs[i].(*luaast.FunctionExpr); ok {
						fnNode := node("fun", n)
						if rt := inferReturnType(fn); rt != "" {
							fnNode.Children = append(fnNode.Children, node("type", rt))
						}
						fnNode.Children = append(fnNode.Children, convertStmtsToNodes(fn.Stmts, map[string]bool{}, mut)...)
						out = append(out, fnNode)
						vars[n] = true
						continue
					}
				}
				var val *ast.Node
				if i < len(s.Exprs) {
					val = exprToNode(s.Exprs[i], mut)
				}
				kind := "let"
				if mut[n] {
					kind = "var"
				}
				out = append(out, node(kind, n, val))
				vars[n] = true
			}
		case *luaast.AssignStmt:
			for i, lh := range s.Lhs {
				nameExpr := exprToNode(lh, mut)
				var val *ast.Node
				if i < len(s.Rhs) {
					val = exprToNode(s.Rhs[i], mut)
				}
				if id, ok := lh.(*luaast.IdentExpr); ok && !vars[id.Value] {
					kind := "let"
					if mut[id.Value] {
						kind = "var"
					}
					out = append(out, node(kind, id.Value, val))
					vars[id.Value] = true
				} else {
					assignNode := node("assign", nil, nameExpr, val)
					out = append(out, assignNode)
				}
			}
		case *luaast.ReturnStmt:
			if len(s.Exprs) == 0 {
				out = append(out, node("return", nil))
			} else if len(s.Exprs) == 1 {
				out = append(out, node("return", nil, exprToNode(s.Exprs[0], mut)))
			} else {
				var nodes []*ast.Node
				for _, e := range s.Exprs {
					nodes = append(nodes, exprToNode(e, mut))
				}
				out = append(out, node("return", nil, node("tuple", nil, nodes...)))
			}
		case *luaast.FuncCallStmt:
			if fc, ok := s.Expr.(*luaast.FuncCallExpr); ok {
				if luaExprString(fc.Func, mut) == "print" && len(fc.Args) == 1 {
					if inner, ok := fc.Args[0].(*luaast.FuncCallExpr); ok {
						if luaExprString(inner.Func, mut) == "string.format" && len(inner.Args) == 2 {
							if str, ok := inner.Args[0].(*luaast.StringExpr); ok {
								parts := strings.SplitN(str.Value, "%s", 2)
								if len(parts) == 2 && parts[1] == "" {
									prefix := strings.TrimSuffix(parts[0], " ")
									out = append(out, node("call", "print", node("string", prefix), exprToNode(inner.Args[1], mut)))
									break
								}
							}
						}
					}
				}
			}
			out = append(out, exprToNode(s.Expr, mut))
		case *luaast.BreakStmt:
			out = append(out, node("break", nil))
		case *luaast.GotoStmt:
			out = append(out, node("continue", nil))
		case *luaast.WhileStmt:
			cond := exprToNode(s.Condition, mut)
			body := node("block", nil, convertStmtsToNodes(s.Stmts, copyVars(vars), mut)...)
			out = append(out, node("while", nil, cond, body))
		case *luaast.IfStmt:
			out = append(out, convertIfStmt(s, vars, mut))
		case *luaast.NumberForStmt:
			step := 1
			if s.Step != nil {
				if v, err := strconv.Atoi(luaNumberString(s.Step)); err == nil {
					step = v
				}
			}
			init := exprToNode(s.Init, mut)
			limit := exprToNode(s.Limit, mut)
			endVal := node("binary", "+", limit, node("int", step))
			rng := node("range", nil, init, endVal)
			body := node("block", nil, convertStmtsToNodes(s.Stmts, copyVars(vars), mut)...)
			out = append(out, node("for", s.Name, rng, body))
		case *luaast.GenericForStmt:
			iter := node("null", nil)
			if len(s.Exprs) > 0 {
				iter = exprToNode(s.Exprs[0], mut)
			}
			name := s.Names[len(s.Names)-1]
			if len(s.Names) > 1 {
				if call, ok := s.Exprs[0].(*luaast.FuncCallExpr); ok {
					callee := luaExprString(call.Func, mut)
					if callee == "pairs" && len(call.Args) == 1 {
						iter = node("call", "values", exprToNode(call.Args[0], mut))
					} else if callee == "ipairs" && len(call.Args) == 1 {
						iter = exprToNode(call.Args[0], mut)
					} else {
						iter = node("call", "values", iter)
					}
				} else {
					iter = node("call", "values", iter)
				}
			}
			body := node("block", nil, convertStmtsToNodes(s.Stmts, copyVars(vars), mut)...)
			out = append(out, node("for", name, node("in", nil, iter), body))
		case *luaast.FuncDefStmt:
			name := luaExprString(s.Name.Func, mut)
			fnNode := node("fun", name)
			fnNode.Children = append(fnNode.Children, paramNodes(s.Func)...)
			if rt := inferReturnType(s.Func); rt != "" {
				fnNode.Children = append(fnNode.Children, node("type", rt))
			}
			fnNode.Children = append(fnNode.Children, convertStmtsToNodes(s.Func.Stmts, map[string]bool{}, mut)...)
			out = append(out, fnNode)
		}
	}
	return out
}

func convertIfStmt(s *luaast.IfStmt, vars map[string]bool, mut map[string]bool) *ast.Node {
	n := node("if", nil, exprToNode(s.Condition, mut))
	thenBlock := node("block", nil, convertStmtsToNodes(s.Then, copyVars(vars), mut)...)
	n.Children = append(n.Children, thenBlock)
	if len(s.Else) == 1 {
		if next, ok := s.Else[0].(*luaast.IfStmt); ok {
			n.Children = append(n.Children, convertIfStmt(next, copyVars(vars), mut))
			return n
		}
	}
	if len(s.Else) > 0 {
		elseBlock := node("block", nil, convertStmtsToNodes(s.Else, copyVars(vars), mut)...)
		n.Children = append(n.Children, elseBlock)
	}
	return n
}

func exprToNode(e luaast.Expr, mut map[string]bool) *ast.Node {
	switch v := e.(type) {
	case *luaast.NumberExpr:
		if i, err := strconv.Atoi(v.Value); err == nil {
			return node("int", i)
		}
		return node("int", 0)
	case *luaast.StringExpr:
		return node("string", v.Value)
	case *luaast.TrueExpr:
		return node("bool", true)
	case *luaast.FalseExpr:
		return node("bool", false)
	case *luaast.NilExpr:
		return node("null", nil)
	case *luaast.IdentExpr:
		if v.Value == "__print" {
			return node("selector", "print")
		}
		return node("selector", v.Value)
	case *luaast.ArithmeticOpExpr:
		left := exprToNode(v.Lhs, mut)
		right := exprToNode(v.Rhs, mut)
		return node("binary", v.Operator, left, right)
	case *luaast.StringConcatOpExpr:
		return node("binary", "+", exprToNode(v.Lhs, mut), exprToNode(v.Rhs, mut))
	case *luaast.LogicalOpExpr:
		if v.Operator == "or" {
			if num, ok := v.Rhs.(*luaast.NumberExpr); ok && num.Value == "0" {
				if lhs, ok := v.Lhs.(*luaast.LogicalOpExpr); ok && lhs.Operator == "and" {
					if num1, ok := lhs.Rhs.(*luaast.NumberExpr); ok && num1.Value == "1" {
						return exprToNode(lhs.Lhs, mut)
					}
				}
			}
			// Convert Lua ternary pattern: a and b or c
			if lhs, ok := v.Lhs.(*luaast.LogicalOpExpr); ok && lhs.Operator == "and" {
				return node("if_expr", nil,
					exprToNode(lhs.Lhs, mut),
					exprToNode(lhs.Rhs, mut),
					exprToNode(v.Rhs, mut))
			}
		}
		op := v.Operator
		if op == "and" {
			op = "&&"
		} else if op == "or" {
			op = "||"
		}
		return node("binary", op, exprToNode(v.Lhs, mut), exprToNode(v.Rhs, mut))
	case *luaast.RelationalOpExpr:
		if v.Operator == "~=" {
			if _, ok := v.Rhs.(*luaast.NilExpr); ok {
				if call, ok := v.Lhs.(*luaast.FuncCallExpr); ok {
					if isAttrCall(call.Func, "string", "find") && len(call.Args) >= 4 {
						if num, ok := call.Args[2].(*luaast.NumberExpr); ok && num.Value == "1" {
							if _, ok := call.Args[3].(*luaast.TrueExpr); ok {
								recv := exprToNode(call.Args[0], mut)
								callee := node("selector", "contains", recv)
								return node("call", nil, callee, exprToNode(call.Args[1], mut))
							}
						}
					}
				}
			}
		}
		return node("binary", v.Operator, exprToNode(v.Lhs, mut), exprToNode(v.Rhs, mut))
	case *luaast.UnaryMinusOpExpr:
		return node("binary", "-", node("int", 0), exprToNode(v.Expr, mut))
	case *luaast.UnaryNotOpExpr:
		return node("unary", "!", exprToNode(v.Expr, mut))
	case *luaast.UnaryLenOpExpr:
		return node("call", "len", exprToNode(v.Expr, mut))
	case *luaast.FuncCallExpr:
		if val, ok := tryValuesCallNode(v, mut); ok {
			return val
		}
		var args []*ast.Node
		for _, a := range v.Args {
			args = append(args, exprToNode(a, mut))
		}
		callee := luaExprString(v.Func, mut)
		if v.Method != "" {
			callee = luaExprString(v.Receiver, mut) + "." + v.Method
		}
		if fn, ok := v.Func.(*luaast.FunctionExpr); ok {
			if isAppendFunc(fn) && len(v.Args) == 2 {
				return node("call", "append", exprToNode(v.Args[0], mut), exprToNode(v.Args[1], mut))
			}
			if isAvgFunc(fn) && len(v.Args) == 1 {
				return node("call", "avg", exprToNode(v.Args[0], mut))
			}
			if isSumFunc(fn) && len(v.Args) == 1 {
				return node("call", "sum", exprToNode(v.Args[0], mut))
			}
			if isLenFunc(fn) && len(v.Args) == 1 {
				return node("call", "len", exprToNode(v.Args[0], mut))
			}
		}
		if (callee == "pairs" || callee == "ipairs") && len(args) == 1 {
			return exprToNode(v.Args[0], mut)
		}
		switch callee {
		case "__print":
			callee = "print"
		case "__add":
			if len(args) == 2 {
				return node("binary", "+", args[0], args[1])
			}
		case "__div":
			if len(args) == 2 {
				return node("binary", "/", args[0], args[1])
			}
		case "__eq":
			if len(args) == 2 {
				return node("binary", "==", args[0], args[1])
			}
		case "__contains":
			if len(args) == 2 {
				return node("binary", "in", args[0], args[1])
			}
		case "__count":
			if len(args) == 1 {
				callee = "count"
			}
		case "__avg":
			if len(args) == 1 {
				callee = "avg"
			}
		case "__sum":
			if len(args) == 1 {
				callee = "sum"
			}
		case "tostring":
			if len(args) == 1 {
				callee = "str"
			}
		case "__append":
			if len(args) == 2 {
				callee = "append"
			}
		case "__input":
			callee = "input"
		case "__index":
			if len(args) == 2 {
				return node("index", nil, args[0], args[1])
			}
		case "__slice":
			if len(args) == 3 {
				return node("index", nil, args[0], node("start", nil, args[1]), node("end", nil, args[2]))
			}
		case "string.sub":
			if len(v.Args) == 3 {
				if addStart, ok := v.Args[1].(*luaast.ArithmeticOpExpr); ok && addStart.Operator == "+" {
					if num, ok := addStart.Rhs.(*luaast.NumberExpr); ok && num.Value == "1" {
						return node("call", "substring", exprToNode(v.Args[0], mut), exprToNode(addStart.Lhs, mut), exprToNode(v.Args[2], mut))
					}
				}
				return node("call", "substring", exprToNode(v.Args[0], mut), exprToNode(v.Args[1], mut), exprToNode(v.Args[2], mut))
			}
		case "string.format":
			callee = "fmt.Sprintf"
		case "__union_all":
			if len(args) == 2 {
				return node("binary", "union_all", args[0], args[1])
			}
		case "__union":
			if len(args) == 2 {
				return node("binary", "union", args[0], args[1])
			}
		case "__except":
			if len(args) == 2 {
				return node("binary", "except", args[0], args[1])
			}
		case "__intersect":
			if len(args) == 2 {
				return node("binary", "intersect", args[0], args[1])
			}
		}
		n := node("call", callee)
		n.Children = append(n.Children, args...)
		return n
	case *luaast.FunctionExpr:
		n := node("funexpr", nil)
		n.Children = append(n.Children, paramNodes(v)...)
		if rt := inferReturnType(v); rt != "" {
			n.Children = append(n.Children, node("type", rt))
		}
		if len(v.Stmts) == 1 {
			if r, ok := v.Stmts[0].(*luaast.ReturnStmt); ok && len(r.Exprs) == 1 {
				n.Children = append(n.Children, exprToNode(r.Exprs[0], mut))
				return n
			}
		}
		blk := node("block", nil, convertStmtsToNodes(v.Stmts, map[string]bool{}, mut)...)
		n.Children = append(n.Children, blk)
		return n
	case *luaast.AttrGetExpr:
		if k, ok := v.Key.(*luaast.StringExpr); ok {
			if id, ok := v.Object.(*luaast.IdentExpr); ok {
				return node("selector", k.Value, node("selector", id.Value))
			}
			return node("selector", k.Value, exprToNode(v.Object, mut))
		}
		if _, ok := v.Key.(*luaast.IdentExpr); ok {
			return node("selector", luaExprString(v.Key, mut), exprToNode(v.Object, mut))
		}
		if add, ok := v.Key.(*luaast.ArithmeticOpExpr); ok && add.Operator == "+" {
			if num, ok := add.Rhs.(*luaast.NumberExpr); ok && num.Value == "1" {
				return node("index", nil, exprToNode(v.Object, mut), exprToNode(add.Lhs, mut))
			}
		}
		return node("index", nil, exprToNode(v.Object, mut), exprToNode(v.Key, mut))
	case *luaast.TableExpr:
		isMap := false
		var items []*ast.Node
		for _, f := range v.Fields {
			if f.Key != nil {
				isMap = true
				items = append(items, node("entry", nil, exprToNode(f.Key, mut), exprToNode(f.Value, mut)))
			} else {
				items = append(items, exprToNode(f.Value, mut))
			}
		}
		if isMap {
			return node("map", nil, items...)
		}
		return node("list", nil, items...)
	}
	return node("unknown", nil)
}

func tryValuesCallNode(call *luaast.FuncCallExpr, mut map[string]bool) (*ast.Node, bool) {
	if !isAttrCall(call.Func, "table", "concat") || len(call.Args) != 2 {
		return nil, false
	}
	if s, ok := call.Args[1].(*luaast.StringExpr); !ok || s.Value != " " {
		return nil, false
	}
	inner, ok := call.Args[0].(*luaast.FuncCallExpr)
	if !ok || len(inner.Args) != 1 {
		return nil, false
	}
	if _, ok := inner.Func.(*luaast.FunctionExpr); !ok {
		return nil, false
	}
	return node("call", "values", exprToNode(inner.Args[0], mut)), true
}

func inferReturnType(fn *luaast.FunctionExpr) string {
	for _, st := range fn.Stmts {
		if r, ok := st.(*luaast.ReturnStmt); ok && len(r.Exprs) == 1 {
			switch r.Exprs[0].(type) {
			case *luaast.TrueExpr, *luaast.FalseExpr, *luaast.LogicalOpExpr, *luaast.RelationalOpExpr, *luaast.UnaryNotOpExpr:
				return "bool"
			case *luaast.NumberExpr, *luaast.ArithmeticOpExpr:
				return "int"
			}
		}
	}
	return ""
}

func paramNodes(fn *luaast.FunctionExpr) []*ast.Node {
	var params []string
	if fn.ParList != nil {
		params = fn.ParList.Names
	}
	types := make(map[string]string)
	paramSet := make(map[string]bool)
	for _, n := range params {
		paramSet[n] = true
	}
	collectParamTypes(fn.Stmts, paramSet, types)
	var out []*ast.Node
	for _, n := range params {
		p := node("param", n)
		if t, ok := types[n]; ok {
			p.Children = append(p.Children, node("type", t))
		}
		out = append(out, p)
	}
	return out
}

func luaNumberString(e luaast.Expr) string {
	if n, ok := e.(*luaast.NumberExpr); ok {
		return n.Value
	}
	return "0"
}

func convertLuaStmts(stmts []luaast.Stmt, indent int, vars map[string]bool, mut map[string]bool) []string {
	if vars == nil {
		vars = make(map[string]bool)
	}
	ind := strings.Repeat("  ", indent)
	var out []string
	for _, st := range stmts {
		switch s := st.(type) {
		case *luaast.LocalAssignStmt:
			for i, n := range s.Names {
				if i < len(s.Exprs) {
					if fn, ok := s.Exprs[i].(*luaast.FunctionExpr); ok {
						out = append(out, ind+"fun "+n+luaFuncSignature(fn)+" {")
						out = append(out, convertLuaStmts(fn.Stmts, indent+1, map[string]bool{}, mut)...)
						out = append(out, ind+"}")
						vars[n] = true
						continue
					}
				}
				val := ""
				if i < len(s.Exprs) {
					val = luaExprString(s.Exprs[i], mut)
				}
				keyword := "let "
				if mut[n] {
					keyword = "var "
				}
				out = append(out, ind+keyword+n+" = "+val)
				vars[n] = true
			}
		case *luaast.AssignStmt:
			for i, lh := range s.Lhs {
				name := luaExprString(lh, mut)
				val := ""
				if i < len(s.Rhs) {
					val = luaExprString(s.Rhs[i], mut)
				}
				if vars[name] {
					out = append(out, ind+name+" = "+val)
				} else if strings.ContainsAny(name, "[].") {
					out = append(out, ind+name+" = "+val)
				} else {
					keyword := "let "
					if mut[name] {
						keyword = "var "
					}
					out = append(out, ind+keyword+name+" = "+val)
					vars[name] = true
				}
			}
		case *luaast.ReturnStmt:
			if len(s.Exprs) == 0 {
				out = append(out, ind+"return")
			} else if len(s.Exprs) == 1 {
				out = append(out, ind+"return "+luaExprString(s.Exprs[0], mut))
			} else {
				var parts []string
				for _, e := range s.Exprs {
					parts = append(parts, luaExprString(e, mut))
				}
				out = append(out, ind+"return ("+strings.Join(parts, ", ")+")")
			}
		case *luaast.FuncCallStmt:
			if fc, ok := s.Expr.(*luaast.FuncCallExpr); ok {
				if luaExprString(fc.Func, mut) == "print" && len(fc.Args) == 1 {
					if inner, ok := fc.Args[0].(*luaast.FuncCallExpr); ok {
						if luaExprString(inner.Func, mut) == "string.format" && len(inner.Args) == 2 {
							if str, ok := inner.Args[0].(*luaast.StringExpr); ok {
								parts := strings.SplitN(str.Value, "%s", 2)
								if len(parts) == 2 && parts[1] == "" {
									prefix := strings.TrimSuffix(parts[0], " ")
									out = append(out, ind+"print("+strconv.Quote(prefix)+", "+luaExprString(inner.Args[1], mut)+")")
									continue
								}
							}
						}
					}
				}
			}
			out = append(out, ind+luaExprString(s.Expr, mut))
		case *luaast.BreakStmt:
			out = append(out, ind+"break")
		case *luaast.GotoStmt:
			// Transpiler emits goto statements for simulated
			// continue calls. Replace them with continue.
			out = append(out, ind+"continue")
		case *luaast.LabelStmt:
			// Ignore labels used for continue targets.
			continue
		case *luaast.DoBlockStmt:
			out = append(out, ind+"do {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, copyVars(vars), mut)...)
			out = append(out, ind+"}")
		case *luaast.WhileStmt:
			cond := luaExprString(s.Condition, mut)
			out = append(out, ind+"while "+cond+" {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, copyVars(vars), mut)...)
			out = append(out, ind+"}")
		case *luaast.RepeatStmt:
			out = append(out, ind+"do {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, copyVars(vars), mut)...)
			cond := luaExprString(s.Condition, mut)
			out = append(out, ind+"} while !("+cond+")")
		case *luaast.IfStmt:
			writeLuaIfStmt(&out, s, indent, vars, mut)
		case *luaast.NumberForStmt:
			step := "1"
			if s.Step != nil {
				step = luaExprString(s.Step, mut)
			}
			init := luaExprString(s.Init, mut)
			limit := luaExprString(s.Limit, mut)
			endVal := fmt.Sprintf("(%s) + %s", limit, step)
			loop := fmt.Sprintf("for %s in %s..%s", s.Name, init, endVal)
			if step != "1" {
				loop += " step " + step
			}
			loop += " {"
			out = append(out, ind+loop)
			out = append(out, convertLuaStmts(s.Stmts, indent+1, copyVars(vars), mut)...)
			out = append(out, ind+"}")
		case *luaast.GenericForStmt:
			iter := ""
			if len(s.Exprs) > 0 {
				iter = luaExprString(s.Exprs[0], mut)
			}
			name := strings.Join(s.Names, ", ")
			if len(s.Names) > 1 {
				if call, ok := s.Exprs[0].(*luaast.FuncCallExpr); ok {
					callee := luaExprString(call.Func, mut)
					if callee == "pairs" && len(call.Args) == 1 {
						iter = "values(" + luaExprString(call.Args[0], mut) + ")"
					} else if callee == "ipairs" && len(call.Args) == 1 {
						iter = luaExprString(call.Args[0], mut)
					} else {
						iter = "values(" + iter + ")"
					}
				} else {
					iter = "values(" + iter + ")"
				}
				name = s.Names[len(s.Names)-1]
			}
			out = append(out, ind+"for "+name+" in "+iter+" {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, copyVars(vars), mut)...)
			out = append(out, ind+"}")
		case *luaast.FuncDefStmt:
			origName := ""
			if id, ok := s.Name.Func.(*luaast.IdentExpr); ok {
				origName = id.Value
			}
			name := luaExprString(s.Name.Func, mut)
			if s.Name.Method != "" {
				recv := luaExprString(s.Name.Func, mut)
				name = recv + "." + s.Name.Method
			}
			if strings.HasPrefix(origName, "__") {
				continue
			}
			out = append(out, ind+"fun "+name+luaFuncSignature(s.Func)+" {")
			out = append(out, convertLuaStmts(s.Func.Stmts, indent+1, map[string]bool{}, mut)...)
			out = append(out, ind+"}")
		}
	}
	return out
}

func writeLuaIfStmt(out *[]string, s *luaast.IfStmt, indent int, vars map[string]bool, mut map[string]bool) {
	ind := strings.Repeat("  ", indent)
	*out = append(*out, ind+"if "+luaExprString(s.Condition, mut)+" {")
	*out = append(*out, convertLuaStmts(s.Then, indent+1, copyVars(vars), mut)...)
	cur := s
	for {
		if len(cur.Else) == 1 {
			if next, ok := cur.Else[0].(*luaast.IfStmt); ok {
				*out = append(*out, ind+"} else if "+luaExprString(next.Condition, mut)+" {")
				*out = append(*out, convertLuaStmts(next.Then, indent+1, copyVars(vars), mut)...)
				cur = next
				continue
			}
		}
		if len(cur.Else) > 0 {
			*out = append(*out, ind+"} else {")
			*out = append(*out, convertLuaStmts(cur.Else, indent+1, copyVars(vars), mut)...)
			*out = append(*out, ind+"}")
		} else {
			*out = append(*out, ind+"}")
		}
		break
	}
}

func luaExprString(e luaast.Expr, mut map[string]bool) string {
	switch v := e.(type) {
	case *luaast.NumberExpr:
		return v.Value
	case *luaast.StringExpr:
		return strconv.Quote(v.Value)
	case *luaast.TrueExpr:
		return "true"
	case *luaast.FalseExpr:
		return "false"
	case *luaast.NilExpr:
		return "null"
	case *luaast.IdentExpr:
		if v.Value == "__print" {
			return "print"
		}
		return v.Value
	case *luaast.ArithmeticOpExpr:
		// Always wrap arithmetic expressions in parentheses to
		// preserve operator precedence when converting back to Mochi.
		return "(" + luaExprString(v.Lhs, mut) + " " + v.Operator + " " + luaExprString(v.Rhs, mut) + ")"
	case *luaast.StringConcatOpExpr:
		return luaExprString(v.Lhs, mut) + " + " + luaExprString(v.Rhs, mut)
	case *luaast.LogicalOpExpr:
		// Lua's transpiler represents boolean expressions like
		// `cond and 1 or 0`. Strip the numeric parts so we just emit the
		// condition itself.
		if v.Operator == "or" {
			if num, ok := v.Rhs.(*luaast.NumberExpr); ok && num.Value == "0" {
				if lhs, ok := v.Lhs.(*luaast.LogicalOpExpr); ok && lhs.Operator == "and" {
					if num1, ok := lhs.Rhs.(*luaast.NumberExpr); ok && num1.Value == "1" {
						return luaExprString(lhs.Lhs, mut)
					}
				}
			}
		}
		op := v.Operator
		if op == "and" {
			op = "&&"
		} else if op == "or" {
			op = "||"
		}
		return luaExprString(v.Lhs, mut) + " " + op + " " + luaExprString(v.Rhs, mut)
	case *luaast.RelationalOpExpr:
		if v.Operator == "~=" {
			if _, ok := v.Rhs.(*luaast.NilExpr); ok {
				if call, ok := v.Lhs.(*luaast.FuncCallExpr); ok {
					if isAttrCall(call.Func, "string", "find") && len(call.Args) >= 4 {
						if num, ok := call.Args[2].(*luaast.NumberExpr); ok && num.Value == "1" {
							if _, ok := call.Args[3].(*luaast.TrueExpr); ok {
								recv := luaExprString(call.Args[0], mut)
								arg := luaExprString(call.Args[1], mut)
								return recv + ".contains(" + arg + ")"
							}
						}
					}
				}
			}
		}
		return luaExprString(v.Lhs, mut) + " " + v.Operator + " " + luaExprString(v.Rhs, mut)
	case *luaast.UnaryMinusOpExpr:
		return "-" + luaExprString(v.Expr, mut)
	case *luaast.UnaryNotOpExpr:
		return "!" + luaExprString(v.Expr, mut)
	case *luaast.UnaryLenOpExpr:
		return "len(" + luaExprString(v.Expr, mut) + ")"
	case *luaast.FuncCallExpr:
		if val, ok := tryValuesCall(v, mut); ok {
			return val
		}
		var args []string
		for _, a := range v.Args {
			args = append(args, luaExprString(a, mut))
		}
		callee := luaExprString(v.Func, mut)
		if v.Method != "" {
			callee = luaExprString(v.Receiver, mut) + "." + v.Method
		}
		if fn, ok := v.Func.(*luaast.FunctionExpr); ok {
			if isAppendFunc(fn) && len(v.Args) == 2 {
				return "append(" + luaExprString(v.Args[0], mut) + ", " + luaExprString(v.Args[1], mut) + ")"
			}
			if isAvgFunc(fn) && len(v.Args) == 1 {
				return "avg(" + luaExprString(v.Args[0], mut) + ")"
			}
		}
		if (callee == "pairs" || callee == "ipairs") && len(args) == 1 {
			return luaExprString(v.Args[0], mut)
		}
		switch callee {
		case "__print":
			callee = "print"
		case "__add":
			if len(args) == 2 {
				return "(" + args[0] + " + " + args[1] + ")"
			}
		case "__div":
			if len(args) == 2 {
				return "(" + args[0] + " / " + args[1] + ")"
			}
		case "__eq":
			if len(args) == 2 {
				return "(" + args[0] + " == " + args[1] + ")"
			}
		case "__contains":
			if len(args) == 2 {
				return "(" + args[1] + " in " + args[0] + ")"
			}
		case "__count":
			if len(args) == 1 {
				callee = "count"
			}
		case "__avg":
			if len(args) == 1 {
				callee = "avg"
			}
		case "__sum":
			if len(args) == 1 {
				callee = "sum"
			}
		case "tostring":
			if len(args) == 1 {
				callee = "str"
			}
		case "__append":
			if len(args) == 2 {
				callee = "append"
			}
		case "__input":
			callee = "input"
		case "__index":
			if len(args) == 2 {
				return args[0] + "[" + args[1] + "]"
			}
		case "__slice":
			if len(args) == 3 {
				return args[0] + "[" + args[1] + ":" + args[2] + "]"
			}
		case "string.format":
			if len(args) == 2 {
				if s, ok := v.Args[0].(*luaast.StringExpr); ok {
					parts := strings.SplitN(s.Value, "%s", 2)
					before := strconv.Quote(parts[0])
					middle := luaExprString(v.Args[1], mut)
					if len(parts) == 2 {
						after := strconv.Quote(parts[1])
						if parts[1] != "" {
							return before + " + " + middle + " + " + after
						}
					}
					return before + " + " + middle
				}
			}
			callee = "fmt.Sprintf"
		case "__union_all":
			if len(args) == 2 {
				return "(" + args[0] + " union all " + args[1] + ")"
			}
		case "__union":
			if len(args) == 2 {
				return "(" + args[0] + " union " + args[1] + ")"
			}
		case "__except":
			if len(args) == 2 {
				return "(" + args[0] + " except " + args[1] + ")"
			}
		case "__intersect":
			if len(args) == 2 {
				return "(" + args[0] + " intersect " + args[1] + ")"
			}
		}
		return callee + "(" + strings.Join(args, ", ") + ")"
	case *luaast.FunctionExpr:
		var b strings.Builder
		b.WriteString("fun")
		b.WriteString(luaFuncSignature(v))
		if len(v.Stmts) == 1 {
			if r, ok := v.Stmts[0].(*luaast.ReturnStmt); ok && len(r.Exprs) == 1 {
				b.WriteString(" => ")
				b.WriteString(luaExprString(r.Exprs[0], mut))
				return b.String()
			}
		}
		b.WriteString(" {")
		for _, line := range convertLuaStmts(v.Stmts, 1, map[string]bool{}, mut) {
			b.WriteByte('\n')
			b.WriteString("  ")
			b.WriteString(line)
		}
		b.WriteByte('\n')
		b.WriteString("}")
		return b.String()
	case *luaast.AttrGetExpr:
		if k, ok := v.Key.(*luaast.StringExpr); ok {
			if id, ok := v.Object.(*luaast.IdentExpr); ok {
				return id.Value + "." + k.Value
			}
			return luaExprString(v.Object, mut) + "[" + strconv.Quote(k.Value) + "]"
		}
		if _, ok := v.Key.(*luaast.IdentExpr); ok {
			return luaExprString(v.Object, mut) + "." + luaExprString(v.Key, mut)
		}
		return luaExprString(v.Object, mut) + "[" + luaExprString(v.Key, mut) + "]"
	case *luaast.TableExpr:
		isMap := false
		var items []string
		for _, f := range v.Fields {
			if f.Key != nil {
				isMap = true
				items = append(items, luaExprString(f.Key, mut)+": "+luaExprString(f.Value, mut))
			} else {
				items = append(items, luaExprString(f.Value, mut))
			}
		}
		if isMap {
			return "{ " + strings.Join(items, ", ") + " }"
		}
		return "[" + strings.Join(items, ", ") + "]"
	}
	return ""
}

func luaFuncSignature(fn *luaast.FunctionExpr) string {
	var params []string
	paramSet := make(map[string]bool)
	if fn.ParList != nil {
		for _, n := range fn.ParList.Names {
			params = append(params, n)
			paramSet[n] = true
		}
	}
	types := make(map[string]string)
	collectParamTypes(fn.Stmts, paramSet, types)
	ret := ""
	for _, st := range fn.Stmts {
		if r, ok := st.(*luaast.ReturnStmt); ok && len(r.Exprs) == 1 {
			switch r.Exprs[0].(type) {
			case *luaast.TrueExpr, *luaast.FalseExpr, *luaast.LogicalOpExpr, *luaast.RelationalOpExpr, *luaast.UnaryNotOpExpr:
				ret = ": bool"
			case *luaast.NumberExpr, *luaast.ArithmeticOpExpr:
				ret = ": int"
			case *luaast.FunctionExpr:
				ret = ": fun" + luaFuncSignature(r.Exprs[0].(*luaast.FunctionExpr))
			}
			break
		}
	}
	for i, n := range params {
		if t, ok := types[n]; ok {
			params[i] = n + ": " + t
		}
	}
	return "(" + strings.Join(params, ", ") + ")" + ret
}

func collectParamTypes(stmts []luaast.Stmt, params map[string]bool, types map[string]string) {
	var visitExpr func(e luaast.Expr)
	visitExpr = func(e luaast.Expr) {
		switch v := e.(type) {
		case *luaast.IdentExpr:
			if params[v.Value] {
				if _, ok := types[v.Value]; !ok {
					types[v.Value] = "int"
				}
			}
		case *luaast.ArithmeticOpExpr:
			visitExpr(v.Lhs)
			visitExpr(v.Rhs)
		case *luaast.FuncCallExpr:
			for _, a := range v.Args {
				visitExpr(a)
			}
		case *luaast.FunctionExpr:
			collectParamTypes(v.Stmts, params, types)
		case *luaast.LogicalOpExpr:
			visitExpr(v.Lhs)
			visitExpr(v.Rhs)
		case *luaast.RelationalOpExpr:
			visitExpr(v.Lhs)
			visitExpr(v.Rhs)
		}
	}
	for _, st := range stmts {
		switch s := st.(type) {
		case *luaast.AssignStmt:
			for _, e := range s.Rhs {
				visitExpr(e)
			}
		case *luaast.LocalAssignStmt:
			for _, e := range s.Exprs {
				visitExpr(e)
			}
		case *luaast.FuncCallStmt:
			visitExpr(s.Expr)
		case *luaast.ReturnStmt:
			for _, e := range s.Exprs {
				visitExpr(e)
			}
		case *luaast.NumberForStmt, *luaast.GenericForStmt:
			// ignore
		}
	}
}

func isAppendFunc(fn *luaast.FunctionExpr) bool {
	if len(fn.Stmts) != 3 {
		return false
	}
	assign, ok := fn.Stmts[0].(*luaast.LocalAssignStmt)
	if !ok || len(assign.Names) != 1 || assign.Names[0] != "res" || len(assign.Exprs) != 1 {
		return false
	}
	tbl, ok := assign.Exprs[0].(*luaast.TableExpr)
	if !ok || len(tbl.Fields) != 1 {
		return false
	}
	call, ok := tbl.Fields[0].Value.(*luaast.FuncCallExpr)
	if !ok || !isAttrCall(call.Func, "table", "unpack") {
		return false
	}
	fc, ok := fn.Stmts[1].(*luaast.FuncCallStmt)
	if !ok {
		return false
	}
	if !isAttrCall(fc.Expr.(*luaast.FuncCallExpr).Func, "table", "insert") {
		return false
	}
	ret, ok := fn.Stmts[2].(*luaast.ReturnStmt)
	if !ok || len(ret.Exprs) != 1 {
		return false
	}
	if id, ok := ret.Exprs[0].(*luaast.IdentExpr); !ok || id.Value != "res" {
		return false
	}
	return true
}

func isAvgFunc(fn *luaast.FunctionExpr) bool {
	// Simple heuristic for the generated avg_builtin function
	if len(fn.Stmts) < 4 {
		return false
	}
	// first stmt: local sum = 0
	if assign, ok := fn.Stmts[0].(*luaast.LocalAssignStmt); !ok || len(assign.Names) != 1 || assign.Names[0] != "sum" {
		return false
	}
	// presence of for loop over ipairs and final return statements
	hasLoop := false
	hasReturn := false
	for _, st := range fn.Stmts {
		switch st.(type) {
		case *luaast.NumberForStmt, *luaast.GenericForStmt:
			hasLoop = true
		case *luaast.ReturnStmt:
			hasReturn = true
		}
	}
	return hasLoop && hasReturn
}

func isSumFunc(fn *luaast.FunctionExpr) bool {
	if len(fn.Stmts) < 3 {
		return false
	}
	if assign, ok := fn.Stmts[0].(*luaast.LocalAssignStmt); !ok || len(assign.Names) != 1 || assign.Names[0] != "s" {
		return false
	}
	hasLoop := false
	hasReturn := false
	for _, st := range fn.Stmts {
		switch st.(type) {
		case *luaast.NumberForStmt, *luaast.GenericForStmt:
			hasLoop = true
		case *luaast.ReturnStmt:
			hasReturn = true
		}
	}
	return hasLoop && hasReturn
}

func isLenFunc(fn *luaast.FunctionExpr) bool {
	if fn.ParList == nil || len(fn.ParList.Names) != 1 {
		return false
	}
	found := false
	var visitExpr func(luaast.Expr)
	visitExpr = func(e luaast.Expr) {
		switch v := e.(type) {
		case *luaast.UnaryLenOpExpr:
			found = true
		case *luaast.ArithmeticOpExpr:
			visitExpr(v.Lhs)
			visitExpr(v.Rhs)
		case *luaast.AttrGetExpr:
			visitExpr(v.Object)
			visitExpr(v.Key)
		case *luaast.FuncCallExpr:
			visitExpr(v.Func)
			for _, a := range v.Args {
				visitExpr(a)
			}
		}
	}
	var visitStmt func(luaast.Stmt)
	visitStmt = func(s luaast.Stmt) {
		switch st := s.(type) {
		case *luaast.ReturnStmt:
			for _, e := range st.Exprs {
				visitExpr(e)
			}
		case *luaast.AssignStmt:
			for _, e := range st.Rhs {
				visitExpr(e)
			}
		case *luaast.LocalAssignStmt:
			for _, e := range st.Exprs {
				visitExpr(e)
			}
		case *luaast.IfStmt:
			visitExpr(st.Condition)
			for _, t := range st.Then {
				visitStmt(t)
			}
			for _, e := range st.Else {
				visitStmt(e)
			}
		case *luaast.NumberForStmt, *luaast.GenericForStmt, *luaast.WhileStmt, *luaast.RepeatStmt, *luaast.DoBlockStmt:
			// ignore for len builtin detection
		}
	}
	for _, st := range fn.Stmts {
		visitStmt(st)
	}
	return found
}

func tryValuesCall(call *luaast.FuncCallExpr, mut map[string]bool) (string, bool) {
	if !isAttrCall(call.Func, "table", "concat") || len(call.Args) != 2 {
		return "", false
	}
	if s, ok := call.Args[1].(*luaast.StringExpr); !ok || s.Value != " " {
		return "", false
	}
	inner, ok := call.Args[0].(*luaast.FuncCallExpr)
	if !ok || len(inner.Args) != 1 {
		return "", false
	}
	if _, ok := inner.Func.(*luaast.FunctionExpr); !ok {
		return "", false
	}
	return "values(" + luaExprString(inner.Args[0], mut) + ")", true
}

func isAttrCall(e luaast.Expr, object, method string) bool {
	get, ok := e.(*luaast.AttrGetExpr)
	if !ok {
		return false
	}
	obj, ok1 := get.Object.(*luaast.IdentExpr)
	if !ok1 || obj.Value != object {
		return false
	}
	switch k := get.Key.(type) {
	case *luaast.IdentExpr:
		return k.Value == method
	case *luaast.StringExpr:
		return k.Value == method
	}
	return false
}
func writeLuaChunk(out *strings.Builder, chunk []luaast.Stmt, mut map[string]bool) {
	for _, line := range convertLuaStmts(chunk, 0, map[string]bool{}, mut) {
		if strings.HasPrefix(line, "fun __") {
			continue
		}
		out.WriteString(line)
		out.WriteByte('\n')
	}
}

func copyVars(src map[string]bool) map[string]bool {
	dst := make(map[string]bool, len(src))
	for k, v := range src {
		dst[k] = v
	}
	return dst
}

func collectMutables(stmts []luaast.Stmt) map[string]bool {
	counts := make(map[string]int)
	collectAssignCounts(stmts, counts)
	out := make(map[string]bool)
	for k, c := range counts {
		if c > 1 {
			out[k] = true
		}
	}
	return out
}

func collectAssignCounts(stmts []luaast.Stmt, counts map[string]int) {
	for _, st := range stmts {
		switch s := st.(type) {
		case *luaast.LocalAssignStmt:
			for _, n := range s.Names {
				counts[n]++
			}
		case *luaast.AssignStmt:
			for _, lh := range s.Lhs {
				collectAssignTargets(lh, counts)
			}
		case *luaast.FuncDefStmt:
			if id, ok := s.Name.Func.(*luaast.IdentExpr); ok {
				counts[id.Value]++
			}
			collectAssignCounts(s.Func.Stmts, counts)
		case *luaast.DoBlockStmt:
			collectAssignCounts(s.Stmts, counts)
		case *luaast.WhileStmt:
			collectAssignCounts(s.Stmts, counts)
		case *luaast.RepeatStmt:
			collectAssignCounts(s.Stmts, counts)
		case *luaast.IfStmt:
			collectAssignCounts(s.Then, counts)
			collectAssignCounts(s.Else, counts)
		case *luaast.NumberForStmt:
			counts[s.Name]++
			collectAssignCounts(s.Stmts, counts)
		case *luaast.GenericForStmt:
			for _, n := range s.Names {
				counts[n]++
			}
			collectAssignCounts(s.Stmts, counts)
		}
	}
}

func collectAssignTargets(e luaast.Expr, counts map[string]int) {
	switch v := e.(type) {
	case *luaast.IdentExpr:
		counts[v.Value]++
	case *luaast.AttrGetExpr:
		collectAssignTargets(v.Object, counts)
	}
}
