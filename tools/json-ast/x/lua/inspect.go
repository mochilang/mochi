package lua

import (
	"bytes"
	"fmt"
	"strings"

	luaast "github.com/yuin/gopher-lua/ast"
	luaparse "github.com/yuin/gopher-lua/parse"
)

// Inspect parses the given Lua source code and returns its AST.
func Inspect(src string) (*Program, error) {
	pre := preprocessLuaSource(src)
	chunk, err := luaparse.Parse(bytes.NewReader([]byte(pre)), "src.lua")
	if err != nil {
		return nil, fmt.Errorf("%s", formatLuaParseError(err, src))
	}
	return &Program{Stmts: convertStmts(chunk)}, nil
}

func preprocessLuaSource(src string) string {
	var out strings.Builder
	inStr := false
	strDelim := byte(0)
	inComment := false
	for i := 0; i < len(src); i++ {
		ch := src[i]
		if inComment {
			out.WriteByte(ch)
			if ch == '\n' {
				inComment = false
			}
			continue
		}
		if inStr {
			out.WriteByte(ch)
			if ch == strDelim && (i == 0 || src[i-1] != '\\') {
				inStr = false
			}
			continue
		}
		if ch == '-' && i+1 < len(src) && src[i+1] == '-' {
			out.WriteByte(ch)
			inComment = true
			continue
		}
		if ch == '\'' || ch == '"' {
			inStr = true
			strDelim = ch
			out.WriteByte(ch)
			continue
		}
		if ch == '/' && i+1 < len(src) && src[i+1] == '/' {
			out.WriteByte('/')
			out.WriteByte(' ')
			i++
			continue
		}
		out.WriteByte(ch)
	}
	res := out.String()
	lines := strings.Split(res, "\n")
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "goto __continue") || strings.HasPrefix(trimmed, "::__continue") {
			lines[i] = "--" + line
		}
	}
	return strings.Join(lines, "\n")
}

func formatLuaParseError(err error, src string) string {
	msg := err.Error()
	line := 0
	col := 0
	if idx := strings.Index(msg, "line:"); idx >= 0 {
		i := idx + len("line:")
		for i < len(msg) && msg[i] >= '0' && msg[i] <= '9' {
			line = line*10 + int(msg[i]-'0')
			i++
		}
		if strings.HasPrefix(msg[i:], "(column:") {
			i += len("(column:")
			for i < len(msg) && msg[i] >= '0' && msg[i] <= '9' {
				col = col*10 + int(msg[i]-'0')
				i++
			}
		}
	}
	lines := strings.Split(src, "\n")
	if line <= 0 || line-1 >= len(lines) {
		return msg
	}
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	b.WriteString(fmt.Sprintf("line %d: %s\n", line, msg))
	for i := start; i <= end; i++ {
		b.WriteString(fmt.Sprintf("%4d| %s\n", i+1, lines[i]))
		if i == line-1 && col > 0 {
			b.WriteString("     " + strings.Repeat(" ", col-1) + "^\n")
		}
	}
	return strings.TrimSpace(b.String())
}

func convertStmts(stmts []luaast.Stmt) []Stmt {
	out := make([]Stmt, len(stmts))
	for i, st := range stmts {
		out[i] = convertStmt(st)
	}
	return out
}

func convertStmt(s luaast.Stmt) Stmt {
	switch st := s.(type) {
	case *luaast.AssignStmt:
		return &AssignStmt{Lhs: convertExprs(st.Lhs), Rhs: convertExprs(st.Rhs)}
	case *luaast.LocalAssignStmt:
		return &LocalAssignStmt{Names: append([]string(nil), st.Names...), Exprs: convertExprs(st.Exprs)}
	case *luaast.FuncCallStmt:
		return &FuncCallStmt{Expr: convertExpr(st.Expr)}
	case *luaast.DoBlockStmt:
		return &DoBlockStmt{Stmts: convertStmts(st.Stmts)}
	case *luaast.WhileStmt:
		return &WhileStmt{Condition: convertExpr(st.Condition), Stmts: convertStmts(st.Stmts)}
	case *luaast.RepeatStmt:
		return &RepeatStmt{Condition: convertExpr(st.Condition), Stmts: convertStmts(st.Stmts)}
	case *luaast.IfStmt:
		return &IfStmt{Condition: convertExpr(st.Condition), Then: convertStmts(st.Then), Else: convertStmts(st.Else)}
	case *luaast.NumberForStmt:
		return &NumberForStmt{Name: st.Name, Init: convertExpr(st.Init), Limit: convertExpr(st.Limit), Step: convertExpr(st.Step), Stmts: convertStmts(st.Stmts)}
	case *luaast.GenericForStmt:
		return &GenericForStmt{Names: append([]string(nil), st.Names...), Exprs: convertExprs(st.Exprs), Stmts: convertStmts(st.Stmts)}
	case *luaast.FuncDefStmt:
		return &FuncDefStmt{Name: convertFuncName(st.Name), Func: convertFunctionExpr(st.Func)}
	case *luaast.ReturnStmt:
		return &ReturnStmt{Exprs: convertExprs(st.Exprs)}
	case *luaast.BreakStmt:
		return &BreakStmt{}
	case *luaast.LabelStmt:
		return &LabelStmt{Name: st.Name}
	case *luaast.GotoStmt:
		return &GotoStmt{Label: st.Label}
	default:
		return nil
	}
}

func convertExprs(exprs []luaast.Expr) []Expr {
	out := make([]Expr, len(exprs))
	for i, e := range exprs {
		out[i] = convertExpr(e)
	}
	return out
}

func convertExpr(e luaast.Expr) Expr {
	switch ex := e.(type) {
	case *luaast.TrueExpr:
		return &TrueExpr{}
	case *luaast.FalseExpr:
		return &FalseExpr{}
	case *luaast.NilExpr:
		return &NilExpr{}
	case *luaast.NumberExpr:
		return &NumberExpr{Value: ex.Value}
	case *luaast.StringExpr:
		return &StringExpr{Value: ex.Value}
	case *luaast.Comma3Expr:
		return &Comma3Expr{AdjustRet: ex.AdjustRet}
	case *luaast.IdentExpr:
		return &IdentExpr{Value: ex.Value}
	case *luaast.AttrGetExpr:
		return &AttrGetExpr{Object: convertExpr(ex.Object), Key: convertExpr(ex.Key)}
	case *luaast.TableExpr:
		fields := make([]*Field, len(ex.Fields))
		for i, f := range ex.Fields {
			fields[i] = convertField(f)
		}
		return &TableExpr{Fields: fields}
	case *luaast.FuncCallExpr:
		return &FuncCallExpr{Func: convertExpr(ex.Func), Receiver: convertExpr(ex.Receiver), Method: ex.Method, Args: convertExprs(ex.Args), AdjustRet: ex.AdjustRet}
	case *luaast.LogicalOpExpr:
		return &LogicalOpExpr{Operator: ex.Operator, Lhs: convertExpr(ex.Lhs), Rhs: convertExpr(ex.Rhs)}
	case *luaast.RelationalOpExpr:
		return &RelationalOpExpr{Operator: ex.Operator, Lhs: convertExpr(ex.Lhs), Rhs: convertExpr(ex.Rhs)}
	case *luaast.StringConcatOpExpr:
		return &StringConcatOpExpr{Lhs: convertExpr(ex.Lhs), Rhs: convertExpr(ex.Rhs)}
	case *luaast.ArithmeticOpExpr:
		return &ArithmeticOpExpr{Operator: ex.Operator, Lhs: convertExpr(ex.Lhs), Rhs: convertExpr(ex.Rhs)}
	case *luaast.UnaryMinusOpExpr:
		return &UnaryMinusOpExpr{Expr: convertExpr(ex.Expr)}
	case *luaast.UnaryNotOpExpr:
		return &UnaryNotOpExpr{Expr: convertExpr(ex.Expr)}
	case *luaast.UnaryLenOpExpr:
		return &UnaryLenOpExpr{Expr: convertExpr(ex.Expr)}
	case *luaast.FunctionExpr:
		return convertFunctionExpr(ex)
	default:
		return nil
	}
}

func convertFunctionExpr(fn *luaast.FunctionExpr) *FunctionExpr {
	if fn == nil {
		return nil
	}
	return &FunctionExpr{ParList: convertParList(fn.ParList), Stmts: convertStmts(fn.Stmts)}
}

func convertField(f *luaast.Field) *Field {
	if f == nil {
		return nil
	}
	return &Field{Key: convertExpr(f.Key), Value: convertExpr(f.Value)}
}

func convertParList(p *luaast.ParList) *ParList {
	if p == nil {
		return nil
	}
	return &ParList{HasVargs: p.HasVargs, Names: append([]string(nil), p.Names...)}
}

func convertFuncName(n *luaast.FuncName) *FuncName {
	if n == nil {
		return nil
	}
	return &FuncName{Func: convertExpr(n.Func), Receiver: convertExpr(n.Receiver), Method: n.Method}
}
