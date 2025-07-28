//go:build slow

package lua

import (
	"bytes"
	"fmt"
	"regexp"
	"strconv"
	"strings"

	luaast "github.com/yuin/gopher-lua/ast"
	luaparse "github.com/yuin/gopher-lua/parse"

	"mochi/ast"
	"mochi/parser"
)

// Parse parses Lua source code into a slice of AST statements.
func Parse(src string) ([]luaast.Stmt, error) {
	pre := preprocessLuaSource(src)
	chunk, err := luaparse.Parse(bytes.NewReader([]byte(pre)), "src.lua")
	if err != nil {
		return nil, fmt.Errorf("%s", formatLuaParseError(err, src))
	}
	return chunk, nil
}

// ConvertSource converts parsed Lua statements into Mochi source code.
func ConvertSource(stmts []luaast.Stmt) (string, error) {
	var b strings.Builder
	writeLuaChunk(&b, stmts)
	return b.String(), nil
}

// Convert converts parsed Lua statements into a Mochi AST node.
func Convert(stmts []luaast.Stmt) (*ast.Node, error) {
	src, err := ConvertSource(stmts)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

func convertLuaStmts(stmts []luaast.Stmt, indent int, vars map[string]bool) []string {
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
						out = append(out, convertLuaStmts(fn.Stmts, indent+1, map[string]bool{})...)
						out = append(out, ind+"}")
						vars[n] = true
						continue
					}
				}
				val := ""
				if i < len(s.Exprs) {
					val = luaExprString(s.Exprs[i])
				}
				out = append(out, ind+"let "+n+" = "+val)
				vars[n] = true
			}
		case *luaast.AssignStmt:
			for i, lh := range s.Lhs {
				name := luaExprString(lh)
				val := ""
				if i < len(s.Rhs) {
					val = luaExprString(s.Rhs[i])
				}
				if vars[name] {
					out = append(out, ind+name+" = "+val)
				} else if strings.ContainsAny(name, "[].") {
					out = append(out, ind+name+" = "+val)
				} else {
					out = append(out, ind+"let "+name+" = "+val)
					vars[name] = true
				}
			}
		case *luaast.ReturnStmt:
			if len(s.Exprs) == 0 {
				out = append(out, ind+"return")
			} else if len(s.Exprs) == 1 {
				out = append(out, ind+"return "+luaExprString(s.Exprs[0]))
			} else {
				var parts []string
				for _, e := range s.Exprs {
					parts = append(parts, luaExprString(e))
				}
				out = append(out, ind+"return ("+strings.Join(parts, ", ")+")")
			}
		case *luaast.FuncCallStmt:
			out = append(out, ind+luaExprString(s.Expr))
		case *luaast.BreakStmt:
			out = append(out, ind+"break")
		case *luaast.DoBlockStmt:
			out = append(out, ind+"do {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, map[string]bool{})...)
			out = append(out, ind+"}")
		case *luaast.WhileStmt:
			cond := luaExprString(s.Condition)
			out = append(out, ind+"while "+cond+" {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, map[string]bool{})...)
			out = append(out, ind+"}")
		case *luaast.RepeatStmt:
			out = append(out, ind+"do {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, map[string]bool{})...)
			cond := luaExprString(s.Condition)
			out = append(out, ind+"} while !("+cond+")")
		case *luaast.IfStmt:
			writeLuaIfStmt(&out, s, indent)
		case *luaast.NumberForStmt:
			step := "1"
			if s.Step != nil {
				step = luaExprString(s.Step)
			}
			init := luaExprString(s.Init)
			limit := luaExprString(s.Limit)
			out = append(out, ind+fmt.Sprintf("for %s = %s; %s <= %s; %s += %s {", s.Name, init, s.Name, limit, s.Name, step))
			out = append(out, convertLuaStmts(s.Stmts, indent+1, map[string]bool{})...)
			out = append(out, ind+"}")
		case *luaast.GenericForStmt:
			iter := ""
			if len(s.Exprs) > 0 {
				iter = luaExprString(s.Exprs[0])
			}
			out = append(out, ind+"for "+strings.Join(s.Names, ", ")+" in "+iter+" {")
			out = append(out, convertLuaStmts(s.Stmts, indent+1, map[string]bool{})...)
			out = append(out, ind+"}")
		case *luaast.FuncDefStmt:
			origName := ""
			if id, ok := s.Name.Func.(*luaast.IdentExpr); ok {
				origName = id.Value
			}
			name := luaExprString(s.Name.Func)
			if s.Name.Method != "" {
				recv := luaExprString(s.Name.Func)
				name = recv + "." + s.Name.Method
			}
			if strings.HasPrefix(origName, "__") {
				continue
			}
			out = append(out, ind+"fun "+name+luaFuncSignature(s.Func)+" {")
			out = append(out, convertLuaStmts(s.Func.Stmts, indent+1, map[string]bool{})...)
			out = append(out, ind+"}")
		}
	}
	return out
}

func writeLuaIfStmt(out *[]string, s *luaast.IfStmt, indent int) {
	ind := strings.Repeat("  ", indent)
	*out = append(*out, ind+"if "+luaExprString(s.Condition)+" {")
	*out = append(*out, convertLuaStmts(s.Then, indent+1, map[string]bool{})...)
	cur := s
	for {
		if len(cur.Else) == 1 {
			if next, ok := cur.Else[0].(*luaast.IfStmt); ok {
				*out = append(*out, ind+"} else if "+luaExprString(next.Condition)+" {")
				*out = append(*out, convertLuaStmts(next.Then, indent+1, map[string]bool{})...)
				cur = next
				continue
			}
		}
		if len(cur.Else) > 0 {
			*out = append(*out, ind+"} else {")
			*out = append(*out, convertLuaStmts(cur.Else, indent+1, map[string]bool{})...)
			*out = append(*out, ind+"}")
		} else {
			*out = append(*out, ind+"}")
		}
		break
	}
}

func luaExprString(e luaast.Expr) string {
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
		return luaExprString(v.Lhs) + " " + v.Operator + " " + luaExprString(v.Rhs)
	case *luaast.StringConcatOpExpr:
		return luaExprString(v.Lhs) + " + " + luaExprString(v.Rhs)
	case *luaast.LogicalOpExpr:
		op := v.Operator
		if op == "and" {
			op = "&&"
		} else if op == "or" {
			op = "||"
		}
		return luaExprString(v.Lhs) + " " + op + " " + luaExprString(v.Rhs)
	case *luaast.RelationalOpExpr:
		return luaExprString(v.Lhs) + " " + v.Operator + " " + luaExprString(v.Rhs)
	case *luaast.UnaryMinusOpExpr:
		return "-" + luaExprString(v.Expr)
	case *luaast.UnaryNotOpExpr:
		return "!" + luaExprString(v.Expr)
	case *luaast.UnaryLenOpExpr:
		return "len(" + luaExprString(v.Expr) + ")"
	case *luaast.FuncCallExpr:
		var args []string
		for _, a := range v.Args {
			args = append(args, luaExprString(a))
		}
		callee := luaExprString(v.Func)
		if v.Method != "" {
			callee = luaExprString(v.Receiver) + "." + v.Method
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
		b.WriteString(" {")
		for _, line := range convertLuaStmts(v.Stmts, 1, map[string]bool{}) {
			b.WriteByte('\n')
			b.WriteString("  ")
			b.WriteString(line)
		}
		b.WriteByte('\n')
		b.WriteString("}")
		return b.String()
	case *luaast.AttrGetExpr:
		if k, ok := v.Key.(*luaast.StringExpr); ok {
			return luaExprString(v.Object) + "[" + strconv.Quote(k.Value) + "]"
		}
		if _, ok := v.Key.(*luaast.IdentExpr); ok {
			return luaExprString(v.Object) + "." + luaExprString(v.Key)
		}
		return luaExprString(v.Object) + "[" + luaExprString(v.Key) + "]"
	case *luaast.TableExpr:
		isMap := false
		var items []string
		for _, f := range v.Fields {
			if f.Key != nil {
				isMap = true
				items = append(items, luaExprString(f.Key)+": "+luaExprString(f.Value))
			} else {
				items = append(items, luaExprString(f.Value))
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
	if fn.ParList != nil {
		for _, n := range fn.ParList.Names {
			params = append(params, n)
		}
	}
	return "(" + strings.Join(params, ", ") + ")"
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

func writeLuaChunk(out *strings.Builder, chunk []luaast.Stmt) {
	for _, line := range convertLuaStmts(chunk, 0, map[string]bool{}) {
		if strings.HasPrefix(line, "fun __") {
			continue
		}
		out.WriteString(line)
		out.WriteByte('\n')
	}
}

func formatLuaParseError(err error, src string) string {
	msg := err.Error()
	line := 0
	col := 0
	re := regexp.MustCompile(`line:(\d+)(?:\(column:(\d+)\))?`)
	if m := re.FindStringSubmatch(msg); len(m) > 0 {
		line, _ = strconv.Atoi(m[1])
		if len(m) > 2 {
			col, _ = strconv.Atoi(m[2])
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
