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
	mut := collectMutables(stmts)
	writeLuaChunk(&b, stmts, mut)
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
			out = append(out, ind+luaExprString(s.Expr, mut))
		case *luaast.BreakStmt:
			out = append(out, ind+"break")
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
		return luaExprString(v.Lhs, mut) + " " + v.Operator + " " + luaExprString(v.Rhs, mut)
	case *luaast.StringConcatOpExpr:
		return luaExprString(v.Lhs, mut) + " + " + luaExprString(v.Rhs, mut)
	case *luaast.LogicalOpExpr:
		op := v.Operator
		if op == "and" {
			op = "&&"
		} else if op == "or" {
			op = "||"
		}
		return luaExprString(v.Lhs, mut) + " " + op + " " + luaExprString(v.Rhs, mut)
	case *luaast.RelationalOpExpr:
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
	if fn.ParList != nil {
		for _, n := range fn.ParList.Names {
			params = append(params, n)
		}
	}
	return "(" + strings.Join(params, ", ") + ")"
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
				if id, ok := lh.(*luaast.IdentExpr); ok {
					counts[id.Value]++
				}
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
