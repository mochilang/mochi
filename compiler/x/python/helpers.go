package pycode

// Helper functions used by the Python compiler. Splitting them into this file
// keeps compiler.go focused on the high level logic.

import (
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) writeln(s string) { c.writeIndent(); c.buf.WriteString(s); c.buf.WriteByte('\n') }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

var pyReserved = map[string]struct{}{
	// Python keywords
	"False": {}, "None": {}, "True": {}, "and": {}, "as": {}, "assert": {},
	"async": {}, "await": {}, "break": {}, "class": {}, "continue": {},
	"def": {}, "del": {}, "elif": {}, "else": {}, "except": {}, "finally": {},
	"for": {}, "from": {}, "global": {}, "if": {}, "import": {}, "in": {},
	"is": {}, "lambda": {}, "nonlocal": {}, "not": {}, "or": {}, "pass": {},
	"raise": {}, "return": {}, "try": {}, "while": {}, "with": {}, "yield": {},
	// Builtins commonly emitted by the compiler
	"range": {}, "len": {}, "print": {}, "sorted": {}, "list": {}, "dict": {},
	"set": {}, "str": {}, "int": {}, "float": {}, "bool": {}, "type": {}, "next": {},
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	res := b.String()
	if _, ok := pyReserved[res]; ok {
		return "_" + res
	}
	return res
}

func unexportName(name string) string {
	if name == "" {
		return ""
	}
	runes := []rune(name)
	if runes[0] >= 'A' && runes[0] <= 'Z' {
		runes[0] = runes[0] - 'A' + 'a'
	}
	return string(runes)
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if isInt64(a) && (isInt64(b) || isInt(b)) {
		return true
	}
	if isInt64(b) && (isInt64(a) || isInt(a)) {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

func isInt64(t types.Type) bool {
	_, ok := t.(types.Int64Type)
	return ok
}

func isInt(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return true
	default:
		return false
	}
}

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isNumeric(t types.Type) bool {
	return isInt(t) || isFloat(t)
}

func isBool(t types.Type) bool {
	_, ok := t.(types.BoolType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isAny(t types.Type) bool {
	if t == nil {
		return true
	}
	_, ok := t.(types.AnyType)
	return ok
}

func pyType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "float"
	case types.StringType:
		return "str"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "list[" + pyType(tt.Elem) + "]"
	case types.MapType:
		return "dict[" + pyType(tt.Key) + ", " + pyType(tt.Value) + "]"
	case types.GroupType:
		return "_Group[Any, " + pyType(tt.Elem) + "]"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = pyType(p)
		}
		return "typing.Callable[[" + strings.Join(params, ", ") + "], " + pyType(tt.Return) + "]"
	case types.VoidType:
		return "None"
	case types.AnyType:
		return "typing.Any"
	default:
		return "typing.Any"
	}
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func isLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 || len(e.Binary.Left.Ops) != 0 {
		return false
	}
	return isLiteralPrimary(e.Binary.Left.Value.Target)
}

func isLiteralPrimary(p *parser.Primary) bool {
	switch {
	case p.Lit != nil:
		return true
	case p.List != nil:
		for _, el := range p.List.Elems {
			if !isLiteralExpr(el) {
				return false
			}
		}
		return true
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if !isLiteralExpr(it.Key) || !isLiteralExpr(it.Value) {
				return false
			}
		}
		return true
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if !isLiteralExpr(f.Value) {
				return false
			}
		}
		return true
	case p.Group != nil:
		return isLiteralExpr(p.Group)
	default:
		return false
	}
}

func isPureExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		for _, op := range e.Binary.Right {
			if !isPurePostfixExpr(op.Right) {
				return false
			}
		}
	}
	return isPurePostfix(e.Binary.Left)
}

func isPurePostfix(u *parser.Unary) bool {
	if u == nil {
		return true
	}
	return isPurePostfixExpr(u.Value)
}

func isPurePostfixExpr(p *parser.PostfixExpr) bool {
	if p == nil {
		return true
	}
	if len(p.Ops) > 0 {
		for _, op := range p.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					if !isPureExpr(a) {
						return false
					}
				}
			}
			if op.Index != nil {
				if (op.Index.Start != nil && !isPureExpr(op.Index.Start)) ||
					(op.Index.End != nil && !isPureExpr(op.Index.End)) {
					return false
				}
			}
			if op.Cast != nil {
				continue
			}
		}
	}
	return isPurePrimary(p.Target)
}

func isPurePrimary(p *parser.Primary) bool {
	switch {
	case p.Generate != nil || p.Fetch != nil || p.Load != nil || p.Save != nil || p.Query != nil || p.LogicQuery != nil:
		return false
	case p.Call != nil:
		if p.Call.Func == "input" {
			return false
		}
		for _, a := range p.Call.Args {
			if !isPureExpr(a) {
				return false
			}
		}
		return true
	case p.Group != nil:
		return isPureExpr(p.Group)
	case p.List != nil:
		for _, el := range p.List.Elems {
			if !isPureExpr(el) {
				return false
			}
		}
		return true
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if !isPureExpr(it.Key) || !isPureExpr(it.Value) {
				return false
			}
		}
		return true
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if !isPureExpr(f.Value) {
				return false
			}
		}
		return true
	case p.Lit != nil, p.Selector != nil:
		return true
	default:
		return false
	}
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

// collectScopeInfo traverses statements to track locally declared variables and
// variables that are assigned to. Nested function bodies are ignored since they
// are handled separately when compiled.
func collectScopeInfo(stmts []*parser.Statement, locals, assigns map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			locals[s.Let.Name] = true
		case s.Var != nil:
			locals[s.Var.Name] = true
		case s.Assign != nil:
			assigns[s.Assign.Name] = true
		case s.For != nil:
			locals[s.For.Name] = true
			collectScopeInfo(s.For.Body, locals, assigns)
		case s.While != nil:
			collectScopeInfo(s.While.Body, locals, assigns)
		case s.If != nil:
			collectScopeInfo(s.If.Then, locals, assigns)
			if s.If.ElseIf != nil {
				collectScopeInfo([]*parser.Statement{{If: s.If.ElseIf}}, locals, assigns)
			}
			collectScopeInfo(s.If.Else, locals, assigns)
		case s.Test != nil:
			collectScopeInfo(s.Test.Body, locals, assigns)
		case s.On != nil:
			collectScopeInfo(s.On.Body, locals, assigns)
		case s.Agent != nil:
			for _, blk := range s.Agent.Body {
				switch {
				case blk.Let != nil:
					locals[blk.Let.Name] = true
				case blk.Var != nil:
					locals[blk.Var.Name] = true
				case blk.Assign != nil:
					assigns[blk.Assign.Name] = true
				case blk.On != nil:
					collectScopeInfo(blk.On.Body, locals, assigns)
				case blk.Intent != nil:
					collectScopeInfo(blk.Intent.Body, locals, assigns)
				}
			}
		}
	}
}

// exprVars collects variable names referenced in expression e.
func exprVars(e *parser.Expr, vars map[string]struct{}) {
	if e == nil || e.Binary == nil {
		return
	}
	var scanUnary func(*parser.Unary)
	var scanPostfix func(*parser.PostfixExpr)
	var scanPrimary func(*parser.Primary)

	scanUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Index != nil {
				exprVars(op.Index.Start, vars)
				exprVars(op.Index.End, vars)
			}
			if op.Call != nil {
				for _, a := range op.Call.Args {
					exprVars(a, vars)
				}
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		if p.Selector != nil {
			vars[p.Selector.Root] = struct{}{}
		}
		if p.Group != nil {
			exprVars(p.Group, vars)
		}
		if p.FunExpr != nil {
			exprVars(p.FunExpr.ExprBody, vars)
			for _, st := range p.FunExpr.BlockBody {
				scanStmtVars(st, vars)
			}
		}
		if p.List != nil {
			for _, el := range p.List.Elems {
				exprVars(el, vars)
			}
		}
		if p.Map != nil {
			for _, it := range p.Map.Items {
				exprVars(it.Key, vars)
				exprVars(it.Value, vars)
			}
		}
		if p.Call != nil {
			for _, a := range p.Call.Args {
				exprVars(a, vars)
			}
		}
	}

	scanUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right)
	}
}

func scanStmtVars(s *parser.Statement, vars map[string]struct{}) {}

// whereEvalLevel returns the earliest FROM clause index where the query's WHERE
// predicate can be evaluated. Index 0 refers to the initial source variable.
func whereEvalLevel(q *parser.QueryExpr) int {
	if q.Where == nil {
		return len(q.Froms)
	}
	vars := map[string]struct{}{}
	exprVars(q.Where, vars)
	positions := map[string]int{q.Var: 0}
	for i, f := range q.Froms {
		positions[f.Var] = i + 1
	}
	level := 0
	for v := range vars {
		if idx, ok := positions[v]; ok && idx > level {
			level = idx
		}
	}
	if level > len(q.Froms) {
		level = len(q.Froms)
	}
	return level
}

func unionFieldPathType(ut types.UnionType, tail []string) (types.Type, bool) {
	var result types.Type
	for _, variant := range ut.Variants {
		cur := types.Type(variant)
		for _, field := range tail {
			st, ok := cur.(types.StructType)
			if !ok {
				return nil, false
			}
			ft, ok := st.Fields[field]
			if !ok {
				return nil, false
			}
			cur = ft
		}
		if result == nil {
			result = cur
		} else if !equalTypes(result, cur) {
			return nil, false
		}
	}
	if result == nil {
		return nil, false
	}
	return result, true
}
