//go:build slow

package rustcode

import (
	"bytes"
	"fmt"
	"math"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a subset of Mochi to Rust source code.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	helpers       map[string]bool
	tmp           int
	env           *types.Env
	structs       map[string]types.StructType
	genStructs    []types.StructType
	structCounter int
	inMain        bool
	globals       map[string]bool
	mutParams     map[string]map[int]bool
	listVars      map[string]string
	variantInfo   map[string]struct {
		Union  string
		Fields map[string]types.Type
		Order  []string
	}
	groupVars      []string
	enumDecls      []string
	lastListStruct string
}

func (c *Compiler) fieldType(e *parser.Expr) types.Type {
	if name, arg, ok := c.aggCall(e); ok {
		at := types.ExprType(arg, c.env)
		if lt, ok := at.(types.ListType); ok {
			at = lt.Elem
		}
		if _, ok := at.(types.AnyType); ok {
			if t := c.inferAggElemType(arg); t != nil {
				at = t
			}
		}
		switch name {
		case "avg":
			return types.FloatType{}
		case "sum":
			if _, ok := at.(types.FloatType); ok || c.exprHasFloat(arg) {
				return types.FloatType{}
			}
			return types.IntType{}
		case "min", "max":
			return at
		case "len", "count":
			return types.IntType{}
		}
	}
	if e == nil || e.Binary == nil {
		return types.TypeOfExprBasic(e, c.env)
	}
	if len(e.Binary.Right) > 0 {
		op := e.Binary.Right[0].Op
		switch op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return types.BoolType{}
		case "/":
			lt := types.TypeOfUnary(e.Binary.Left, c.env)
			rt := types.TypeOfPostfix(e.Binary.Right[0].Right, c.env)
			isInt := func(t types.Type) bool {
				switch t.(type) {
				case types.IntType, types.Int64Type:
					return true
				}
				return false
			}
			if isInt(lt) && isInt(rt) {
				return types.FloatType{}
			}
		case "*", "+", "-", "%":
			return types.ExprType(e, c.env)
		}
		return types.ExprType(e, c.env)
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return types.TypeOfExprBasic(e, c.env)
	}
	pf := u.Value
	var root, field string
	switch {
	case len(pf.Ops) == 1 && pf.Ops[0].Field != nil && pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0:
		root = pf.Target.Selector.Root
		field = pf.Ops[0].Field.Name
	case len(pf.Ops) == 0 && pf.Target != nil && pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1:
		root = pf.Target.Selector.Root
		field = pf.Target.Selector.Tail[0]
	default:
		return types.TypeOfExprBasic(e, c.env)
	}
	if structName, ok := c.listVars[root]; ok {
		if st, ok2 := c.structs[structName]; ok2 {
			if t, ok3 := st.Fields[field]; ok3 {
				return t
			}
		}
	}
	return types.TypeOfExprBasic(e, c.env)
}

func (c *Compiler) exprHasFloat(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if _, ok := types.TypeOfExprBasic(e, c.env).(types.FloatType); ok {
		return true
	}
	if e.Binary != nil {
		if c.unaryHasFloat(e.Binary.Left) {
			return true
		}
		for _, op := range e.Binary.Right {
			if c.postfixHasFloat(op.Right) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) unaryHasFloat(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	if c.postfixHasFloat(u.Value) {
		return true
	}
	return false
}

func (c *Compiler) postfixHasFloat(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if c.primaryHasFloat(p.Target) {
		return true
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if c.exprHasFloat(op.Index.Start) || c.exprHasFloat(op.Index.End) {
				return true
			}
		} else if op.Call != nil {
			for _, a := range op.Call.Args {
				if c.exprHasFloat(a) {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) primaryHasFloat(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if _, ok := types.TypeOfPrimaryBasic(p, c.env).(types.FloatType); ok {
		return true
	}
	switch {
	case p.Group != nil:
		return c.exprHasFloat(p.Group)
	case p.If != nil:
		return c.exprHasFloat(p.If.Then) || c.exprHasFloat(p.If.Else) || c.exprHasFloat(p.If.Cond)
	case p.List != nil:
		for _, e2 := range p.List.Elems {
			if c.exprHasFloat(e2) {
				return true
			}
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if c.exprHasFloat(it.Value) {
				return true
			}
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			if c.exprHasFloat(f.Value) {
				return true
			}
		}
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody != nil {
			return c.exprHasFloat(p.FunExpr.ExprBody)
		}
	}
	return false
}

func usesIdentExpr(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if usesIdentUnary(e.Binary.Left, name) {
		return true
	}
	for _, op := range e.Binary.Right {
		if usesIdentPostfix(op.Right, name) {
			return true
		}
	}
	return false
}

func usesIdentUnary(u *parser.Unary, name string) bool {
	if u == nil {
		return false
	}
	if usesIdentPostfix(u.Value, name) {
		return true
	}
	if len(u.Ops) > 0 {
		// unary ops don't contain identifiers
	}
	return false
}

func usesIdentPostfix(p *parser.PostfixExpr, name string) bool {
	if p == nil {
		return false
	}
	if usesIdentPrimary(p.Target, name) {
		return true
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if usesIdentExpr(op.Index.Start, name) || usesIdentExpr(op.Index.End, name) {
				return true
			}
		} else if op.Call != nil {
			for _, a := range op.Call.Args {
				if usesIdentExpr(a, name) {
					return true
				}
			}
		}
	}
	return false
}

func usesIdentPrimary(p *parser.Primary, name string) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Selector != nil:
		if p.Selector.Root == name {
			return true
		}
	case p.Call != nil:
		if p.Call.Func == name {
			return true
		}
		for _, a := range p.Call.Args {
			if usesIdentExpr(a, name) {
				return true
			}
		}
	case p.Group != nil:
		return usesIdentExpr(p.Group, name)
	case p.If != nil:
		if usesIdentExpr(p.If.Cond, name) || usesIdentExpr(p.If.Then, name) || usesIdentExpr(p.If.Else, name) {
			return true
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			if usesIdentExpr(e, name) {
				return true
			}
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			if usesIdentExpr(it.Value, name) {
				return true
			}
		}
	case p.Struct != nil:
		for _, it := range p.Struct.Fields {
			if usesIdentExpr(it.Value, name) {
				return true
			}
		}
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody != nil {
			return usesIdentExpr(p.FunExpr.ExprBody, name)
		}
		for _, s := range p.FunExpr.BlockBody {
			if usesIdentStmt(s, name) {
				return true
			}
		}
	}
	return false
}

func usesIdentStmt(s *parser.Statement, name string) bool {
	switch {
	case s.Let != nil:
		return usesIdentExpr(s.Let.Value, name)
	case s.Var != nil:
		return usesIdentExpr(s.Var.Value, name)
	case s.Assign != nil:
		if usesIdentExpr(s.Assign.Value, name) {
			return true
		}
		for _, idx := range s.Assign.Index {
			if usesIdentExpr(idx.Start, name) || usesIdentExpr(idx.End, name) {
				return true
			}
		}
		return false
	case s.Return != nil:
		return usesIdentExpr(s.Return.Value, name)
	case s.Expr != nil:
		return usesIdentExpr(s.Expr.Expr, name)
	case s.If != nil:
		if usesIdentExpr(s.If.Cond, name) {
			return true
		}
		for _, stmt := range s.If.Then {
			if usesIdentStmt(stmt, name) {
				return true
			}
		}
		for _, stmt := range s.If.Else {
			if usesIdentStmt(stmt, name) {
				return true
			}
		}
	case s.For != nil:
		if usesIdentExpr(s.For.Source, name) || usesIdentExpr(s.For.RangeEnd, name) {
			return true
		}
		for _, stmt := range s.For.Body {
			if usesIdentStmt(stmt, name) {
				return true
			}
		}
	case s.While != nil:
		if usesIdentExpr(s.While.Cond, name) {
			return true
		}
		for _, stmt := range s.While.Body {
			if usesIdentStmt(stmt, name) {
				return true
			}
		}
	case s.Fun != nil:
		for _, stmt := range s.Fun.Body {
			if usesIdentStmt(stmt, name) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("tmp%d", c.tmp)
}

func titleCase(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

func singular(s string) string {
	if strings.HasSuffix(s, "s") && len(s) > 1 {
		return s[:len(s)-1]
	}
	return s
}

func (c *Compiler) newStructName(base string) string {
	name := titleCase(base)
	if _, ok := c.structs[name]; !ok {
		return name
	}
	for {
		c.structCounter++
		candidate := fmt.Sprintf("%s%d", name, c.structCounter)
		if _, ok := c.structs[candidate]; !ok {
			return candidate
		}
	}
}

// New returns a new Compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		helpers:    make(map[string]bool),
		env:        env,
		structs:    make(map[string]types.StructType),
		genStructs: []types.StructType{},
		inMain:     true,
		globals:    make(map[string]bool),
		mutParams:  make(map[string]map[int]bool),
		listVars:   make(map[string]string),
		variantInfo: make(map[string]struct {
			Union  string
			Fields map[string]types.Type
			Order  []string
		}),
		groupVars:      []string{},
		enumDecls:      []string{},
		lastListStruct: "",
	}
}

func (c *Compiler) pushGroupVar(name string) {
	c.groupVars = append(c.groupVars, name)
}

func (c *Compiler) popGroupVar() {
	if len(c.groupVars) > 0 {
		c.groupVars = c.groupVars[:len(c.groupVars)-1]
	}
}

func (c *Compiler) isGroupVar(name string) bool {
	for i := len(c.groupVars) - 1; i >= 0; i-- {
		if c.groupVars[i] == name {
			return true
		}
	}
	return false
}

func loopHead(varName, src string, env *types.Env) string {
	if env != nil {
		if t, err := env.GetVar(varName); err == nil {
			if _, ok := t.(types.StructType); ok {
				return fmt.Sprintf("for %s in &%s {", varName, src)
			}
		}
	}
	return fmt.Sprintf("for &%s in &%s {", varName, src)
}

func loopVal(varName string, env *types.Env) string {
	if env != nil {
		if t, err := env.GetVar(varName); err == nil {
			if _, ok := t.(types.StructType); ok {
				return varName + ".clone()"
			}
		}
	}
	return varName
}

func defaultDecl(varName string, env *types.Env) string {
	if env != nil {
		if t, err := env.GetVar(varName); err == nil {
			return defaultDeclType(varName, t)
		}
	}
	return fmt.Sprintf("let %s = Default::default();", varName)
}

func defaultDeclType(varName string, t types.Type) string {
	if st, ok := t.(types.StructType); ok {
		return fmt.Sprintf("let %s: %s = Default::default();", varName, st.Name)
	}
	return fmt.Sprintf("let %s = Default::default();", varName)
}

func stmtMutatesVar(s *parser.Statement, name string) bool {
	switch {
	case s.Assign != nil:
		if s.Assign.Name == name {
			return true
		}
	case s.For != nil:
		for _, b := range s.For.Body {
			if stmtMutatesVar(b, name) {
				return true
			}
		}
	case s.While != nil:
		for _, b := range s.While.Body {
			if stmtMutatesVar(b, name) {
				return true
			}
		}
	case s.If != nil:
		for _, b := range s.If.Then {
			if stmtMutatesVar(b, name) {
				return true
			}
		}
		for _, b := range s.If.Else {
			if stmtMutatesVar(b, name) {
				return true
			}
		}
	case s.Fun != nil:
		for _, b := range s.Fun.Body {
			if stmtMutatesVar(b, name) {
				return true
			}
		}
	}
	return false
}

func analyzeMutations(prog *parser.Program) map[string]map[int]bool {
	res := make(map[string]map[int]bool)
	for _, st := range prog.Statements {
		if st.Fun == nil {
			continue
		}
		f := st.Fun
		mp := make(map[int]bool)
		for i, p := range f.Params {
			for _, b := range f.Body {
				if stmtMutatesVar(b, p.Name) {
					mp[i] = true
					break
				}
			}
		}
		if len(mp) > 0 {
			res[f.Name] = mp
		}
	}
	return res
}

// inferAggElemType deduces the element type of an aggregation argument.
// It handles queries over group variables where regular inference results in AnyType.
func (c *Compiler) inferAggElemType(e *parser.Expr) types.Type {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil || u.Value.Target.Query == nil {
		return nil
	}
	q := u.Value.Target.Query
	if name, ok := c.simpleIdent(q.Source); ok && c.isGroupVar(name) {
		if structName, ok2 := c.listVars[name]; ok2 {
			if st, ok3 := c.structs[structName]; ok3 {
				if t, ok4 := st.Fields["items"]; ok4 {
					if lt, ok5 := t.(types.ListType); ok5 {
						child := types.NewEnv(c.env)
						child.SetVar(q.Var, lt.Elem, true)
						return types.ExprType(q.Select, child)
					}
				}
			}
		}
	}
	return nil
}

func isEqHashable(t types.Type) bool {
	switch tt := t.(type) {
	case types.FloatType:
		return false
	case types.ListType:
		return isEqHashable(tt.Elem)
	case types.MapType:
		return false
	case types.StructType:
		for _, f := range tt.Order {
			if !isEqHashable(tt.Fields[f]) {
				return false
			}
		}
	case types.UnionType:
		return false
	}
	return true
}

// Compile converts a parsed Mochi program into Rust source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.helpers = map[string]bool{}
	c.inMain = true
	c.globals = map[string]bool{}
	c.enumDecls = []string{}
	c.mutParams = analyzeMutations(prog)
	c.writeln("fn main() {")
	c.indent++
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	// prepend generated enums, structs and helpers
	var out bytes.Buffer
	for _, decl := range c.enumDecls {
		out.WriteString(decl)
		if !strings.HasSuffix(decl, "\n") {
			out.WriteByte('\n')
		}
		out.WriteByte('\n')
	}
	for _, st := range c.genStructs {
		derives := "#[derive(Default, Debug, Clone, PartialEq"
		if isEqHashable(st) {
			derives += ", Eq, Hash"
		}
		out.WriteString(derives + ")]\n")
		out.WriteString("struct " + st.Name + " {\n")
		for _, f := range st.Order {
			out.WriteString("    " + f + ": " + rustTypeFromType(st.Fields[f]) + ",\n")
		}
		out.WriteString("}\n\n")
	}
	if c.helpers["append"] {
		out.WriteString("fn append<T: Clone>(mut v: Vec<T>, item: T) -> Vec<T> {\n")
		out.WriteString("    v.push(item);\n    v\n}\n\n")
	}
	if c.helpers["avg"] {
		out.WriteString("fn avg(v: &[i32]) -> f64 {\n    let sum: i32 = v.iter().sum();\n    sum as f64 / v.len() as f64\n}\n\n")
	}
	if c.helpers["sum"] {
		out.WriteString("fn sum<T>(v: &[T]) -> T where T: std::iter::Sum<T> + Copy {\n    v.iter().copied().sum()\n}\n\n")
	}
	if c.helpers["min"] {
		out.WriteString("fn min(v: &[i32]) -> i32 {\n    *v.iter().min().unwrap()\n}\n\n")
	}
	if c.helpers["max"] {
		out.WriteString("fn max(v: &[i32]) -> i32 {\n    *v.iter().max().unwrap()\n}\n\n")
	}
	if c.helpers["_union"] {
		out.WriteString("fn _union<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    use std::collections::HashSet;\n    let mut set: HashSet<T> = a.into_iter().collect();\n    set.extend(b.into_iter());\n    set.into_iter().collect()\n}\n\n")
	}
	if c.helpers["_union_all"] {
		out.WriteString("fn _union_all<T: Clone>(mut a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    a.extend(b);\n    a\n}\n\n")
	}
	if c.helpers["_except"] {
		out.WriteString("fn _except<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    use std::collections::HashSet;\n    let set: HashSet<T> = b.into_iter().collect();\n    a.into_iter().filter(|x| !set.contains(x)).collect()\n}\n\n")
	}
	if c.helpers["_intersect"] {
		out.WriteString("fn _intersect<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    use std::collections::HashSet;\n    let set: HashSet<T> = b.into_iter().collect();\n    a.into_iter().filter(|x| set.contains(x)).collect()\n}\n\n")
	}
	if c.helpers["_load"] {
		out.WriteString("fn _load<T: Default + Clone>(_path: &str, _opts: std::collections::HashMap<String, String>) -> Vec<T> {\n    Vec::new()\n}\n\n")
	}
	if c.helpers["_save"] {
		out.WriteString("fn _save<T>(_src: &[T], _path: &str, _opts: std::collections::HashMap<String, String>) { }\n\n")
	}
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Update != nil:
		return c.compileUpdateStmt(s.Update)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	if l.Value == nil {
		if l.Type == nil {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		typ := rustType(l.Type)
		c.writeln(fmt.Sprintf("let %s: %s = %s;", l.Name, typ, rustDefault(typ)))
		if c.inMain && c.indent == 1 {
			c.globals[l.Name] = true
		}
		return nil
	}
	if list := tryListLiteral(l.Value); list != nil {
		if code, ok, err := c.tryMapListStruct(l.Name, list); ok {
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let %s = %s;", l.Name, code))
			if c.env != nil {
				if structName, ok2 := c.listVars[l.Name]; ok2 {
					if st, ok3 := c.structs[structName]; ok3 {
						c.env.SetVar(l.Name, types.ListType{Elem: st}, false)
					}
				}
			}
			if c.inMain && c.indent == 1 {
				c.globals[l.Name] = true
			}
			return nil
		}
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	if l.Type != nil {
		typ := rustType(l.Type)
		c.writeln(fmt.Sprintf("let %s: %s = %s;", l.Name, typ, val))
		if c.env != nil {
			c.env.SetVar(l.Name, types.ResolveTypeRef(l.Type, c.env), false)
		}
	} else {
		c.writeln(fmt.Sprintf("let %s = %s;", l.Name, val))
		if c.env != nil {
			t := types.TypeOfExpr(l.Value, c.env)
			c.env.SetVar(l.Name, t, false)
			if lt, ok := t.(types.ListType); ok {
				if st, ok2 := lt.Elem.(types.StructType); ok2 {
					c.listVars[l.Name] = st.Name
				}
			}
		}
	}
	if c.lastListStruct != "" {
		c.listVars[l.Name] = c.lastListStruct
		c.lastListStruct = ""
	}
	if c.inMain && c.indent == 1 {
		c.globals[l.Name] = true
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	if v.Value == nil {
		if v.Type == nil {
			return fmt.Errorf("var without value at line %d", v.Pos.Line)
		}
		typ := rustType(v.Type)
		c.writeln(fmt.Sprintf("let mut %s: %s = %s;", v.Name, typ, rustDefault(typ)))
		if c.inMain && c.indent == 1 {
			c.globals[v.Name] = true
		}
		return nil
	}
	if list := tryListLiteral(v.Value); list != nil {
		if code, ok, err := c.tryMapListStruct(v.Name, list); ok {
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let mut %s = %s;", v.Name, code))
			if c.env != nil {
				if structName, ok2 := c.listVars[v.Name]; ok2 {
					if st, ok3 := c.structs[structName]; ok3 {
						c.env.SetVar(v.Name, types.ListType{Elem: st}, true)
					}
				}
			}
			if c.inMain && c.indent == 1 {
				c.globals[v.Name] = true
			}
			return nil
		}
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type != nil {
		typ := rustType(v.Type)
		c.writeln(fmt.Sprintf("let mut %s: %s = %s;", v.Name, typ, val))
		if c.env != nil {
			c.env.SetVar(v.Name, types.ResolveTypeRef(v.Type, c.env), true)
		}
	} else {
		c.writeln(fmt.Sprintf("let mut %s = %s;", v.Name, val))
		if c.env != nil {
			t := types.TypeOfExpr(v.Value, c.env)
			c.env.SetVar(v.Name, t, true)
			if lt, ok := t.(types.ListType); ok {
				if st, ok2 := lt.Elem.(types.StructType); ok2 {
					c.listVars[v.Name] = st.Name
				}
			}
		}
	}
	if c.lastListStruct != "" {
		c.listVars[v.Name] = c.lastListStruct
		c.lastListStruct = ""
	}
	if c.inMain && c.indent == 1 {
		c.globals[v.Name] = true
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	prefix := &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: a.Name}}}}
	// handle map assignments specially
	if len(a.Index) > 0 && len(a.Field) == 0 {
		t, _ := c.env.GetVar(a.Name)
		if len(a.Index) == 1 && types.IsMapType(t) {
			idxStr, err := c.compileExpr(a.Index[0].Start)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s.insert(%s, %s);", target, idxStr, val))
			return nil
		}
		if len(a.Index) == 2 && types.IsMapType(t) {
			first, err := c.compileExpr(a.Index[0].Start)
			if err != nil {
				return err
			}
			second, err := c.compileExpr(a.Index[1].Start)
			if err != nil {
				return err
			}
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("if let Some(%s) = %s.get_mut(%s) {", tmp, target, first))
			c.indent++
			c.writeln(fmt.Sprintf("%s.insert(%s, %s);", tmp, second, val))
			c.indent--
			c.writeln("}")
			return nil
		}
	}
	for _, idx := range a.Index {
		idxStr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		t := types.TypeOfPostfixBasic(prefix, c.env)
		if types.IsMapType(t) {
			target = fmt.Sprintf("%s[%s]", target, idxStr)
		} else {
			target = fmt.Sprintf("%s[%s as usize]", target, idxStr)
		}
		prefix.Value.Ops = append(prefix.Value.Ops, &parser.PostfixOp{Index: idx})
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s.%s", target, f.Name)
		prefix.Value.Ops = append(prefix.Value.Ops, &parser.PostfixOp{Field: f})
	}
	c.writeln(fmt.Sprintf("%s = %s;", target, val))
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileCond(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, s := range i.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else != nil {
		c.writeln("} else {")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	} else {
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	if id, ok := c.simpleIdent(f.Source); ok && c.isGroupVar(id) {
		src += ".items"
	}
	var elemType types.Type
	if f.RangeEnd != nil {
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		elemType = types.IntType{}
		c.writeln(fmt.Sprintf("for %s in (%s as i32)..(%s as i32) {", f.Name, src, end))
	} else if mt, ok := types.TypeOfExprBasic(f.Source, c.env).(types.MapType); ok {
		elemType = mt.Key
		c.writeln(fmt.Sprintf("for %s in %s.keys() {", f.Name, src))
	} else if lt, ok := types.TypeOfExprBasic(f.Source, c.env).(types.ListType); ok {
		elemType = lt.Elem
		c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
	} else {
		elemType = types.AnyType{}
		c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
	}
	if c.env != nil {
		c.env.SetVar(f.Name, elemType, true)
	}
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileCond(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " {")
	c.indent++
	for _, s := range w.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileUpdateStmt(u *parser.UpdateStmt) error {
	list := u.Target
	idx := c.newTmp()
	item := c.newTmp()
	mutList := list
	if m, err := c.env.IsMutable(list); err != nil || !m {
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("let mut %s = %s.clone();", tmp, list))
		mutList = tmp
	}
	c.writeln(fmt.Sprintf("for %s in 0..%s.len() {", idx, mutList))
	c.indent++
	c.writeln(fmt.Sprintf("let mut %s = %s[%s].clone();", item, mutList, idx))

	orig := c.env
	if c.env != nil {
		if lt, err := c.env.GetVar(list); err == nil {
			if lt2, ok2 := lt.(types.ListType); ok2 {
				if st, ok3 := lt2.Elem.(types.StructType); ok3 {
					child := types.NewEnv(c.env)
					for _, f := range st.Order {
						child.SetVar(f, st.Fields[f], true)
						c.writeln(fmt.Sprintf("let mut %s = %s.%s;", f, item, f))
					}
					c.env = child
				}
			}
		}
	}

	if u.Where != nil {
		cond, err := c.compileCond(u.Where)
		if err != nil {
			c.env = orig
			return err
		}
		c.writeln(fmt.Sprintf("if %s {", cond))
		c.indent++
	}
	for _, it := range u.Set.Items {
		if key, ok := c.simpleKey(it.Key); ok {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln(fmt.Sprintf("%s.%s = %s;", item, key, val))
		}
	}
	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}
	c.env = orig
	c.writeln(fmt.Sprintf("%s[%s] = %s;", mutList, idx, item))
	c.indent--
	c.writeln("}")
	if mutList != list {
		c.writeln(fmt.Sprintf("let %s = %s;", list, mutList))
	}
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		var b strings.Builder
		b.WriteString("#[derive(Debug, Clone)]\n")
		b.WriteString(fmt.Sprintf("enum %s {\n", td.Name))
		c.indent++
		for _, v := range td.Variants {
			if len(v.Fields) == 0 {
				b.WriteString(strings.Repeat("    ", c.indent))
				b.WriteString(fmt.Sprintf("%s,\n", v.Name))
				c.variantInfo[v.Name] = struct {
					Union  string
					Fields map[string]types.Type
					Order  []string
				}{Union: td.Name, Fields: map[string]types.Type{}, Order: []string{}}
				continue
			}
			fields := make([]string, len(v.Fields))
			flds := make(map[string]types.Type)
			order := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				typ := rustType(f.Type)
				if f.Type != nil && f.Type.Simple != nil && *f.Type.Simple == td.Name {
					typ = fmt.Sprintf("Box<%s>", td.Name)
				}
				fields[i] = fmt.Sprintf("%s: %s", f.Name, typ)
				flds[f.Name] = types.ResolveTypeRef(f.Type, c.env)
				order[i] = f.Name
			}
			b.WriteString(strings.Repeat("    ", c.indent))
			b.WriteString(fmt.Sprintf("%s { %s },\n", v.Name, strings.Join(fields, ", ")))
			c.variantInfo[v.Name] = struct {
				Union  string
				Fields map[string]types.Type
				Order  []string
			}{Union: td.Name, Fields: flds, Order: order}
			c.structs[v.Name] = types.StructType{Name: v.Name, Fields: flds, Order: order}
		}
		c.indent--
		b.WriteString("}\n")
		c.enumDecls = append(c.enumDecls, b.String())
		return nil
	}
	st := types.StructType{Name: td.Name, Fields: map[string]types.Type{}, Order: []string{}}
	var body strings.Builder
	eqHash := true
	c.indent++
	for _, m := range td.Members {
		if m.Field == nil {
			return fmt.Errorf("unsupported type member")
		}
		typ := rustType(m.Field.Type)
		ft := types.ResolveTypeRef(m.Field.Type, c.env)
		st.Fields[m.Field.Name] = ft
		st.Order = append(st.Order, m.Field.Name)
		if !isEqHashable(ft) {
			eqHash = false
		}
		body.WriteString(strings.Repeat("    ", c.indent))
		body.WriteString(fmt.Sprintf("%s: %s,\n", m.Field.Name, typ))
	}
	c.indent--
	derives := "#[derive(Default, Debug, Clone, PartialEq"
	if eqHash {
		derives += ", Eq, Hash"
	}
	derives += ")]\n"
	var b strings.Builder
	b.WriteString(derives)
	b.WriteString(fmt.Sprintf("struct %s {\n", td.Name))
	b.WriteString(body.String())
	b.WriteString("}\n")
	c.enumDecls = append(c.enumDecls, b.String())
	c.structs[td.Name] = st
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	mut := c.mutParams[f.Name]
	for i, p := range f.Params {
		typ := rustType(p.Type)
		if p.Type != nil && p.Type.Simple != nil {
			if _, ok := c.env.GetUnion(*p.Type.Simple); ok {
				typ = "&" + typ
			}
		}
		if mut != nil && mut[i] {
			params[i] = fmt.Sprintf("%s: &mut %s", p.Name, typ)
		} else {
			params[i] = fmt.Sprintf("%s: %s", p.Name, typ)
		}
	}
	origEnv := c.env
	childEnv := types.NewEnv(c.env)
	for _, p := range f.Params {
		childEnv.SetVar(p.Name, types.ResolveTypeRef(p.Type, c.env), true)
	}
	c.env = childEnv
	retTy := "()"
	if f.Return != nil {
		retTy = rustType(f.Return)
	}
	topLevel := c.inMain && c.indent == 1
	if topLevel {
		for g := range c.globals {
			for _, st := range f.Body {
				if usesIdentStmt(st, g) {
					topLevel = false
					break
				}
			}
			if !topLevel {
				break
			}
		}
	}
	if !topLevel {
		c.writeln(fmt.Sprintf("let %s = move |%s| -> %s {", f.Name, strings.Join(params, ", "), retTy))
	} else {
		c.writeln(fmt.Sprintf("fn %s(%s) -> %s {", f.Name, strings.Join(params, ", "), retTy))
	}
	prev := c.inMain
	c.inMain = false
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.indent--
	c.inMain = prev
	c.env = origEnv
	if !topLevel {
		c.writeln("};")
	} else {
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	v, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("assert!(%s);", v))
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileCond(e *parser.Expr) (string, error) {
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	if c.env != nil {
		t := types.TypeOfExpr(e, c.env)
		switch tt := t.(type) {
		case types.BoolType:
			return expr, nil
		case types.IntType, types.Int64Type:
			return fmt.Sprintf("%s != 0", expr), nil
		case types.FloatType:
			return fmt.Sprintf("%s != 0.0", expr), nil
		case types.StringType:
			return fmt.Sprintf("!%s.is_empty()", expr), nil
		case types.StructType:
			return fmt.Sprintf("%s != %s::default()", expr, tt.Name), nil
		}
	}
	return fmt.Sprintf("%s != Default::default()", expr), nil
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	leftAST := b.Left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "in":
			if c.env != nil {
				rt := types.TypeOfPostfix(op.Right, c.env)
				switch {
				case types.IsStringType(rt):
					if !c.unaryIsString(leftAST) {
						return "", fmt.Errorf("in type mismatch")
					}
					res = fmt.Sprintf("%s.contains(%s)", r, res)
				case types.IsListType(rt):
					res = fmt.Sprintf("%s.contains(&%s)", r, res)
				case types.IsMapType(rt):
					res = fmt.Sprintf("%s.contains_key(&%s)", r, res)
				default:
					switch {
					case c.postfixIsMap(op.Right):
						res = fmt.Sprintf("%s.contains_key(&%s)", r, res)
					case c.postfixIsList(op.Right):
						res = fmt.Sprintf("%s.contains(&%s)", r, res)
					case c.postfixIsString(op.Right):
						if !c.unaryIsString(leftAST) {
							return "", fmt.Errorf("in type mismatch")
						}
						res = fmt.Sprintf("%s.contains(%s)", r, res)
					default:
						return "", fmt.Errorf("in unsupported")
					}
				}
			} else {
				switch {
				case c.postfixIsMap(op.Right):
					res = fmt.Sprintf("%s.contains_key(&%s)", r, res)
				case c.postfixIsList(op.Right):
					res = fmt.Sprintf("%s.contains(&%s)", r, res)
				case c.postfixIsString(op.Right) && c.unaryIsString(leftAST):
					res = fmt.Sprintf("%s.contains(%s)", r, res)
				default:
					return "", fmt.Errorf("in unsupported")
				}
			}
		case "+":
			if c.unaryIsString(leftAST) || c.postfixIsString(op.Right) {
				res = fmt.Sprintf("format!(\"{}{}\", %s, %s)", res, r)
			} else {
				if c.env != nil {
					lt := types.TypeOfUnary(leftAST, c.env)
					rt := types.TypeOfPostfix(op.Right, c.env)
					if _, ok := lt.(types.FloatType); ok {
						if _, ok2 := rt.(types.IntType); ok2 {
							r = fmt.Sprintf("%s as f64", r)
						}
					} else if _, ok := rt.(types.FloatType); ok {
						if _, ok2 := lt.(types.IntType); ok2 {
							res = fmt.Sprintf("(%s as f64)", res)
						}
					}
				}
				res = fmt.Sprintf("%s + %s", res, r)
			}
		case "union":
			if op.All {
				c.helpers["_union_all"] = true
				res = fmt.Sprintf("_union_all(%s, %s)", res, r)
			} else {
				c.helpers["_union"] = true
				res = fmt.Sprintf("_union(%s, %s)", res, r)
			}
		case "except":
			c.helpers["_except"] = true
			res = fmt.Sprintf("_except(%s, %s)", res, r)
		case "intersect":
			c.helpers["_intersect"] = true
			res = fmt.Sprintf("_intersect(%s, %s)", res, r)
		default:
			if c.env != nil {
				lt := types.TypeOfUnary(leftAST, c.env)
				rt := types.TypeOfPostfix(op.Right, c.env)
				isInt := func(t types.Type) bool {
					switch t.(type) {
					case types.IntType, types.Int64Type:
						return true
					}
					return false
				}
				if op.Op == "/" && isInt(lt) && isInt(rt) {
					res = fmt.Sprintf("(%s as f64) / (%s as f64)", res, r)
					leftAST = &parser.Unary{Value: op.Right}
					continue
				}
				if op.Op == "*" || op.Op == "/" {
					if _, ok := lt.(types.FloatType); ok {
						if isInt(rt) {
							r = fmt.Sprintf("%s as f64", r)
						}
					} else if _, ok := rt.(types.FloatType); ok {
						if isInt(lt) {
							res = fmt.Sprintf("(%s as f64)", res)
						}
					}
				}
			}
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
		}
		leftAST = &parser.Unary{Value: op.Right}
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for opIndex, op := range p.Ops {
		switch {
		case op.Cast != nil:
			rustTy := rustType(op.Cast.Type)
			if p.Target.Map != nil && opIndex == 0 && len(p.Ops) == 1 && op.Cast.Type.Simple != nil {
				lit, err := c.compileStructLiteralFromMap(*op.Cast.Type.Simple, p.Target.Map)
				if err == nil {
					val = lit
					break
				}
			}
			switch rustTy {
			case "i32":
				val = fmt.Sprintf("%s.parse::<i32>().unwrap()", val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", rustTy)
			}
		case op.Index != nil:
			prefix := &parser.Unary{Value: &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:opIndex]}}
			t := types.TypeOfPostfixBasic(prefix, c.env)
			if op.Index.Colon != nil || op.Index.End != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				} else {
					end = fmt.Sprintf("%s.len()", val)
				}
				if types.IsStringType(t) {
					val = fmt.Sprintf("&%s[%s as usize..%s as usize]", val, start, end)
				} else {
					val = fmt.Sprintf("%s[%s as usize..%s as usize].to_vec()", val, start, end)
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("missing index")
				}
				idxVal, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if types.IsMapType(t) {
					val = fmt.Sprintf("%s[&%s]", val, idxVal)
				} else if types.IsStringType(t) {
					val = fmt.Sprintf("%s.chars().nth(%s as usize).unwrap()", val, idxVal)
				} else {
					val = fmt.Sprintf("%s[%s as usize]", val, idxVal)
				}
			}
		case op.Field != nil:
			prefix := &parser.Unary{Value: &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:opIndex]}}
			t := types.TypeOfPostfixBasic(prefix, c.env)
			if types.IsMapType(t) {
				val = fmt.Sprintf("%s[\"%s\"]", val, op.Field.Name)
			} else {
				val = fmt.Sprintf("%s.%s", val, op.Field.Name)
			}
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "vec![" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			if info, ok := c.variantInfo[p.Selector.Root]; ok {
				return fmt.Sprintf("%s::%s", info.Union, p.Selector.Root), nil
			}
			return loopVal(p.Selector.Root, c.env), nil
		}
		return p.Selector.Root + "." + strings.Join(p.Selector.Tail, "."), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileCond(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	fmt.Fprintf(&b, "if %s { %s }", cond, thenExpr)
	if ie.ElseIf != nil {
		elseStr, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		b.WriteString(" else ")
		b.WriteString(elseStr)
	} else if ie.Else != nil {
		elseExpr, err := c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&b, " else { %s }", elseExpr)
	}
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(me *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(me.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("match ")
	b.WriteString(target)
	b.WriteString(" {")
	for i, cs := range me.Cases {
		pat, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if i > 0 {
			b.WriteString(" ")
		}
		fmt.Fprintf(&b, "%s => %s,", pat, res)
	}
	b.WriteString(" }")
	return b.String(), nil
}

func (c *Compiler) compileMatchPattern(e *parser.Expr) (string, error) {
	if name, ok := c.simpleIdent(e); ok {
		if name == "_" {
			return "_", nil
		}
		if info, ok := c.variantInfo[name]; ok {
			return fmt.Sprintf("%s::%s", info.Union, name), nil
		}
		return name, nil
	}
	// handle variant pattern Node(a,b)
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if u != nil && len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil {
			call := u.Value.Target.Call
			if info, ok := c.variantInfo[call.Func]; ok {
				if len(call.Args) != len(info.Order) {
					return "", fmt.Errorf("pattern arg mismatch")
				}
				parts := make([]string, len(call.Args))
				for i, a := range call.Args {
					id, ok := c.simpleIdent(a)
					if !ok {
						return "", fmt.Errorf("complex pattern not supported")
					}
					parts[i] = fmt.Sprintf("%s: %s", info.Order[i], id)
				}
				return fmt.Sprintf("%s::%s { %s }", info.Union, call.Func, strings.Join(parts, ", ")), nil
			}
		}
	}
	return c.compileExpr(e)
}

func (c *Compiler) unaryIsString(u *parser.Unary) bool {
	if c.env == nil {
		return false
	}
	t := types.TypeOfUnary(u, c.env)
	return types.IsStringType(t)
}

func (c *Compiler) postfixIsString(pf *parser.PostfixExpr) bool {
	if c.env != nil {
		t := types.TypeOfPostfix(pf, c.env)
		if types.IsStringType(t) {
			return true
		}
	}
	if id, ok := c.simplePostfixIdent(pf); ok {
		if name, ok2 := c.listVars[id]; ok2 && name == "string" {
			return true
		}
	}
	return false
}

func (c *Compiler) unaryIsList(u *parser.Unary) bool {
	if c.env == nil {
		return false
	}
	_, ok := types.TypeOfUnary(u, c.env).(types.ListType)
	return ok
}

func (c *Compiler) postfixIsList(pf *parser.PostfixExpr) bool {
	if c.env != nil {
		if _, ok := types.TypeOfPostfix(pf, c.env).(types.ListType); ok {
			return true
		}
	}
	if id, ok := c.simplePostfixIdent(pf); ok {
		if _, ok2 := c.listVars[id]; ok2 {
			return true
		}
	}
	return false
}

func (c *Compiler) unaryIsMap(u *parser.Unary) bool {
	if c.env == nil {
		return false
	}
	_, ok := types.TypeOfUnary(u, c.env).(types.MapType)
	return ok
}

func (c *Compiler) postfixIsMap(pf *parser.PostfixExpr) bool {
	if c.env != nil {
		if _, ok := types.TypeOfPostfix(pf, c.env).(types.MapType); ok {
			return true
		}
	}
	if id, ok := c.simplePostfixIdent(pf); ok {
		if _, ok2 := c.structs[id]; ok2 {
			return true
		}
	}
	return false
}

func (c *Compiler) simplePostfixIdent(pf *parser.PostfixExpr) (string, bool) {
	if pf == nil || len(pf.Ops) > 0 || pf.Target == nil || pf.Target.Selector == nil || len(pf.Target.Selector.Tail) > 0 {
		return "", false
	}
	return pf.Target.Selector.Root, true
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	origEnv := c.env
	childEnv := types.NewEnv(c.env)
	for i, p := range fn.Params {
		typ := rustType(p.Type)
		if p.Type != nil && p.Type.Simple != nil {
			if _, ok := c.env.GetUnion(*p.Type.Simple); ok {
				typ = "&" + typ
			}
		}
		params[i] = fmt.Sprintf("%s: %s", p.Name, typ)
		childEnv.SetVar(p.Name, types.ResolveTypeRef(p.Type, c.env), true)
	}
	c.env = childEnv
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		c.env = origEnv
		return fmt.Sprintf("Box::new(move |%s| %s)", strings.Join(params, ", "), body), nil
	}
	c.env = origEnv
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileGroupBySimple(q *parser.QueryExpr, src string, child *types.Env, fromSrcs []string) (string, error) {
	orig := c.env
	c.env = child
	var cond string
	var err error
	if q.Where != nil {
		cond, err = c.compileCond(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if len(q.Group.Exprs) != 1 {
		c.env = orig
		return "", fmt.Errorf("multi-key group not supported")
	}
	var keyExpr string
	var keyType types.Type
	if ml := tryMapLiteral(q.Group.Exprs[0]); ml != nil {
		name := c.newStructName("Key")
		keyExpr, err = c.compileMapLiteralAsStruct(name, ml)
		if err != nil {
			c.env = orig
			return "", err
		}
		if st, ok := c.structs[name]; ok {
			keyType = st
		} else {
			keyType = types.StructType{Name: name}
		}

	} else {
		keyExpr, err = c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		keyType = c.fieldType(q.Group.Exprs[0])
	}
	elemType, _ := child.GetVar(q.Var)

	groupStruct := c.newStructName("Group")
	st := types.StructType{Name: groupStruct, Fields: map[string]types.Type{"key": keyType, "items": types.ListType{Elem: elemType}}, Order: []string{"key", "items"}}
	c.structs[groupStruct] = st
	c.genStructs = append(c.genStructs, st)

	eqHash := isEqHashable(keyType)
	mapTmp := c.newTmp()
	groupVec := c.newTmp()

	var b strings.Builder
	if eqHash {
		b.WriteString("{ let mut ")
		b.WriteString(mapTmp)
		b.WriteString(" = std::collections::HashMap::new();")
	} else {
		b.WriteString("{ let mut ")
		b.WriteString(groupVec)
		b.WriteString(fmt.Sprintf(" = Vec::<%s>::new();", groupStruct))
	}
	b.WriteString(loopHead(q.Var, src, child))
	for i, fs := range fromSrcs {
		fmt.Fprintf(&b, " %s", loopHead(q.Froms[i].Var, fs, child))
	}
	if cond != "" {
		b.WriteString(" if !(" + cond + ") { continue; }")
	}
	fmt.Fprintf(&b, " let key = %s;", keyExpr)
	if eqHash {
		fmt.Fprintf(&b, " %s.entry(key).or_insert_with(Vec::new).push(%s);", mapTmp, loopVal(q.Var, child))
	} else {
		tmpg := c.newTmp()
		fmt.Fprintf(&b, " if let Some(%s) = %s.iter_mut().find(|g| g.key == key) { %s.items.push(%s); } else { %s.push(%s { key: key, items: vec![%s] }); }", tmpg, groupVec, tmpg, loopVal(q.Var, child), groupVec, groupStruct, loopVal(q.Var, child))
	}
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	if eqHash {
		b.WriteString(" let mut ")
		b.WriteString(groupVec)
		b.WriteString(fmt.Sprintf(" = Vec::<%s>::new();", groupStruct))
		b.WriteString(fmt.Sprintf(" for (k,v) in %s { %s.push(%s { key: k, items: v }); }", mapTmp, groupVec, groupStruct))
	}

	groupVar := q.Group.Name
	groupEnv := types.NewEnv(orig)
	groupEnv.SetVar(groupVar, st, true)
	c.listVars[groupVar] = groupStruct
	c.pushGroupVar(groupVar)
	c.env = groupEnv

	if q.Group.Having != nil {
		haveExpr, err := c.compileCond(q.Group.Having)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
		tmp2 := c.newTmp()
		fmt.Fprintf(&b, " let mut %s = Vec::new();", tmp2)
		fmt.Fprintf(&b, " for g in %s.into_iter() { if %s { %s.push(g); } }", groupVec, haveExpr, tmp2)
		groupVec = tmp2
	}

	var sortExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileSortKey(q.Sort)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
		sa := fmt.Sprintf("(%s)", strings.ReplaceAll(sortExpr, groupVar, "a"))
		sb := fmt.Sprintf("(%s)", strings.ReplaceAll(sortExpr, groupVar, "b"))
		fmt.Fprintf(&b, " %s.sort_by(|a,b| %s.partial_cmp(&%s).unwrap());", groupVec, sa, sb)
	}

	var skipExpr, takeExpr string
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
	}

	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
	} else {
		sel, err = c.compileExpr(q.Select)
		if id, ok := c.simpleIdent(q.Select); ok && id == groupVar {
			c.lastListStruct = groupStruct
		}
	}
	if err != nil {
		c.popGroupVar()
		c.env = orig
		return "", err
	}

	b.WriteString(" let mut result = Vec::new();")
	b.WriteString(fmt.Sprintf(" for %s in %s {", groupVar, groupVec))
	b.WriteString(" result.push(" + sel + ");")
	b.WriteString(" }")

	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = "result.len()"
		}
		b.WriteString(" result = result[" + start + ".." + end + "].to_vec();")
	}

	c.popGroupVar()
	c.env = orig
	b.WriteString(" result }")
	return b.String(), nil
}

func (c *Compiler) compileGroupByJoin(q *parser.QueryExpr, src string, child *types.Env, fromSrcs, joinSrcs, joinConds, joinSides []string) (string, error) {
	orig := c.env
	c.env = child
	var cond string
	var err error
	if q.Where != nil {
		cond, err = c.compileCond(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if len(q.Group.Exprs) != 1 {
		c.env = orig
		return "", fmt.Errorf("multi-key group not supported")
	}
	var keyExpr string
	var keyType types.Type
	if ml := tryMapLiteral(q.Group.Exprs[0]); ml != nil {
		name := c.newStructName("Key")
		keyExpr, err = c.compileMapLiteralAsStruct(name, ml)
		if err != nil {
			c.env = orig
			return "", err
		}
		if st, ok := c.structs[name]; ok {
			keyType = st
		} else {
			keyType = types.StructType{Name: name}
		}
	} else {
		keyExpr, err = c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		keyType = c.fieldType(q.Group.Exprs[0])
	}

	itemStruct := c.newStructName("Item")
	stItem := types.StructType{Name: itemStruct, Fields: map[string]types.Type{}, Order: []string{}}
	names := []string{q.Var}
	for _, f := range q.Froms {
		names = append(names, f.Var)
	}
	for _, j := range q.Joins {
		names = append(names, j.Var)
	}
	for _, n := range names {
		if t, err := child.GetVar(n); err == nil {
			stItem.Fields[n] = t
			stItem.Order = append(stItem.Order, n)
		}
	}
	c.structs[itemStruct] = stItem
	c.genStructs = append(c.genStructs, stItem)

	groupStruct := c.newStructName("Group")
	st := types.StructType{Name: groupStruct, Fields: map[string]types.Type{"key": keyType, "items": types.ListType{Elem: stItem}}, Order: []string{"key", "items"}}
	c.structs[groupStruct] = st
	c.genStructs = append(c.genStructs, st)

	eqHash := isEqHashable(keyType)
	mapTmp := c.newTmp()
	groupVec := c.newTmp()

	var b strings.Builder
	if eqHash {
		b.WriteString("{ let mut ")
		b.WriteString(mapTmp)
		b.WriteString(" = std::collections::HashMap::new();")
	} else {
		b.WriteString("{ let mut ")
		b.WriteString(groupVec)
		b.WriteString(fmt.Sprintf(" = Vec::<%s>::new();", groupStruct))
	}
	b.WriteString(loopHead(q.Var, src, child))
	for i, fs := range fromSrcs {
		fmt.Fprintf(&b, " %s", loopHead(q.Froms[i].Var, fs, child))
	}
	singleLeft := len(joinSides) == 1 && joinSides[0] == "left"
	if singleLeft {
		matched := c.newTmp()
		b.WriteString(" let mut " + matched + " = false;")
		b.WriteString(" ")
		b.WriteString(loopHead(q.Joins[0].Var, joinSrcs[0], child))
		if joinConds[0] != "" {
			b.WriteString(" if !(" + joinConds[0] + ") { continue; }")
		}
		if cond != "" {
			b.WriteString(" if !(" + cond + ") { continue; }")
		}
		b.WriteString(" " + matched + " = true;")
		b.WriteString(" let key = " + keyExpr + ";")
		if eqHash {
			b.WriteString(" " + mapTmp + ".entry(key).or_insert_with(Vec::new).push(" + itemStruct + " {")
		} else {
			tmpg := c.newTmp()
			b.WriteString(fmt.Sprintf(" if let Some(%s) = %s.iter_mut().find(|g| g.key == key) { %s.items.push(%s {", tmpg, groupVec, tmpg, itemStruct))
		}
		for i, n := range names {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(n + ": " + loopVal(n, child))
		}
		if eqHash {
			b.WriteString(" });")
		} else {
			b.WriteString(" }); } else { " + groupVec + ".push(" + itemStruct + " {")
			for i, n := range names {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(n + ": " + loopVal(n, child))
			}
			b.WriteString(" }); }")
		}
		b.WriteString(" }")
		b.WriteString(" if !" + matched + " {")
		b.WriteString(" ")
		b.WriteString(defaultDecl(q.Joins[0].Var, child))
		b.WriteString(" let key = " + keyExpr + ";")
		if eqHash {
			b.WriteString(" " + mapTmp + ".entry(key).or_insert_with(Vec::new).push(" + itemStruct + " {")
			for i, n := range names {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(n + ": " + loopVal(n, child))
			}
			b.WriteString(" });")
		} else {
			tmpg := c.newTmp()
			b.WriteString(fmt.Sprintf(" if let Some(%s) = %s.iter_mut().find(|g| g.key == key) { %s.items.push(%s {", tmpg, groupVec, tmpg, itemStruct))
			for i, n := range names {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(n + ": " + loopVal(n, child))
			}
			b.WriteString(" }); } else { " + groupVec + ".push(" + itemStruct + " {")
			for i, n := range names {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(n + ": " + loopVal(n, child))
			}
			b.WriteString(" }); }")
		}
		b.WriteString(" }")
	} else {
		for i, js := range joinSrcs {
			b.WriteString(" ")
			b.WriteString(loopHead(q.Joins[i].Var, js, child))
			if joinConds[i] != "" {
				b.WriteString(" if !(" + joinConds[i] + ") { continue; }")
			}
		}
		if cond != "" {
			b.WriteString(" if !(" + cond + ") { continue; }")
		}
		b.WriteString(" let key = " + keyExpr + ";")
		if eqHash {
			b.WriteString(" " + mapTmp + ".entry(key).or_insert_with(Vec::new).push(" + itemStruct + " {")
		} else {
			tmpg := c.newTmp()
			b.WriteString(fmt.Sprintf(" if let Some(%s) = %s.iter_mut().find(|g| g.key == key) { %s.items.push(%s {", tmpg, groupVec, tmpg, itemStruct))
		}
		for i, n := range names {
			if i > 0 {
				b.WriteString(", ")
			}
			b.WriteString(n + ": " + loopVal(n, child))
		}
		if eqHash {
			b.WriteString(" });")
		} else {
			b.WriteString(" }); } else { " + groupVec + ".push(" + itemStruct + " {")
			for i, n := range names {
				if i > 0 {
					b.WriteString(", ")
				}
				b.WriteString(n + ": " + loopVal(n, child))
			}
			b.WriteString(" }); }")
		}
		for range joinSrcs {
			b.WriteString(" }")
		}
	}
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	if eqHash {
		b.WriteString(" let mut ")
		b.WriteString(groupVec)
		b.WriteString(fmt.Sprintf(" = Vec::<%s>::new();", groupStruct))
		b.WriteString(fmt.Sprintf(" for (k,v) in %s { %s.push(%s { key: k, items: v }); }", mapTmp, groupVec, groupStruct))
	}

	groupVar := q.Group.Name
	groupEnv := types.NewEnv(orig)
	groupEnv.SetVar(groupVar, st, true)
	c.listVars[groupVar] = groupStruct
	c.pushGroupVar(groupVar)
	c.env = groupEnv

	if q.Group.Having != nil {
		haveExpr, err := c.compileCond(q.Group.Having)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
		tmp2 := c.newTmp()
		fmt.Fprintf(&b, " let mut %s = Vec::new();", tmp2)
		fmt.Fprintf(&b, " for g in %s.into_iter() { if %s { %s.push(g); } }", groupVec, haveExpr, tmp2)
		groupVec = tmp2
	}

	var sortExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileSortKey(q.Sort)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
		sa := fmt.Sprintf("(%s)", strings.ReplaceAll(sortExpr, groupVar, "a"))
		sb := fmt.Sprintf("(%s)", strings.ReplaceAll(sortExpr, groupVar, "b"))
		fmt.Fprintf(&b, " %s.sort_by(|a,b| %s.partial_cmp(&%s).unwrap());", groupVec, sa, sb)
	}

	var skipExpr, takeExpr string
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.popGroupVar()
			c.env = orig
			return "", err
		}
	}

	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
	} else {
		sel, err = c.compileExpr(q.Select)
		if id, ok := c.simpleIdent(q.Select); ok && id == groupVar {
			c.lastListStruct = groupStruct
		}
	}
	if err != nil {
		c.popGroupVar()
		c.env = orig
		return "", err
	}

	b.WriteString(" let mut result = Vec::new();")
	b.WriteString(fmt.Sprintf(" for %s in %s {", groupVar, groupVec))
	b.WriteString(" result.push(" + sel + ");")
	b.WriteString(" }")

	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = "result.len()"
		}
		b.WriteString(" result = result[" + start + ".." + end + "].to_vec();")
	}

	c.popGroupVar()
	c.env = orig
	b.WriteString(" result }")
	return b.String(), nil
}

func (c *Compiler) compileLeftJoinSimple(q *parser.QueryExpr, src string, child *types.Env, fromSrcs []string, joinSrc, joinCond string) (string, error) {
	orig := c.env
	c.env = child
	var cond, sortExpr, skipExpr, takeExpr string
	var err error
	if q.Where != nil {
		cond, err = c.compileCond(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileSortKey(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
	} else {
		sel, err = c.compileExpr(q.Select)
	}
	if err != nil {
		c.env = orig
		return "", err
	}

	tmp := c.newTmp()
	var b strings.Builder
	b.WriteString("{ let mut ")
	b.WriteString(tmp)
	b.WriteString(" = Vec::new();")
	b.WriteString(loopHead(q.Var, src, child))
	for i, fs := range fromSrcs {
		b.WriteString(" ")
		b.WriteString(loopHead(q.Froms[i].Var, fs, child))
	}
	b.WriteString(" let mut _matched = false;")
	b.WriteString(" ")
	b.WriteString(loopHead(q.Joins[0].Var, joinSrc, child))
	if joinCond != "" {
		b.WriteString(" if !(" + joinCond + ") { continue; }")
	}
	if cond != "" {
		b.WriteString(" if !(" + cond + ") { continue; }")
	}
	b.WriteString(" _matched = true;")
	if sortExpr != "" {
		item := c.newTmp()
		key := c.newTmp()
		b.WriteString(" let " + item + " = " + sel + ";")
		b.WriteString(" let " + key + " = " + sortExpr + ";")
		b.WriteString(" " + tmp + ".push((" + key + ", " + item + "));")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	b.WriteString(" }")
	b.WriteString(" if !_matched {")
	b.WriteString(" ")
	b.WriteString(defaultDecl(q.Joins[0].Var, child))
	if cond != "" {
		b.WriteString(" if (" + cond + ") { " + tmp + ".push(" + sel + "); }")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	b.WriteString(" }")
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	if sortExpr != "" {
		b.WriteString(" " + tmp + ".sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());")
		res := c.newTmp()
		b.WriteString(" let mut " + res + " = Vec::new();")
		b.WriteString(" for p in " + tmp + " { " + res + ".push(p.1); }")
		tmp = res
	}
	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = tmp + ".len()"
		}
		b.WriteString(" let " + tmp + " = " + tmp + "[" + start + ".." + end + "].to_vec();")
	}
	if q.Distinct {
		uniq := c.newTmp()
		b.WriteString(" let mut set = std::collections::HashSet::new();")
		b.WriteString(" let mut " + uniq + " = Vec::new();")
		b.WriteString(" for v in " + tmp + ".into_iter() { if set.insert(v.clone()) { " + uniq + ".push(v); } }")
		b.WriteString(" let " + tmp + " = " + uniq + ";")
	}
	b.WriteString(" " + tmp + " }")
	c.env = orig
	return b.String(), nil
}

func (c *Compiler) compileRightJoinSimple(q *parser.QueryExpr, src string, child *types.Env, fromSrcs []string, joinSrc, joinCond string) (string, error) {
	orig := c.env
	c.env = child
	var cond, sortExpr, skipExpr, takeExpr string
	var err error
	if q.Where != nil {
		cond, err = c.compileCond(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileSortKey(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	origVarType, _ := child.GetVar(q.Var)
	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
	} else {
		sel, err = c.compileExpr(q.Select)
	}
	if err != nil {
		c.env = orig
		return "", err
	}

	tmp := c.newTmp()
	var b strings.Builder
	b.WriteString("{ let mut ")
	b.WriteString(tmp)
	b.WriteString(" = Vec::new();")
	b.WriteString(loopHead(q.Joins[0].Var, joinSrc, child))
	b.WriteString(" let mut _matched = false;")
	b.WriteString(" ")
	b.WriteString(loopHead(q.Var, src, child))
	for i, fs := range fromSrcs {
		b.WriteString(" ")
		b.WriteString(loopHead(q.Froms[i].Var, fs, child))
	}
	if joinCond != "" {
		b.WriteString(" if !(" + joinCond + ") { continue; }")
	}
	if cond != "" {
		b.WriteString(" if !(" + cond + ") { continue; }")
	}
	b.WriteString(" _matched = true;")
	if sortExpr != "" {
		item := c.newTmp()
		key := c.newTmp()
		b.WriteString(" let " + item + " = " + sel + ";")
		b.WriteString(" let " + key + " = " + sortExpr + ";")
		b.WriteString(" " + tmp + ".push((" + key + ", " + item + "));")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	b.WriteString(" if !_matched {")
	b.WriteString(" ")
	b.WriteString(defaultDeclType(q.Var, origVarType))
	if cond != "" {
		b.WriteString(" if (" + cond + ") { " + tmp + ".push(" + sel + "); }")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	b.WriteString(" }")
	b.WriteString(" }")
	if sortExpr != "" {
		b.WriteString(" " + tmp + ".sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());")
		res := c.newTmp()
		b.WriteString(" let mut " + res + " = Vec::new();")
		b.WriteString(" for p in " + tmp + " { " + res + ".push(p.1); }")
		tmp = res
	}
	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = tmp + ".len()"
		}
		b.WriteString(" let " + tmp + " = " + tmp + "[" + start + ".." + end + "].to_vec();")
	}
	if q.Distinct {
		uniq := c.newTmp()
		b.WriteString(" let mut set = std::collections::HashSet::new();")
		b.WriteString(" let mut " + uniq + " = Vec::new();")
		b.WriteString(" for v in " + tmp + ".into_iter() { if set.insert(v.clone()) { " + uniq + ".push(v); } }")
		b.WriteString(" let " + tmp + " = " + uniq + ";")
	}
	b.WriteString(" " + tmp + " }")
	c.env = orig
	return b.String(), nil
}

func (c *Compiler) compileOuterJoinSimple(q *parser.QueryExpr, src string, child *types.Env, joinSrc, joinCond string) (string, error) {
	orig := c.env
	c.env = child
	var err error
	origVarType, _ := child.GetVar(q.Var)
	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
	} else {
		sel, err = c.compileExpr(q.Select)
	}
	if err != nil {
		c.env = orig
		return "", err
	}

	tmp := c.newTmp()
	var b strings.Builder
	b.WriteString("{ let mut ")
	b.WriteString(tmp)
	b.WriteString(" = Vec::new();")

	b.WriteString(loopHead(q.Var, src, child))
	b.WriteString(" let mut _matched = false;")
	b.WriteString(" ")
	b.WriteString(loopHead(q.Joins[0].Var, joinSrc, child))
	if joinCond != "" {
		b.WriteString(" if !(" + joinCond + ") { continue; }")
	}
	b.WriteString(" _matched = true;")
	b.WriteString(" " + tmp + ".push(" + sel + ");")
	b.WriteString(" }")
	b.WriteString(" if !_matched {")
	b.WriteString(" ")
	b.WriteString(defaultDecl(q.Joins[0].Var, child))
	b.WriteString(" " + tmp + ".push(" + sel + ");")
	b.WriteString(" }")
	b.WriteString(" }")

	b.WriteString(" for ")
	b.WriteString(q.Joins[0].Var)
	b.WriteString(" in &")
	b.WriteString(joinSrc)
	b.WriteString(" {")
	b.WriteString(" let mut _matched = false;")
	b.WriteString(" ")
	b.WriteString(loopHead(q.Var, src, child))
	if joinCond != "" {
		b.WriteString(" if " + joinCond + " { _matched = true; break; }")
	} else {
		b.WriteString(" { _matched = true; break; }")
	}
	b.WriteString(" }")
	b.WriteString(" if !_matched {")
	b.WriteString(" ")
	b.WriteString(defaultDeclType(q.Var, origVarType))
	b.WriteString(" " + tmp + ".push(" + sel + ");")
	b.WriteString(" }")
	b.WriteString(" }")

	b.WriteString(" " + tmp + " }")
	c.env = orig
	return b.String(), nil
}

func (c *Compiler) compileLeftJoinLast(q *parser.QueryExpr, src string, child *types.Env, fromSrcs, joinSrcs, joinConds []string) (string, error) {
	orig := c.env
	c.env = child
	var cond, sortExpr, skipExpr, takeExpr string
	var err error
	if q.Where != nil {
		cond, err = c.compileCond(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileSortKey(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
		if err != nil {
			// continue with error
		}
	} else {
		sel, err = c.compileExpr(q.Select)
	}
	if err != nil {
		c.env = orig
		return "", err
	}
	last := len(joinSrcs) - 1
	tmp := c.newTmp()
	var b strings.Builder
	b.WriteString("{ let mut ")
	b.WriteString(tmp)
	b.WriteString(" = Vec::new();")
	b.WriteString(loopHead(q.Var, src, child))
	for i, fs := range fromSrcs {
		b.WriteString(" ")
		b.WriteString(loopHead(q.Froms[i].Var, fs, child))
	}
	for i := 0; i < last; i++ {
		b.WriteString(" ")
		b.WriteString(loopHead(q.Joins[i].Var, joinSrcs[i], child))
		if joinConds[i] != "" {
			b.WriteString(" if !(" + joinConds[i] + ") { continue; }")
		}
	}
	b.WriteString(" let mut _matched = false;")
	b.WriteString(" ")
	b.WriteString(loopHead(q.Joins[last].Var, joinSrcs[last], child))
	if joinConds[last] != "" {
		b.WriteString(" if !(" + joinConds[last] + ") { continue; }")
	}
	if cond != "" {
		b.WriteString(" if !(" + cond + ") { continue; }")
	}
	b.WriteString(" _matched = true;")
	if sortExpr != "" {
		item := c.newTmp()
		key := c.newTmp()
		b.WriteString(" let " + item + " = " + sel + ";")
		b.WriteString(" let " + key + " = " + sortExpr + ";")
		b.WriteString(" " + tmp + ".push((" + key + ", " + item + "));")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	b.WriteString(" }")
	b.WriteString(" if !_matched {")
	b.WriteString(" ")
	b.WriteString(defaultDecl(q.Joins[last].Var, child))
	if cond != "" {
		b.WriteString(" if (" + cond + ") { " + tmp + ".push(" + sel + "); }")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	b.WriteString(" }")
	for i := 0; i < last; i++ {
		b.WriteString(" }")
	}
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	if sortExpr != "" {
		b.WriteString(" " + tmp + ".sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());")
		res := c.newTmp()
		b.WriteString(" let mut " + res + " = Vec::new();")
		b.WriteString(" for p in " + tmp + " { " + res + ".push(p.1); }")
		tmp = res
	}
	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = tmp + ".len()"
		}
		b.WriteString(" let " + tmp + " = " + tmp + "[" + start + ".." + end + "].to_vec();")
	}
	if q.Distinct {
		uniq := c.newTmp()
		b.WriteString(" let mut set = std::collections::HashSet::new();")
		b.WriteString(" let mut " + uniq + " = Vec::new();")
		b.WriteString(" for v in " + tmp + ".into_iter() { if set.insert(v.clone()) { " + uniq + ".push(v); } }")
		b.WriteString(" let " + tmp + " = " + uniq + ";")
	}
	b.WriteString(" " + tmp + " }")
	c.env = orig
	return b.String(), nil
}
func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	origEnv := c.env
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	child := types.NewEnv(c.env)
	if id, ok := c.simpleIdent(q.Source); ok && c.isGroupVar(id) {
		src += ".items"
		if name, ok2 := c.listVars[id]; ok2 {
			if st, ok3 := c.structs[name]; ok3 {
				if t, ok4 := st.Fields["items"]; ok4 {
					if lt, ok5 := t.(types.ListType); ok5 {
						if stElem, ok6 := lt.Elem.(types.StructType); ok6 {
							child.SetVar(q.Var, stElem, true)
							c.listVars[q.Var] = stElem.Name
						} else {
							child.SetVar(q.Var, lt.Elem, true)
						}
					}
				}
			}
		}
	}
	if id, ok := c.simpleIdent(q.Source); ok {
		if name, ok2 := c.listVars[id]; ok2 {
			if st, ok3 := c.structs[name]; ok3 {
				child.SetVar(q.Var, st, true)
				c.listVars[q.Var] = name
			}
		}
	}
	if _, err := child.GetVar(q.Var); err != nil {
		if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
			child.SetVar(q.Var, lt.Elem, true)
		} else {
			child.SetVar(q.Var, types.AnyType{}, true)
		}
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = s
		if id, ok := c.simpleIdent(f.Src); ok {
			if name, ok2 := c.listVars[id]; ok2 {
				if st, ok3 := c.structs[name]; ok3 {
					child.SetVar(f.Var, st, true)
					c.listVars[f.Var] = name
				}
			}
		}
		if _, err := child.GetVar(f.Var); err != nil {
			if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
				child.SetVar(f.Var, lt.Elem, true)
			} else {
				child.SetVar(f.Var, types.AnyType{}, true)
			}
		}
	}
	joinSrcs := make([]string, len(q.Joins))
	joinConds := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		if id, ok := c.simpleIdent(j.Src); ok {
			if name, ok2 := c.listVars[id]; ok2 {
				if st, ok3 := c.structs[name]; ok3 {
					child.SetVar(j.Var, st, true)
					c.listVars[j.Var] = name
				}
			}
		}
		if _, err := child.GetVar(j.Var); err != nil {
			if lt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.ListType); ok {
				child.SetVar(j.Var, lt.Elem, true)
			} else {
				child.SetVar(j.Var, types.AnyType{}, true)
			}
		}
		if j.On != nil {
			c.env = child
			cond, err := c.compileCond(j.On)
			c.env = origEnv
			if err != nil {
				return "", err
			}
			joinConds[i] = cond
		}
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}
	if len(q.Joins) == 1 && joinSides[0] == "left" && q.Group == nil {
		res, err := c.compileLeftJoinSimple(q, src, child, fromSrcs, joinSrcs[0], joinConds[0])
		c.env = origEnv
		return res, err
	}
	if len(q.Joins) == 1 && joinSides[0] == "right" && q.Group == nil {
		res, err := c.compileRightJoinSimple(q, src, child, fromSrcs, joinSrcs[0], joinConds[0])
		c.env = origEnv
		return res, err
	}
	if len(q.Joins) == 1 && joinSides[0] == "outer" && len(q.Froms) == 0 && q.Group == nil {
		res, err := c.compileOuterJoinSimple(q, src, child, joinSrcs[0], joinConds[0])
		c.env = origEnv
		return res, err
	}
	if len(q.Joins) > 1 && joinSides[len(joinSides)-1] == "left" {
		ok := true
		for _, s := range joinSides[:len(joinSides)-1] {
			if s != "" {
				ok = false
				break
			}
		}
		if ok {
			res, err := c.compileLeftJoinLast(q, src, child, fromSrcs, joinSrcs, joinConds)
			c.env = origEnv
			return res, err
		}
	}
	for _, s := range joinSides {
		if s != "" && q.Group == nil {
			return "", fmt.Errorf("join sides not supported")
		}
	}
	if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		if name, arg, ok := c.aggCall(q.Select); ok && name == "sum" {
			orig := c.env
			c.env = child
			val, err := c.compileExpr(arg)
			if err != nil {
				c.env = orig
				return "", err
			}
			var cond string
			if q.Where != nil {
				cond, err = c.compileCond(q.Where)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			c.env = orig
			acc := c.newTmp()
			var b strings.Builder
			fmt.Fprintf(&b, "{ let mut %s = 0;", acc)
			b.WriteString(loopHead(q.Var, src, child))
			for i, fs := range fromSrcs {
				b.WriteString(" ")
				b.WriteString(loopHead(q.Froms[i].Var, fs, child))
			}
			for i, js := range joinSrcs {
				b.WriteString(" ")
				b.WriteString(loopHead(q.Joins[i].Var, js, child))
				if joinConds[i] != "" {
					b.WriteString(" if !(" + joinConds[i] + ") { continue; }")
				}
			}
			if cond != "" {
				b.WriteString(" if !(" + cond + ") { continue; }")
			}
			fmt.Fprintf(&b, " %s += %s;", acc, val)
			for range joinSrcs {
				b.WriteString(" }")
			}
			for range fromSrcs {
				b.WriteString(" }")
			}
			b.WriteString(" }")
			b.WriteString(" ")
			b.WriteString(acc)
			b.WriteString(" }")
			c.env = origEnv
			return b.String(), nil
		}
	}
	if q.Group != nil {
		if len(q.Joins) > 0 {
			res, err := c.compileGroupByJoin(q, src, child, fromSrcs, joinSrcs, joinConds, joinSides)
			c.env = origEnv
			return res, err
		}
		res, err := c.compileGroupBySimple(q, src, child, fromSrcs)
		c.env = origEnv
		return res, err
	}
	orig := c.env
	c.env = child
	var sel string
	if ml := tryMapLiteral(q.Select); ml != nil {
		name := c.newStructName("Result")
		sel, err = c.compileMapLiteralAsStruct(name, ml)
	} else {
		sel, err = c.compileExpr(q.Select)
	}
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err = c.compileCond(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileSortKey(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig

	tmp := c.newTmp()
	var b strings.Builder
	b.WriteString("{ let mut ")
	b.WriteString(tmp)
	b.WriteString(" = Vec::new();")
	b.WriteString(loopHead(q.Var, src, child))
	for i, fs := range fromSrcs {
		b.WriteString(" ")
		b.WriteString(loopHead(q.Froms[i].Var, fs, child))
	}
	for i, js := range joinSrcs {
		b.WriteString(" ")
		b.WriteString(loopHead(q.Joins[i].Var, js, child))
		if joinConds[i] != "" {
			b.WriteString(" if !(" + joinConds[i] + ") { continue; }")
		}
	}
	if cond != "" {
		b.WriteString(" if !(" + cond + ") { continue; }")
	}
	if sortExpr != "" {
		item := c.newTmp()
		key := c.newTmp()
		b.WriteString(" let " + item + " = " + sel + ";")
		b.WriteString(" let " + key + " = " + sortExpr + ";")
		b.WriteString(" " + tmp + ".push((" + key + ", " + item + "));")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	for range joinSrcs {
		b.WriteString(" }")
	}
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	if sortExpr != "" {
		b.WriteString(" " + tmp + ".sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());")
		res := c.newTmp()
		b.WriteString(" let mut " + res + " = Vec::new();")
		b.WriteString(" for p in " + tmp + " { " + res + ".push(p.1); }")
		tmp = res
	}
	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = tmp + ".len()"
		}
		b.WriteString(" let " + tmp + " = " + tmp + "[" + start + ".." + end + "].to_vec();")
	}
	if q.Distinct {
		uniq := c.newTmp()
		b.WriteString(" let mut set = std::collections::HashSet::new();")
		b.WriteString(" let mut " + uniq + " = Vec::new();")
		b.WriteString(" for v in " + tmp + ".into_iter() { if set.insert(v.clone()) { " + uniq + ".push(v); } }")
		b.WriteString(" let " + tmp + " = " + uniq + ";")
	}
	b.WriteString(" " + tmp + " }")
	return b.String(), nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	var b strings.Builder
	b.WriteString("{ let mut m = std::collections::BTreeMap::new();")
	for _, it := range m.Items {
		var k string
		if name, ok := c.simpleIdent(it.Key); ok {
			k = fmt.Sprintf("\"%s\"", name)
		} else {
			var err error
			k, err = c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&b, " m.insert(%s, %s);", k, v)
	}
	b.WriteString(" m }")
	return b.String(), nil
}

func (c *Compiler) compileHashMapLiteral(m *parser.MapLiteral) (string, error) {
	var b strings.Builder
	b.WriteString("{ let mut m = std::collections::HashMap::new();")
	for _, it := range m.Items {
		var k string
		if name, ok := c.simpleIdent(it.Key); ok {
			k = fmt.Sprintf("\"%s\"", name)
		} else {
			var err error
			k, err = c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&b, " m.insert(%s.to_string(), %s.to_string());", k, v)
	}
	b.WriteString(" m }")
	return b.String(), nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	if info, ok := c.variantInfo[sl.Name]; ok {
		fields := make([]string, len(sl.Fields))
		for i, f := range sl.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			if ft, ok := info.Fields[f.Name]; ok {
				if ut, ok2 := ft.(types.UnionType); ok2 && ut.Name == info.Union {
					v = fmt.Sprintf("Box::new(%s)", v)
				}
			}
			fields[i] = fmt.Sprintf("%s: %s", f.Name, v)
		}
		return fmt.Sprintf("%s::%s { %s }", info.Union, sl.Name, strings.Join(fields, ", ")), nil
	}
	fields := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", f.Name, v)
	}
	return fmt.Sprintf("%s { %s }", sl.Name, strings.Join(fields, ", ")), nil
}

func (c *Compiler) compileStructLiteralFromMap(name string, m *parser.MapLiteral) (string, error) {
	st, ok := c.structs[name]
	if !ok {
		if c.env != nil {
			if s, ok2 := c.env.GetStruct(name); ok2 {
				st = s
				c.structs[name] = st
				ok = true
			}
		}
	}
	if !ok {
		return "", fmt.Errorf("unknown struct %s", name)
	}
	fieldMap := map[string]*parser.Expr{}
	for _, it := range m.Items {
		if str, ok := c.simpleKey(it.Key); ok {
			fieldMap[str] = it.Value
		}
	}
	fields := make([]string, len(st.Order))
	for i, f := range st.Order {
		expr, ok := fieldMap[f]
		if !ok {
			return "", fmt.Errorf("missing field %s", f)
		}
		v, err := c.compileExpr(expr)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", f, v)
	}
	return fmt.Sprintf("%s { %s }", name, strings.Join(fields, ", ")), nil
}

func (c *Compiler) compileMapLiteralAsStruct(name string, m *parser.MapLiteral) (string, error) {
	st := types.StructType{Name: name, Fields: map[string]types.Type{}, Order: []string{}}
	fields := make([]string, len(m.Items))
	for i, it := range m.Items {
		str, ok := c.simpleKey(it.Key)
		if !ok {
			return "", fmt.Errorf("expected simple key")
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", str, v)
		st.Fields[str] = c.fieldType(it.Value)
		st.Order = append(st.Order, str)
	}
	c.structs[name] = st
	c.genStructs = append(c.genStructs, st)
	c.lastListStruct = name
	return fmt.Sprintf("%s { %s }", name, strings.Join(fields, ", ")), nil
}

func (c *Compiler) valueForKey(m *parser.MapLiteral, key string) *parser.Expr {
	for _, it := range m.Items {
		if str, ok := c.simpleKey(it.Key); ok && str == key {
			return it.Value
		}
	}
	return nil
}

func (c *Compiler) tryMapListStruct(varName string, list *parser.ListLiteral) (string, bool, error) {
	if len(list.Elems) == 0 {
		return "", false, nil
	}
	firstKeys, ok := c.mapKeys(list.Elems[0])
	if !ok {
		return "", false, nil
	}
	for _, e := range list.Elems[1:] {
		keys, ok := c.mapKeys(e)
		if !ok || len(keys) != len(firstKeys) {
			return "", false, nil
		}
		for i, k := range keys {
			if k != firstKeys[i] {
				return "", false, nil
			}
		}
	}
	structName := c.newStructName(singular(varName))
	st := types.StructType{Name: structName, Fields: map[string]types.Type{}, Order: firstKeys}
	firstMap := list.Elems[0].Binary.Left.Value.Target.Map
	for _, k := range firstKeys {
		expr := c.valueForKey(firstMap, k)
		t := types.TypeOfExprBasic(expr, c.env)
		st.Fields[k] = t
	}
	c.structs[structName] = st
	c.genStructs = append(c.genStructs, st)
	c.listVars[varName] = structName

	elems := make([]string, len(list.Elems))
	for i, e := range list.Elems {
		m := e.Binary.Left.Value.Target.Map
		fields := make([]string, len(firstKeys))
		for j, k := range firstKeys {
			vexpr := c.valueForKey(m, k)
			val, err := c.compileExpr(vexpr)
			if err != nil {
				return "", false, err
			}
			fields[j] = fmt.Sprintf("%s: %s", k, val)
		}
		elems[i] = fmt.Sprintf("%s { %s }", structName, strings.Join(fields, ", "))
	}
	return "vec![" + strings.Join(elems, ", ") + "]", true, nil
}

func (c *Compiler) mapKeys(e *parser.Expr) ([]string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return nil, false
	}
	keys := make([]string, len(u.Value.Target.Map.Items))
	for i, it := range u.Value.Target.Map.Items {
		str, ok := c.simpleKey(it.Key)
		if !ok {
			return nil, false
		}
		keys[i] = str
	}
	return keys, true
}

func tryListLiteral(e *parser.Expr) *parser.ListLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.List == nil {
		return nil
	}
	return u.Value.Target.List
}

func tryMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return nil
	}
	return u.Value.Target.Map
}

func (c *Compiler) compileSortKey(e *parser.Expr) (string, error) {
	if ml := tryMapLiteral(e); ml != nil {
		parts := make([]string, len(ml.Items))
		for i, it := range ml.Items {
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = v
		}
		return "(" + strings.Join(parts, ", ") + ")", nil
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	for i, a := range call.Args {
		if name, ok := c.simpleIdent(a); ok && c.isGroupVar(name) {
			args[i] += ".items"
		}
	}
	if mp, ok := c.mutParams[call.Func]; ok {
		for i := range args {
			if mp[i] {
				args[i] = fmt.Sprintf("&mut %s", args[i])
			}
		}
	}
	if typ, err := c.env.GetVar(call.Func); err == nil {
		if ft, ok := typ.(types.FuncType); ok {
			for i := range args {
				if i < len(ft.Params) {
					switch ft.Params[i].(type) {
					case types.UnionType, types.StructType:
						if !strings.HasPrefix(args[i], "&") {
							args[i] = "&" + args[i]
						}
					}
				}
			}
			if !ft.Variadic && len(args) < len(ft.Params) {
				missing := len(ft.Params) - len(args)
				params := make([]string, missing)
				for i := range params {
					params[i] = c.newTmp()
				}
				callArgs := append(append([]string{}, args...), params...)
				return fmt.Sprintf("|%s| %s(%s)", strings.Join(params, ", "), call.Func, strings.Join(callArgs, ", ")), nil
			}
		}
	}
	switch call.Func {
	case "print":
		fmtParts := make([]string, len(args))
		for i, a := range call.Args {
			if c.env != nil {
				t := types.TypeOfExpr(a, c.env)
				switch t.(type) {
				case types.StringType, types.IntType, types.Int64Type, types.FloatType, types.BoolType:
					fmtParts[i] = "{}"
				default:
					fmtParts[i] = "{:?}"
				}
			} else {
				fmtParts[i] = "{:?}"
			}
		}
		fmtStr := strings.TrimSpace(strings.Join(fmtParts, " "))
		return fmt.Sprintf("println!(\"%s\", %s)", fmtStr, strings.Join(args, ", ")), nil
	case "append":
		c.helpers["append"] = true
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		if ml := tryMapLiteral(call.Args[1]); ml != nil {
			if name, ok := c.simpleIdent(call.Args[0]); ok {
				structName := c.newStructName("Item")
				code, err := c.compileMapLiteralAsStruct(structName, ml)
				if err == nil {
					args[1] = code
					c.listVars[name] = structName
					if c.env != nil {
						c.env.SetVar(name, types.ListType{Elem: types.StructType{Name: structName}}, true)
					}
				}
			}
		}
		return fmt.Sprintf("append(%s, %s)", args[0], args[1]), nil
	case "avg":
		c.helpers["avg"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("avg(&%s)", args[0]), nil
	case "len", "count":
		if len(args) != 1 {
			return "", fmt.Errorf("%s expects 1 arg", call.Func)
		}
		return fmt.Sprintf("%s.len() as i32", args[0]), nil
	case "sum":
		c.helpers["sum"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("sum(&%s)", args[0]), nil
	case "min":
		c.helpers["min"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("min(&%s)", args[0]), nil
	case "max":
		c.helpers["max"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("max(&%s)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("(%s.len() > 0)", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values().cloned().collect::<Vec<_>>()", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("%s.to_string()", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("&%s[%s as usize..%s as usize]", args[0], args[1], args[2]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		return fmt.Sprintf("println!(\"{:?}\", %s)", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	typ := "std::collections::HashMap<String, String>"
	if l.Type != nil {
		typ = rustType(l.Type)
	}
	opts := "std::collections::HashMap::new()"
	if l.With != nil {
		if ml := tryMapLiteral(l.With); ml != nil {
			v, err := c.compileHashMapLiteral(ml)
			if err != nil {
				return "", err
			}
			opts = v
		} else {
			v, err := c.compileExpr(l.With)
			if err != nil {
				return "", err
			}
			opts = v
		}
	}
	c.helpers["_load"] = true
	return fmt.Sprintf("_load::<%s>(%s, %s)", typ, path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "std::collections::HashMap::new()"
	if s.With != nil {
		if ml := tryMapLiteral(s.With); ml != nil {
			v, err := c.compileHashMapLiteral(ml)
			if err != nil {
				return "", err
			}
			opts = v
		} else {
			v, err := c.compileExpr(s.With)
			if err != nil {
				return "", err
			}
			opts = v
		}
	}
	c.helpers["_save"] = true
	return fmt.Sprintf("_save(&%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str)
	case l.Float != nil:
		f := *l.Float
		s := fmt.Sprintf("%g", f)
		if math.Floor(f) == f {
			if !strings.Contains(s, ".") {
				s += ".0"
			}
		}
		return s
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		} else {
			return "false"
		}
	default:
		return "()"
	}
}

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "i32"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i32"
		case "bool":
			return "bool"
		case "float":
			return "f64"
		case "string":
			return "&'static str"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return fmt.Sprintf("Vec<%s>", rustType(t.Generic.Args[0]))
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return fmt.Sprintf("std::collections::HashMap<%s, %s>", rustType(t.Generic.Args[0]), rustType(t.Generic.Args[1]))
			}
		}
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = rustType(p)
		}
		ret := rustType(t.Fun.Return)
		return fmt.Sprintf("Box<dyn Fn(%s) -> %s>", strings.Join(params, ", "), ret)
	}
	return "i32"
}

func rustTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "i32"
	case types.BoolType:
		return "bool"
	case types.FloatType:
		return "f64"
	case types.StringType:
		return "&'static str"
	case types.ListType:
		return fmt.Sprintf("Vec<%s>", rustTypeFromType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("std::collections::HashMap<%s, %s>", rustTypeFromType(tt.Key), rustTypeFromType(tt.Value))
	case types.StructType:
		return tt.Name
	default:
		return "i32"
	}
}

func rustDefault(typ string) string {
	switch typ {
	case "i32":
		return "0"
	case "bool":
		return "false"
	case "f64":
		return "0.0"
	case "&'static str":
		return "\"\""
	default:
		return fmt.Sprintf("%s::default()", typ)
	}
}

func (c *Compiler) simpleString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil || len(p.Ops) > 0 {
		return "", false
	}
	return *p.Target.Lit.Str, true
}

func (c *Compiler) simpleKey(e *parser.Expr) (string, bool) {
	if s, ok := c.simpleString(e); ok {
		return s, true
	}
	if id, ok := c.simpleIdent(e); ok {
		return id, true
	}
	return "", false
}

func (c *Compiler) simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func (c *Compiler) aggCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	return call.Func, call.Args[0], true
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
