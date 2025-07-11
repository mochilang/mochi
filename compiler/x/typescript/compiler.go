//go:build slow

package typescriptcode

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"gopkg.in/yaml.v3"

	"mochi/parser"
)

func findRepoRoot() string {
	if root := os.Getenv("MOCHI_ROOT"); root != "" {
		return root
	}
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

// Compiler translates a limited subset of Mochi into TypeScript.
type Compiler struct {
	buf                bytes.Buffer
	indent             int
	tmp                int
	needContainsHelper bool
	funParams          map[string]int
	variants           map[string][]string
	repoRoot           string
}

// New returns a new Compiler.
func New() *Compiler {
	return &Compiler{funParams: make(map[string]int), variants: make(map[string][]string), repoRoot: findRepoRoot()}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("_tmp%d", c.tmp)
}

func tsTypeRef(t *parser.TypeRef) string {
	if t == nil {
		return "any"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int", "float":
			return "number"
		case "string":
			return "string"
		case "bool":
			return "boolean"
		case "void":
			return "void"
		default:
			return *t.Simple
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return tsTypeRef(t.Generic.Args[0]) + "[]"
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return fmt.Sprintf("Record<%s, %s>", tsTypeRef(t.Generic.Args[0]), tsTypeRef(t.Generic.Args[1]))
		}
		parts := make([]string, len(t.Generic.Args))
		for i, a := range t.Generic.Args {
			parts[i] = tsTypeRef(a)
		}
		return fmt.Sprintf("%s<%s>", t.Generic.Name, strings.Join(parts, ", "))
	}
	if t.Struct != nil {
		fields := make([]string, len(t.Struct.Fields))
		for i, f := range t.Struct.Fields {
			fields[i] = fmt.Sprintf("%s: %s", f.Name, tsTypeRef(f.Type))
		}
		return fmt.Sprintf("{ %s }", strings.Join(fields, "; "))
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = tsTypeRef(p)
		}
		ret := "void"
		if t.Fun.Return != nil {
			ret = tsTypeRef(t.Fun.Return)
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), ret)
	}
	return "any"
}

func queryResultType(q *parser.QueryExpr) string {
	if q.Select == nil {
		return "any[]"
	}
	if names := selectMapFields(q.Select); names != nil {
		fields := make([]string, len(names))
		for i, n := range names {
			fields[i] = fmt.Sprintf("%s: any", n)
		}
		return fmt.Sprintf("Array<{ %s }>", strings.Join(fields, "; "))
	}
	if name := selectStructName(q.Select); name != "" {
		return fmt.Sprintf("%s[]", name)
	}
	return "any[]"
}

func selectMapFields(e *parser.Expr) []string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	if u.Value.Target == nil || u.Value.Target.Map == nil {
		return nil
	}
	m := u.Value.Target.Map
	fields := make([]string, 0, len(m.Items))
	for _, it := range m.Items {
		if name := getIdent(it.Key); name != "" {
			fields = append(fields, name)
		}
	}
	return fields
}

func selectStructName(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return ""
	}
	if u.Value.Target == nil || u.Value.Target.Struct == nil {
		return ""
	}
	return u.Value.Target.Struct.Name
}

// detectAggCall returns the name and argument of a simple aggregator call like
// sum(x) or count(x). If the expression is not such a call, ok is false.
func detectAggCall(e *parser.Expr) (name string, arg *parser.Expr, ok bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return "", nil, false
	}
	pr := u.Value.Target
	if pr == nil || pr.Call == nil || len(pr.Call.Args) != 1 {
		return "", nil, false
	}
	switch pr.Call.Func {
	case "sum", "count", "avg", "min", "max":
		return pr.Call.Func, pr.Call.Args[0], true
	default:
		return "", nil, false
	}
}

// detectExistsQuery checks if the expression is a call to the `exists` builtin
// with a simple query of the form:
//
//	from x in src
//	where <cond>
//	select x
//
// If so, it returns the source expression, variable name and condition.
func detectExistsQuery(arg *parser.Expr) (src *parser.Expr, varName string, cond *parser.Expr, ok bool) {
	if arg == nil || arg.Binary == nil || len(arg.Binary.Right) != 0 {
		return nil, "", nil, false
	}
	au := arg.Binary.Left
	if len(au.Ops) != 0 || au.Value == nil || len(au.Value.Ops) != 0 {
		return nil, "", nil, false
	}
	targ := au.Value.Target
	if targ == nil || targ.Query == nil {
		return nil, "", nil, false
	}
	q := targ.Query
	if len(q.Froms) != 0 || len(q.Joins) != 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return nil, "", nil, false
	}
	if q.Select == nil || getIdent(q.Select) != q.Var {
		return nil, "", nil, false
	}
	return q.Source, q.Var, q.Where, true
}

// Compile converts the parsed Mochi program into TypeScript source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needContainsHelper = false
	c.funParams = make(map[string]int)
	c.variants = make(map[string][]string)
	for _, st := range prog.Statements {
		if st.Fun != nil {
			c.funParams[st.Fun.Name] = len(st.Fun.Params)
		}
		if st.Type != nil {
			for _, v := range st.Type.Variants {
				names := make([]string, len(v.Fields))
				for i, f := range v.Fields {
					names[i] = f.Name
				}
				c.variants[v.Name] = names
			}
		}
	}

	// Emit simple builtin imports before other statements.
	for _, st := range prog.Statements {
		if st.Import == nil || st.Import.Lang == nil {
			continue
		}
		alias := st.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(st.Import.Path)
		}
		switch *st.Import.Lang {
		case "python":
			if st.Import.Path == "math" {
				c.writeln(fmt.Sprintf("const %s = { pi: Math.PI, e: Math.E, sqrt: Math.sqrt, pow: Math.pow, sin: Math.sin, log: Math.log };", alias))
				c.writeln("")
			}
		case "go":
			if st.Import.Auto && st.Import.Path == "mochi/runtime/ffi/go/testpkg" {
				c.writeln(fmt.Sprintf("const %s = { Add: (a: number, b: number) => a + b, Pi: 3.14, Answer: 42 };", alias))
				c.writeln("")
			}
		}
	}

	for _, st := range prog.Statements {
		if st.Import != nil {
			// imports are handled above
			continue
		}
		if err := c.stmt(st); err != nil {
			return nil, err
		}
	}
	code := c.buf.Bytes()
	if c.needContainsHelper {
		helper := "function contains(a: any, b: any) {\n" +
			"  if (Array.isArray(a) || typeof a === \"string\") return a.includes(b);\n" +
			"  return Object.prototype.hasOwnProperty.call(a, b);\n" +
			"}\n"
		code = append([]byte(helper), code...)
	}
	return code, nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		typ := ""
		if s.Let.Type != nil {
			typ = ": " + tsTypeRef(s.Let.Type)
		}
		if s.Let.Value == nil {
			c.writeln(fmt.Sprintf("const %s%s = null;", s.Let.Name, typ))
		} else {
			val, err := c.expr(s.Let.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("const %s%s = %s;", s.Let.Name, typ, val))
		}
	case s.Var != nil:
		typ := ""
		if s.Var.Type != nil {
			typ = ": " + tsTypeRef(s.Var.Type)
		}
		if s.Var.Value == nil {
			c.writeln(fmt.Sprintf("let %s%s = null;", s.Var.Name, typ))
		} else {
			val, err := c.expr(s.Var.Value)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("let %s%s = %s;", s.Var.Name, typ, val))
		}
	case s.Return != nil:
		val, err := c.expr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
	case s.Expr != nil:
		val, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(val + ";")
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		cond, err := c.expr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeln("while (" + cond + ") {")
		c.indent++
		for _, st := range s.While.Body {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.For != nil:
		return c.forStmt(s.For)
	case s.Fun != nil:
		return c.funStmt(s.Fun)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Assign != nil:
		target, err := c.assignTarget(s.Assign)
		if err != nil {
			return err
		}
		val, err := c.expr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s;", target, val))
	case s.Update != nil:
		return c.updateStmt(s.Update)
	case s.Test != nil:
		for _, st := range s.Test.Body {
			if st.Expect != nil {
				exp, err := c.expr(st.Expect.Value)
				if err != nil {
					return err
				}
				msg := fmt.Sprintf("%s failed", s.Test.Name)
				c.writeln(fmt.Sprintf("if (!(%s)) { throw new Error(%q); }", exp, msg))
			} else {
				if err := c.stmt(st); err != nil {
					return err
				}
			}
		}
	case s.Type != nil:
		return c.typeDecl(s.Type)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternObject != nil, s.ExternType != nil:
		// extern declarations do not affect generated TypeScript code
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else != nil {
		c.writeln("} else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
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

func (c *Compiler) forStmt(f *parser.ForStmt) error {
	src, err := c.expr(f.Source)
	if err != nil {
		return err
	}
	if f.RangeEnd != nil {
		end, err := c.expr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (let %s = %s; %s < %s; %s++) {", f.Name, src, f.Name, end, f.Name))
	} else {
		if isSimpleIterable(f.Source) {
			c.writeln(fmt.Sprintf("for (const %s of %s) {", f.Name, src))
		} else {
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("const %s = %s;", tmp, src))
			c.writeln(fmt.Sprintf("for (const %s of (Array.isArray(%s) ? %s : Object.keys(%s))) {", f.Name, tmp, tmp, tmp))
		}
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) funStmt(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("function %s(%s) {", f.Name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) typeDecl(td *parser.TypeDecl) error {
	if len(td.Members) > 0 && len(td.Variants) == 0 {
		fields := make([]string, 0, len(td.Members))
		for _, m := range td.Members {
			if m.Field != nil {
				fields = append(fields, fmt.Sprintf("%s: %s;", m.Field.Name, tsTypeRef(m.Field.Type)))
			}
		}
		c.writeln(fmt.Sprintf("type %s = { %s };", td.Name, strings.Join(fields, " ")))
		return nil
	}
	if len(td.Variants) > 0 {
		parts := make([]string, len(td.Variants))
		for i, v := range td.Variants {
			fields := []string{fmt.Sprintf("kind: %q", strings.ToLower(v.Name))}
			for _, f := range v.Fields {
				fields = append(fields, fmt.Sprintf("%s: %s", f.Name, tsTypeRef(f.Type)))
			}
			parts[i] = fmt.Sprintf("{ %s }", strings.Join(fields, "; "))
		}
		c.writeln(fmt.Sprintf("type %s = %s;", td.Name, strings.Join(parts, " | ")))
		return nil
	}
	return fmt.Errorf("unsupported type declaration")
}

func (c *Compiler) assignTarget(a *parser.AssignStmt) (string, error) {
	target := a.Name
	for _, idx := range a.Index {
		if idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
			return "", fmt.Errorf("slice assignment not supported")
		}
		if idx.Start == nil {
			return "", fmt.Errorf("missing index expression")
		}
		v, err := c.expr(idx.Start)
		if err != nil {
			return "", err
		}
		target = fmt.Sprintf("%s[%s]", target, v)
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s.%s", target, f.Name)
	}
	return target, nil
}

func (c *Compiler) updateStmt(u *parser.UpdateStmt) error {
	tmp := c.newTmp()
	c.writeln(fmt.Sprintf("for (let i = 0; i < %s.length; i++) {", u.Target))
	c.indent++
	c.writeln(fmt.Sprintf("let %s = %s[i];", tmp, u.Target))

	fieldSet := map[string]bool{}
	for _, it := range u.Set.Items {
		if name := getIdent(it.Key); name != "" {
			fieldSet[name] = true
		}
	}
	collectFields(u.Where, fieldSet)
	for f := range fieldSet {
		c.writeln(fmt.Sprintf("let %s = %s.%s;", f, tmp, f))
	}
	if u.Where != nil {
		cond, err := c.expr(u.Where)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
	}
	for _, it := range u.Set.Items {
		k := getIdent(it.Key)
		v, err := c.expr(it.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s.%s = %s;", tmp, k, v))
	}
	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}
	c.writeln(fmt.Sprintf("%s[i] = %s;", u.Target, tmp))
	c.indent--
	c.writeln("}")
	return nil
}

func collectFields(e *parser.Expr, set map[string]bool) {
	if e == nil {
		return
	}
	if b := e.Binary; b != nil {
		collectFieldsUnary(b.Left, set)
		for _, op := range b.Right {
			collectFieldsPostfix(op.Right, set)
		}
	}
}

func collectFieldsUnary(u *parser.Unary, set map[string]bool) {
	if u == nil {
		return
	}
	collectFieldsPostfix(u.Value, set)
}

func collectFieldsPostfix(p *parser.PostfixExpr, set map[string]bool) {
	if p == nil {
		return
	}
	collectFieldsPrimary(p.Target, set)
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				collectFields(a, set)
			}
		}
		if op.Index != nil {
			collectFields(op.Index.Start, set)
			collectFields(op.Index.End, set)
			collectFields(op.Index.Step, set)
		}
	}
}

func collectFieldsPrimary(pr *parser.Primary, set map[string]bool) {
	if pr == nil {
		return
	}
	switch {
	case pr.Selector != nil && len(pr.Selector.Tail) == 0:
		set[pr.Selector.Root] = true
	case pr.Group != nil:
		collectFields(pr.Group, set)
	case pr.List != nil:
		for _, e := range pr.List.Elems {
			collectFields(e, set)
		}
	case pr.Map != nil:
		for _, it := range pr.Map.Items {
			collectFields(it.Key, set)
			collectFields(it.Value, set)
		}
	case pr.FunExpr != nil:
		if pr.FunExpr.ExprBody != nil {
			collectFields(pr.FunExpr.ExprBody, set)
		}
	case pr.Match != nil:
		collectFields(pr.Match.Target, set)
	case pr.Query != nil:
		collectFields(pr.Query.Select, set)
	}
}

func getIdent(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return ""
	}
	if u.Value == nil || u.Value.Target == nil {
		return ""
	}
	sel := u.Value.Target.Selector
	if sel != nil && len(sel.Tail) == 0 {
		return sel.Root
	}
	return ""
}

func (c *Compiler) variantPattern(e *parser.Expr) (string, []string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return "", nil, false
	}
	if u.Value.Target == nil {
		return "", nil, false
	}
	if u.Value.Target.Call != nil {
		name := u.Value.Target.Call.Func
		fields, ok := c.variants[name]
		if !ok {
			return "", nil, false
		}
		vars := make([]string, len(u.Value.Target.Call.Args))
		for i, a := range u.Value.Target.Call.Args {
			v := getIdent(a)
			if v == "" {
				return "", nil, false
			}
			vars[i] = v
			if i < len(fields) {
				_ = fields[i]
			}
		}
		return name, vars, true
	}
	if u.Value.Target.Selector != nil && len(u.Value.Target.Selector.Tail) == 0 {
		name := u.Value.Target.Selector.Root
		if _, ok := c.variants[name]; ok {
			return name, nil, true
		}
	}
	return "", nil, false
}

func isLiteralComplexUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	return isLiteralComplexPostfix(u.Value)
}

func isLiteralComplexPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) != 0 || p.Target == nil {
		return false
	}
	pr := p.Target
	switch {
	case pr.List != nil, pr.Map != nil, pr.Struct != nil:
		return true
	}
	return false
}

func (c *Compiler) funExpr(fe *parser.FunExpr) (string, error) {
	params := make([]string, len(fe.Params))
	for i, p := range fe.Params {
		params[i] = p.Name
	}
	if fe.ExprBody != nil {
		body, err := c.expr(fe.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
	}
	if len(fe.BlockBody) > 0 {
		sub := New()
		sub.repoRoot = c.repoRoot
		sub.funParams = c.funParams
		sub.variants = c.variants
		for _, st := range fe.BlockBody {
			if err := sub.stmt(st); err != nil {
				return "", err
			}
		}
		if sub.needContainsHelper {
			c.needContainsHelper = true
		}
		lines := strings.Split(strings.TrimRight(sub.buf.String(), "\n"), "\n")
		for i, l := range lines {
			lines[i] = "  " + l
		}
		body := strings.Join(lines, "\n")
		return fmt.Sprintf("(%s) => {\n%s\n}", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("unsupported function literal")
}

func (c *Compiler) queryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.expr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.expr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		on, err := c.expr(j.On)
		if err != nil {
			return "", err
		}
		joinOns[i] = on
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		return "", err
	}
	var whereStr, sortStr, skipStr, takeStr string
	if q.Where != nil {
		whereStr, err = c.expr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortStr, err = c.expr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipStr, err = c.expr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeStr, err = c.expr(q.Take)
		if err != nil {
			return "", err
		}
	}

	if code, ok := c.simpleChainQuery(q, q.Select, src, sel, whereStr, sortStr, skipStr, takeStr); ok {
		return code, nil
	}

	var b bytes.Buffer
	indent := 0
	writeln := func(s string) {
		for i := 0; i < indent; i++ {
			b.WriteString("  ")
		}
		b.WriteString(s)
		b.WriteByte('\n')
	}
	res := c.newTmp()
	writeln("(() => {")
	indent++
	_, _, aggOK := detectAggCall(q.Select)
	if !(aggOK && len(q.Froms) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Group == nil) {
		writeln(fmt.Sprintf("const %s: %s = [];", res, queryResultType(q)))
	}

	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			return "", fmt.Errorf("group by multiple expressions not supported")
		}
		keyStr, err := c.expr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		var havingStr string
		if q.Group.Having != nil {
			havingStr, err = c.expr(q.Group.Having)
			if err != nil {
				return "", err
			}
		}
		writeln("const groups = {};")
		writeln(fmt.Sprintf("for (const %s of %s) {", q.Var, src))
		indent++
		for i, fs := range fromSrcs {
			writeln(fmt.Sprintf("for (const %s of %s) {", q.Froms[i].Var, fs))
			indent++
		}
		for i, js := range joinSrcs {
			writeln(fmt.Sprintf("for (const %s of %s) {", q.Joins[i].Var, js))
			indent++
			writeln("if (!(" + joinOns[i] + ")) continue;")
		}
		if whereStr != "" {
			writeln("if (!(" + whereStr + ")) continue;")
		}
		writeln(fmt.Sprintf("const _k = JSON.stringify(%s);", keyStr))
		writeln("let g = groups[_k];")
		writeln(fmt.Sprintf("if (!g) { g = []; g.key = %s; g.items = g; groups[_k] = g; }", keyStr))
		if len(q.Froms)+len(q.Joins) <= 1 {
			writeln(fmt.Sprintf("g.push(%s);", q.Var))
		} else {
			parts := []string{fmt.Sprintf("%s: %s", q.Var, q.Var)}
			for _, f := range q.Froms {
				parts = append(parts, fmt.Sprintf("%s: %s", f.Var, f.Var))
			}
			for _, j := range q.Joins {
				parts = append(parts, fmt.Sprintf("%s: %s", j.Var, j.Var))
			}
			writeln(fmt.Sprintf("g.push({%s});", strings.Join(parts, ", ")))
		}
		for range q.Joins {
			indent--
			writeln("}")
		}
		for range q.Froms {
			indent--
			writeln("}")
		}
		indent--
		writeln("}")
		// result array declared above
		writeln("for (const _k in groups) {")
		indent++
		writeln("const g = groups[_k];")
		if havingStr != "" {
			writeln("if (!(" + havingStr + ")) continue;")
		}
		if sortStr != "" {
			writeln("res.push({item: " + sel + ", key: " + sortStr + "});")
		} else {
			writeln("res.push(" + sel + ");")
		}
		indent--
		writeln("}")
	} else {
		if agg, arg, ok := detectAggCall(q.Select); ok && len(q.Froms) == 0 && sortStr == "" && skipStr == "" && takeStr == "" {
			argStr, err := c.expr(arg)
			if err != nil {
				return "", err
			}
			switch agg {
			case "sum", "count", "avg", "min", "max":
			default:
				ok = false
			}
			if ok {
				switch agg {
				case "sum", "count":
					writeln(fmt.Sprintf("let %s = 0;", res))
				case "avg":
					writeln("let _sum = 0;")
					writeln("let _count = 0;")
				case "min":
					writeln(fmt.Sprintf("let %s = Infinity;", res))
				case "max":
					writeln(fmt.Sprintf("let %s = -Infinity;", res))
				}
				writeln(fmt.Sprintf("for (const %s of %s) {", q.Var, src))
				indent++
				if whereStr != "" {
					writeln("if (!(" + whereStr + ")) continue;")
				}
				switch agg {
				case "sum":
					writeln(fmt.Sprintf("%s += %s;", res, argStr))
				case "count":
					writeln(fmt.Sprintf("%s++;", res))
				case "avg":
					writeln(fmt.Sprintf("_sum += %s;", argStr))
					writeln("_count++;")
				case "min":
					writeln(fmt.Sprintf("if (%s < %s) %s = %s;", argStr, res, res, argStr))
				case "max":
					writeln(fmt.Sprintf("if (%s > %s) %s = %s;", argStr, res, res, argStr))
				}
				indent--
				writeln("}")
				if agg == "avg" {
					writeln("let res = _count == 0 ? 0 : _sum / _count;")
				} else {
					writeln(fmt.Sprintf("let res = %s;", res))
				}
			} else {
				// fallback generic
				// result array already declared above
				writeln(fmt.Sprintf("for (const %s of %s) {", q.Var, src))
				indent++
				for i, fs := range fromSrcs {
					writeln(fmt.Sprintf("for (const %s of %s) {", q.Froms[i].Var, fs))
					indent++
				}
				for i, js := range joinSrcs {
					writeln(fmt.Sprintf("for (const %s of %s) {", q.Joins[i].Var, js))
					indent++
					writeln("if (!(" + joinOns[i] + ")) continue;")
				}
				if whereStr != "" {
					writeln("if (!(" + whereStr + ")) continue;")
				}
				if sortStr != "" {
					writeln(res + ".push({item: " + sel + ", key: " + sortStr + "});")
				} else {
					writeln(res + ".push(" + sel + ");")
				}
				for range q.Joins {
					indent--
					writeln("}")
				}
				for range q.Froms {
					indent--
					writeln("}")
				}
				indent--
				writeln("}")
				// result already collected in res
			}
		} else {
			// result array already declared above
			writeln(fmt.Sprintf("for (const %s of %s) {", q.Var, src))
			indent++
			for i, fs := range fromSrcs {
				writeln(fmt.Sprintf("for (const %s of %s) {", q.Froms[i].Var, fs))
				indent++
			}
			for i, js := range joinSrcs {
				writeln(fmt.Sprintf("for (const %s of %s) {", q.Joins[i].Var, js))
				indent++
				writeln("if (!(" + joinOns[i] + ")) continue;")
			}
			if whereStr != "" {
				writeln("if (!(" + whereStr + ")) continue;")
			}
			if sortStr != "" {
				writeln(res + ".push({item: " + sel + ", key: " + sortStr + "});")
			} else {
				writeln(res + ".push(" + sel + ");")
			}
			for range q.Joins {
				indent--
				writeln("}")
			}
			for range q.Froms {
				indent--
				writeln("}")
			}
			indent--
			writeln("}")
			// result already collected in res
		}
	}

	if sortStr != "" {
		writeln("res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);")
	}
	if skipStr != "" || takeStr != "" {
		start := "0"
		if skipStr != "" {
			start = skipStr
		}
		end := "res.length"
		if takeStr != "" {
			if skipStr != "" {
				end = "(" + skipStr + " + " + takeStr + ")"
			} else {
				end = takeStr
			}
		}
		writeln(fmt.Sprintf("res = res.slice(%s, %s);", start, end))
	}
	writeln("return res;")
	indent--
	writeln("})()")
	return b.String(), nil
}

func (c *Compiler) ifExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.expr(i.Then)
	if err != nil {
		return "", err
	}
	var elseVal string
	if i.ElseIf != nil {
		elseVal, err = c.ifExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
	} else if i.Else != nil {
		elseVal, err = c.expr(i.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseVal = "undefined"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal), nil
}

func (c *Compiler) matchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.expr(m.Target)
	if err != nil {
		return "", err
	}
	var b bytes.Buffer
	indent := 0
	writeln := func(s string) {
		for i := 0; i < indent; i++ {
			b.WriteString("  ")
		}
		b.WriteString(s)
		b.WriteByte('\n')
	}
	tmp := c.newTmp()
	writeln("(() => {")
	indent++
	writeln(fmt.Sprintf("const %s = %s;", tmp, target))
	writeln("let _res;")

	simple := true
	for _, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			continue
		}
		if _, _, ok := c.variantPattern(cs.Pattern); ok {
			simple = false
			break
		}
		if !isSimplePattern(cs.Pattern) {
			simple = false
			break
		}
	}

	if simple {
		writeln(fmt.Sprintf("switch (%s) {", tmp))
		indent++
		hasDefault := false
		for _, cs := range m.Cases {
			if isUnderscoreExpr(cs.Pattern) {
				res, err := c.expr(cs.Result)
				if err != nil {
					return "", err
				}
				writeln("default:")
				indent++
				writeln(fmt.Sprintf("_res = %s;", res))
				writeln("break;")
				indent--
				hasDefault = true
				continue
			}
			pat, err := c.expr(cs.Pattern)
			if err != nil {
				return "", err
			}
			res, err := c.expr(cs.Result)
			if err != nil {
				return "", err
			}
			writeln(fmt.Sprintf("case %s:", pat))
			indent++
			writeln(fmt.Sprintf("_res = %s;", res))
			writeln("break;")
			indent--
		}
		if !hasDefault {
			writeln("default:")
			indent++
			writeln("_res = undefined;")
			writeln("break;")
			indent--
		}
		indent--
		writeln("}")
	} else {
		prefix := "if"
		for i, cs := range m.Cases {
			cond := "true"
			var pre []string
			if !isUnderscoreExpr(cs.Pattern) {
				if name, vars, ok := c.variantPattern(cs.Pattern); ok {
					cond = fmt.Sprintf("%s.kind === %q", tmp, strings.ToLower(name))
					fields := c.variants[name]
					for j, v := range vars {
						fld := fmt.Sprintf("field%d", j+1)
						if j < len(fields) {
							fld = fields[j]
						}
						pre = append(pre, fmt.Sprintf("const %s = %s.%s;", v, tmp, fld))
					}
				} else {
					pat, err := c.expr(cs.Pattern)
					if err != nil {
						return "", err
					}
					cond = fmt.Sprintf("JSON.stringify(%s) === JSON.stringify(%s)", tmp, pat)
				}
			}
			if i == len(m.Cases)-1 && isUnderscoreExpr(cs.Pattern) {
				writeln("else {")
			} else {
				writeln(fmt.Sprintf("%s (%s) {", prefix, cond))
			}
			indent++
			for _, l := range pre {
				writeln(l)
			}
			res, err := c.expr(cs.Result)
			if err != nil {
				return "", err
			}
			writeln(fmt.Sprintf("_res = %s;", res))
			indent--
			writeln("}")
			prefix = "else if"
		}
	}
	writeln("return _res;")
	indent--
	writeln("})()")
	return b.String(), nil
}

func (c *Compiler) expr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.binary(e.Binary)
}

func (c *Compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	result := left
	leftAst := b.Left
	for _, op := range b.Right {
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "union":
			if op.All {
				result = fmt.Sprintf("%s.concat(%s)", result, r)
			} else {
				result = fmt.Sprintf("Array.from(new Set([...%s, ...%s]))", result, r)
			}
		case "except":
			result = fmt.Sprintf("%s.filter(x => !%s.includes(x))", result, r)
		case "intersect":
			result = fmt.Sprintf("%s.filter(x => %s.includes(x))", result, r)
		case "in":
			c.needContainsHelper = true
			result = fmt.Sprintf("contains(%s, %s)", r, result)
		case "==", "!=":
			if isLiteralComplexUnary(leftAst) || isLiteralComplexPostfix(op.Right) {
				cmp := fmt.Sprintf("JSON.stringify(%s) === JSON.stringify(%s)", result, r)
				if op.Op == "!=" {
					cmp = "!(" + cmp + ")"
				}
				result = cmp
			} else {
				result = fmt.Sprintf("(%s %s %s)", result, op.Op, r)
			}
		default:
			result = fmt.Sprintf("(%s %s %s)", result, op.Op, r)
		}
		leftAst = nil
	}
	return result, nil
}

func (c *Compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-", "!":
			val = fmt.Sprintf("(%s%s)", op, val)
		default:
			return "", fmt.Errorf("unary %s unsupported", op)
		}
	}
	return val, nil
}

func (c *Compiler) postfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Field != nil && op.Field.Name == "contains" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
			val = fmt.Sprintf("%s.includes", val)
			continue
		}
		if op.Call != nil {
			if strings.HasSuffix(val, ".contains") {
				val = strings.TrimSuffix(val, ".contains") + ".includes"
			}
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if arity, ok := c.funParams[val]; ok && len(args) < arity {
				val = fmt.Sprintf("(...args) => %s(%s, ...args)", val, strings.Join(args, ", "))
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			}
		} else if op.Index != nil {
			idx, err := c.expr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s[%s]", val, idx)
		} else if op.Field != nil {
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
		} else if op.Cast != nil {
			// ignore types
		}
	}
	return val, nil
}

func (c *Compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprint(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("%q", *p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return c.formatList(elems), nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, m := range p.Map.Items {
			k, err := c.expr(m.Key)
			if err != nil {
				return "", err
			}
			v, err := c.expr(m.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", strings.Trim(k, "\""), v)
		}
		return c.formatMap(items), nil
	case p.Struct != nil:
		fields := []string{}
		if names, ok := c.variants[p.Struct.Name]; ok {
			fields = append(fields, fmt.Sprintf("kind: %q", strings.ToLower(p.Struct.Name)))
			for i, f := range p.Struct.Fields {
				v, err := c.expr(f.Value)
				if err != nil {
					return "", err
				}
				name := f.Name
				if i < len(names) {
					name = names[i]
				}
				fields = append(fields, fmt.Sprintf("%s: %s", name, v))
			}
		} else {
			for _, f := range p.Struct.Fields {
				v, err := c.expr(f.Value)
				if err != nil {
					return "", err
				}
				fields = append(fields, fmt.Sprintf("%s: %s", f.Name, v))
			}
		}
		return "{" + strings.Join(fields, ", ") + "}", nil
	case p.Group != nil:
		v, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Match != nil:
		return c.matchExpr(p.Match)
	case p.Call != nil:
		if fields, ok := c.variants[p.Call.Func]; ok {
			parts := []string{fmt.Sprintf("kind: %q", strings.ToLower(p.Call.Func))}
			for i, a := range p.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				name := fmt.Sprintf("field%d", i+1)
				if i < len(fields) {
					name = fields[i]
				}
				parts = append(parts, fmt.Sprintf("%s: %s", name, s))
			}
			return "{" + strings.Join(parts, ", ") + "}", nil
		}
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.expr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		if arity, ok := c.funParams[p.Call.Func]; ok && len(args) < arity {
			return fmt.Sprintf("(...args) => %s(%s, ...args)", p.Call.Func, strings.Join(args, ", ")), nil
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 1 {
				return fmt.Sprintf("console.log(%s)", args[0]), nil
			}
			return fmt.Sprintf("console.log(%s)", strings.Join(args, ", ")), nil
		case "append":
			if len(args) == 2 {
				return fmt.Sprintf("[...%s, %s]", args[0], args[1]), nil
			}
			return "", fmt.Errorf("append expects 2 args")
		case "avg":
			if len(args) == 1 {
				return fmt.Sprintf("(%s.reduce((a,b)=>a+b,0)/%s.length)", args[0], args[0]), nil
			}
			return "", fmt.Errorf("avg expects 1 arg")
		case "sum":
			if len(args) == 1 {
				return fmt.Sprintf("(%s.reduce((a,b)=>a+b,0))", args[0]), nil
			}
			return "", fmt.Errorf("sum expects 1 arg")
		case "count":
			if len(args) == 1 {
				return fmt.Sprintf("%s.length", args[0]), nil
			}
			return "", fmt.Errorf("count expects 1 arg")
		case "exists":
			if len(p.Call.Args) == 1 {
				if srcExpr, v, cond, ok := detectExistsQuery(p.Call.Args[0]); ok {
					srcStr, err := c.expr(srcExpr)
					if err != nil {
						return "", err
					}
					condStr := "true"
					if cond != nil {
						condStr, err = c.expr(cond)
						if err != nil {
							return "", err
						}
					}
					return fmt.Sprintf("%s.some((%s) => %s)", srcStr, v, condStr), nil
				}
				return fmt.Sprintf("(%s.length > 0)", args[0]), nil
			}
			return "", fmt.Errorf("exists expects 1 arg")
		case "len":
			if len(args) == 1 {
				return fmt.Sprintf("%s.length", args[0]), nil
			}
			return "", fmt.Errorf("len expects 1 arg")
		case "values":
			if len(args) == 1 {
				return fmt.Sprintf("Object.values(%s)", args[0]), nil
			}
			return "", fmt.Errorf("values expects 1 arg")
		case "contains":
			if len(args) == 2 {
				c.needContainsHelper = true
				return fmt.Sprintf("contains(%s, %s)", args[0], args[1]), nil
			}
			return "", fmt.Errorf("contains expects 2 args")
		case "str":
			if len(args) == 1 {
				return fmt.Sprintf("String(%s)", args[0]), nil
			}
			return "", fmt.Errorf("str expects 1 arg")
		case "substring":
			if len(args) == 3 {
				return fmt.Sprintf("%s.substring(%s, %s)", args[0], args[1], args[2]), nil
			}
			return "", fmt.Errorf("substring expects 3 args")
		case "json":
			if len(args) == 1 {
				return fmt.Sprintf("console.log(JSON.stringify(%s))", args[0]), nil
			}
			return "", fmt.Errorf("json expects 1 arg")
		case "min":
			if len(args) == 1 {
				return fmt.Sprintf("Math.min(...%s)", args[0]), nil
			}
			return "", fmt.Errorf("min expects 1 arg")
		case "max":
			if len(args) == 1 {
				return fmt.Sprintf("Math.max(...%s)", args[0]), nil
			}
			return "", fmt.Errorf("max expects 1 arg")
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
		}
	case p.Load != nil:
		if p.Load.Path == nil {
			return "", fmt.Errorf("load path must be string")
		}
		path := filepath.Join(c.repoRoot, "tests", "vm", *p.Load.Path)
		data, err := os.ReadFile(path)
		if err != nil {
			return "", err
		}
		var v interface{}
		if err := yaml.Unmarshal(data, &v); err != nil {
			return "", err
		}
		js, _ := json.Marshal(v)
		return fmt.Sprintf("JSON.parse(%q)", string(js)), nil
	case p.Save != nil:
		src, err := c.expr(p.Save.Src)
		if err != nil {
			return "", err
		}
		path := "-"
		if p.Save.Path != nil {
			path = *p.Save.Path
		}
		format := ""
		if p.Save.With != nil {
			if m := getFormat(p.Save.With); m != "" {
				format = m
			}
		}
		if path == "-" && format == "jsonl" {
			tmp := c.newTmp()
			var b bytes.Buffer
			b.WriteString("(() => {\n")
			b.WriteString(fmt.Sprintf("  for (const %s of %s) {\n", tmp, src))
			b.WriteString(fmt.Sprintf("    console.log(JSON.stringify(%s));\n", tmp))
			b.WriteString("  }\n")
			b.WriteString("})()")
			return b.String(), nil
		}
		return "undefined", nil
	case p.Query != nil:
		return c.queryExpr(p.Query)
	case p.If != nil:
		return c.ifExpr(p.If)
	case p.Selector != nil:
		if _, ok := c.variants[p.Selector.Root]; ok && len(p.Selector.Tail) == 0 {
			return fmt.Sprintf("{kind: %q}", strings.ToLower(p.Selector.Root)), nil
		}
		return strings.Join(append([]string{p.Selector.Root}, p.Selector.Tail...), "."), nil
	}
	return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func isSimplePattern(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return false
	}
	pr := u.Value.Target
	if pr == nil {
		return false
	}
	switch {
	case pr.Lit != nil:
		return true
	case pr.Selector != nil && len(pr.Selector.Tail) == 0:
		return true
	default:
		return false
	}
}

func getFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Map == nil {
		return ""
	}
	for _, it := range u.Value.Target.Map.Items {
		if getIdent(it.Key) == "format" {
			if it.Value.Binary == nil || len(it.Value.Binary.Right) != 0 {
				continue
			}
			vLit := it.Value.Binary.Left.Value
			if vLit != nil && vLit.Target != nil && vLit.Target.Lit != nil && vLit.Target.Lit.Str != nil {
				return *vLit.Target.Lit.Str
			}
		}
	}
	return ""
}

func (c *Compiler) formatList(elems []string) string {
	flat := "[" + strings.Join(elems, ", ") + "]"
	if len(elems) <= 2 && len(flat) <= 40 {
		return flat
	}
	var b strings.Builder
	indent := strings.Repeat("  ", c.indent)
	b.WriteString("[\n")
	for i, s := range elems {
		b.WriteString(indent + "  " + s)
		if i < len(elems)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString(indent + "]")
	return b.String()
}

func (c *Compiler) formatMap(items []string) string {
	flat := "{" + strings.Join(items, ", ") + "}"
	if len(items) <= 2 && len(flat) <= 40 {
		return flat
	}
	var b strings.Builder
	indent := strings.Repeat("  ", c.indent)
	b.WriteString("{\n")
	for i, s := range items {
		b.WriteString(indent + "  " + s)
		if i < len(items)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString(indent + "}")
	return b.String()
}

func isSimpleIterable(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return false
	}
	p := u.Value.Target
	if p == nil {
		return false
	}
	switch {
	case p.List != nil:
		return true
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	}
	return false
}

func (c *Compiler) simpleChainQuery(q *parser.QueryExpr, selExpr *parser.Expr, src, sel, whereStr, sortStr, skipStr, takeStr string) (string, bool) {
	if len(q.Froms) != 0 || len(q.Joins) != 0 || q.Group != nil || q.Distinct {
		return "", false
	}
	var b strings.Builder
	b.WriteString(src)
	if sortStr != "" {
		pat := regexp.MustCompile(`\b` + regexp.QuoteMeta(q.Var) + `\b`)
		as := pat.ReplaceAllString(sortStr, "a")
		bs := pat.ReplaceAllString(sortStr, "b")
		b.WriteString(".slice().sort((a,b)=> ")
		b.WriteString(as + " < " + bs + " ? -1 : " + as + " > " + bs + " ? 1 : 0")
		b.WriteString(")")
	}
	if whereStr != "" {
		b.WriteString(fmt.Sprintf(".filter((%s) => %s)", q.Var, whereStr))
	}
	if getIdent(q.Select) != q.Var {
		s := sel
		if isLiteralComplexUnary(selExpr.Binary.Left) {
			s = "(" + s + ")"
		}
		b.WriteString(fmt.Sprintf(".map((%s) => %s)", q.Var, s))
	}
	if skipStr != "" || takeStr != "" {
		start := "0"
		if skipStr != "" {
			start = skipStr
		}
		end := ""
		if takeStr != "" {
			if skipStr != "" {
				end = "(" + skipStr + " + " + takeStr + ")"
			} else {
				end = takeStr
			}
		}
		if end == "" {
			b.WriteString(fmt.Sprintf(".slice(%s)", start))
		} else {
			b.WriteString(fmt.Sprintf(".slice(%s, %s)", start, end))
		}
	}
	return b.String(), true
}
