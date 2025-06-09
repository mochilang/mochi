package javacode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Java source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	helpers map[string]bool
}

// New creates a new Java compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: map[string]bool{}}
}

// Compile generates Java source code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("public class Main {")
	c.indent++

	// Function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Test blocks
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("public static void main(String[] args) throws Exception {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s();", name))
		}
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	c.emitRuntime()
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.use("truthy")
		c.writeln(fmt.Sprintf("if (!Runtime.truthy(%s)) throw new RuntimeException(\"expect failed\");", expr))
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	value := "null"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("Object %s = %s;", name, value))
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	value := "null"
	if s.Value != nil {
		v, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		value = v
	}
	c.writeln(fmt.Sprintf("Object %s = %s;", name, value))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	name := sanitizeName(s.Name)
	value, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", name, value))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeln("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (int %s = (int)%s; %s < (int)%s; %s++) {", name, start, name, end, name))
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	c.use("_iter")
	c.writeln(fmt.Sprintf("for (Object %s : Runtime.iter(%s)) {", name, src))
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := strings.Join(paramNames(fun.Params), ", ")
	c.writeln(fmt.Sprintf("static Object %s(%s) {", name, params))
	c.indent++
	for _, s := range fun.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func paramNames(params []*parser.Param) []string {
	out := make([]string, len(params))
	for i, p := range params {
		out[i] = "Object " + sanitizeName(p.Name)
	}
	return out
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("static void %s() throws Exception {", name))
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(e *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(e.Left)
	if err != nil {
		return "", err
	}
	for _, op := range e.Right {
		r, err := c.compilePostfixExpr(op.Right)
		if err != nil {
			return "", err
		}
		left = fmt.Sprintf("(%s %s %s)", left, op.Op, r)
	}
	return left, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfixExpr(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = fmt.Sprintf("(%s%s)", u.Ops[i], expr)
	}
	return expr, nil
}

func (c *Compiler) compilePostfixExpr(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			c.use("_index")
			expr = fmt.Sprintf("Runtime._index(%s, (int)%s)", expr, idx)
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Selector != nil:
		return strings.Join(append([]string{sanitizeName(p.Selector.Root)}, p.Selector.Tail...), "."), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		c.use("listOf")
		return fmt.Sprintf("Runtime.listOf(%s)", strings.Join(elems, ", ")), nil
	case p.Map != nil:
		pairs := make([]string, 0, len(p.Map.Items)*2)
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs = append(pairs, k, v)
		}
		c.use("mapOf")
		return fmt.Sprintf("Runtime.mapOf(%s)", strings.Join(pairs, ", ")), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Group != nil:
		return c.compileExpr(p.Group)
	default:
		return sanitizeName(p.Selector.Root), nil
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	name := sanitizeName(call.Func)
	switch name {
	case "print":
		c.use("print")
		return fmt.Sprintf("Runtime.print(%s)", strings.Join(args, ", ")), nil
	case "len":
		c.use("_len")
		return fmt.Sprintf("Runtime._len(%s)", strings.Join(args, ", ")), nil
	default:
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	default:
		return "null", fmt.Errorf("invalid literal")
	}
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	var body bytes.Buffer
	if f.ExprBody != nil {
		expr, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		body.WriteString("return ")
		body.WriteString(expr)
		body.WriteString(";")
	} else {
		for _, s := range f.BlockBody {
			if err := c.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	c.use("Fn")
	return fmt.Sprintf("(new Runtime.Fn() { public Object call(Object... args) { %s } })", body.String()), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var parts []string
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			parts = append(parts, res)
			break
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		cond := fmt.Sprintf("%s.equals(%s)", target, pat)
		parts = append(parts, fmt.Sprintf("%s ? %s :", cond, res))
	}
	parts = append(parts, "null")
	return fmt.Sprintf("(%s)", strings.Join(parts, " ")), nil
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

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	switch g.Target {
	case "embedding":
		var text string
		for _, f := range g.Fields {
			if f.Name == "text" {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				text = v
			}
		}
		if text == "" {
			text = "\"\""
		}
		c.use("_genEmbed")
		return fmt.Sprintf("Runtime._genEmbed(%s)", text), nil
	default:
		var prompt string
		for _, f := range g.Fields {
			if f.Name == "prompt" {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				prompt = v
			}
		}
		if prompt == "" {
			prompt = "\"\""
		}
		c.use("_genText")
		return fmt.Sprintf("Runtime._genText(%s)", prompt), nil
	}
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	c.use("_fetch")
	return fmt.Sprintf("Runtime._fetch(%s, null)", url), nil
}

func (c *Compiler) writeln(s string) { c.writeIndent(); c.buf.WriteString(s); c.buf.WriteByte('\n') }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
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
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		return "_" + s
	}
	return s
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	c.buf.WriteByte('\n')
	c.buf.WriteString("class Runtime {\n")
	if c.helpers["Fn"] {
		c.buf.WriteString("    interface Fn { Object call(Object... args); }\n")
	}
	if c.helpers["print"] {
		c.buf.WriteString("    static void print(Object... xs) { for (int i=0;i<xs.length;i++){ System.out.print(xs[i]); if (i+1<xs.length) System.out.print(\" \" ); } System.out.println(); }\n")
	}
	if c.helpers["_len"] {
		c.buf.WriteString("    static int _len(Object v) { if (v instanceof java.util.Collection) return ((java.util.Collection<?>)v).size(); if (v instanceof String) return ((String)v).length(); if (v instanceof Object[]) return ((Object[])v).length; return 0; }\n")
	}
	if c.helpers["_index"] {
		c.buf.WriteString("    static Object _index(Object v, int k) { if (v instanceof java.util.List) { java.util.List<?> l=(java.util.List<?>)v; if (k<0) k=l.size()+k; return l.get(k); } if (v instanceof String) { String s=(String)v; if (k<0) k=s.length()+k; return String.valueOf(s.charAt(k)); } if (v instanceof Object[]) { Object[] a=(Object[])v; if (k<0) k=a.length+k; return a[k]; } return null; }\n")
	}
	if c.helpers["_iter"] {
		c.buf.WriteString("    static Iterable<?> iter(Object v) { if (v instanceof java.util.Map) return ((java.util.Map<?,?>)v).keySet(); if (v instanceof Iterable) return (Iterable<?>)v; if (v instanceof Object[]) return java.util.Arrays.asList((Object[])v); return java.util.Collections.emptyList(); }\n")
	}
	if c.helpers["listOf"] {
		c.buf.WriteString("    static java.util.List<Object> listOf(Object... xs){ return java.util.Arrays.asList(xs); }\n")
	}
	if c.helpers["mapOf"] {
		c.buf.WriteString("    static java.util.Map<String,Object> mapOf(Object... xs){ java.util.Map<String,Object> m=new java.util.HashMap<>(); for(int i=0;i<xs.length;i+=2) m.put(xs[i].toString(), xs[i+1]); return m; }\n")
	}
	if c.helpers["_genText"] {
		c.buf.WriteString("    static String _genText(String prompt){ return prompt; }\n")
	}
	if c.helpers["_genEmbed"] {
		c.buf.WriteString("    static java.util.List<Double> _genEmbed(String text){ java.util.List<Double> l=new java.util.ArrayList<>(); for(char c: text.toCharArray()) l.add((double)c); return l; }\n")
	}
	if c.helpers["_fetch"] {
		c.buf.WriteString("    static Object _fetch(String url,Object opts){ return null; }\n")
	}
	if c.helpers["truthy"] {
		c.buf.WriteString("    static boolean truthy(Object v){ if(v==null) return false; if(v instanceof Boolean) return (Boolean)v; if(v instanceof Number) return ((Number)v).doubleValue()!=0; if(v instanceof String) return !((String)v).isEmpty(); if(v instanceof java.util.Collection) return !((java.util.Collection<?>)v).isEmpty(); if(v instanceof java.util.Map) return !((java.util.Map<?,?>)v).isEmpty(); return true; }\n")
	}
	c.buf.WriteString("}\n")
}
