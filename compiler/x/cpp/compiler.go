//go:build slow

package cpp

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler translates a subset of Mochi to simple C++17 code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	tmp    int
}

// New returns a new compiler instance.
func New() *Compiler { return &Compiler{} }

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("__tmp%d", c.tmp)
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

// Compile converts a parsed Mochi program to C++ source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("#include <iostream>")
	c.writeln("#include <vector>")
	c.writeln("#include <unordered_map>")
	c.writeln("#include <map>")
	c.writeln("#include <algorithm>")
	c.writeln("#include <numeric>")
	c.writeln("")

	c.writeln("template<typename T> void print_val(const T& v){ std::cout << v; }")
	c.writeln("void print_val(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}")
	c.writeln("void print_val(bool b){ std::cout<<(b?\"true\":\"false\"); }")
	c.writeln("void print(){ std::cout<<std::endl; }")
	c.writeln("template<typename First, typename... Rest> void print(const First& first, const Rest&... rest){ print_val(first); if constexpr(sizeof...(rest)>0){ std::cout<<' '; print(rest...); } else { std::cout<<std::endl; }}")
	c.writeln("")

	// first generate function declarations
	for _, st := range p.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("int main() {")
	c.indent++
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	c.writeIndent()
	c.buf.WriteString("auto ")
	c.buf.WriteString(fn.Name)
	c.buf.WriteString("(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString("auto ")
		c.buf.WriteString(p.Name)
	}
	c.buf.WriteString(") {\n")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(st *parser.Statement) error {
	switch {
	case st.Fun != nil:
		lambda, err := c.compileLambda(st.Fun.Params, nil, st.Fun.Body)
		if err != nil {
			return err
		}
		c.writeIndent()
		c.buf.WriteString("auto ")
		c.buf.WriteString(st.Fun.Name)
		c.buf.WriteString(" = ")
		c.buf.WriteString(lambda)
		c.buf.WriteString(";\n")
		return nil
	case st.Let != nil:
		return c.compileLet(st.Let)
	case st.Var != nil:
		return c.compileVar(st.Var)
	case st.Assign != nil:
		return c.compileAssign(st.Assign)
	case st.Return != nil:
		expr, err := c.compileExpr(st.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
		return nil
	case st.If != nil:
		return c.compileIf(st.If)
	case st.While != nil:
		return c.compileWhile(st.While)
	case st.For != nil:
		return c.compileFor(st.For)
	case st.Break != nil:
		c.writeln("break;")
		return nil
	case st.Continue != nil:
		c.writeln("continue;")
		return nil
	case st.Expr != nil:
		expr, err := c.compileExpr(st.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement at %v", st.Pos)
	}
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	c.writeIndent()
	typ, err := c.compileType(st.Type)
	if err != nil {
		return err
	}
	c.buf.WriteString(typ)
	c.buf.WriteByte(' ')
	c.buf.WriteString(st.Name)
	if st.Value != nil {
		expr, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		c.buf.WriteString(" = ")
		c.buf.WriteString(expr)
	} else if st.Type != nil {
		switch typ {
		case "int":
			c.buf.WriteString(" = 0")
		case "std::string":
			c.buf.WriteString(" = \"\"")
		}
	}
	c.buf.WriteString(";\n")
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	c.writeIndent()
	typ, err := c.compileType(st.Type)
	if err != nil {
		return err
	}
	c.buf.WriteString(typ)
	c.buf.WriteByte(' ')
	c.buf.WriteString(st.Name)
	if st.Value != nil {
		expr, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		c.buf.WriteString(" = ")
		c.buf.WriteString(expr)
	} else if st.Type != nil {
		switch typ {
		case "int":
			c.buf.WriteString(" = 0")
		case "std::string":
			c.buf.WriteString(" = \"\"")
		}
	}
	c.buf.WriteString(";\n")
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	c.writeIndent()
	name := st.Name
	for _, idx := range st.Index {
		if idx.Start != nil {
			expr, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			name += "[" + expr + "]"
		}
	}
	for _, fld := range st.Field {
		name += "." + fld.Name
	}
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.buf.WriteString(name + " = " + expr + ";\n")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if st.Else != nil {
		c.writeln("else {")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	if st.RangeEnd != nil {
		startExpr := src
		endExpr, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (int %s = %s; %s < %s; ++%s) {", st.Name, startExpr, st.Name, endExpr, st.Name))
	} else {
		c.writeln(fmt.Sprintf("for (auto %s : %s) {", st.Name, src))
	}
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{left}
	ops := []string{}
	for _, part := range b.Right {
		rhs, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, rhs)
		ops = append(ops, part.Op)
	}

	prec := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}
	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				var combined string
				if ops[i] == "in" {
					combined = fmt.Sprintf("(std::find(%s.begin(), %s.end(), %s) != %s.end())", l, l, r, l)
				} else {
					combined = fmt.Sprintf("(%s %s %s)", l, ops[i], r)
				}
				operands[i] = combined
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary expression")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	x, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		x = fmt.Sprintf("(%s%s)", u.Ops[i], x)
	}
	return x, nil
}

func (c *Compiler) compilePostfix(pf *parser.PostfixExpr) (string, error) {
	base, err := c.compilePrimary(pf.Target)
	if err != nil {
		return "", err
	}
	expr := base
	for _, op := range pf.Ops {
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, s)
			}
			if base == "print" {
				expr = fmt.Sprintf("print(%s)", strings.Join(args, ", "))
			} else if base == "len" && len(args) == 1 {
				expr = fmt.Sprintf("(%s.size())", args[0])
			} else if base == "append" && len(args) == 2 {
				expr = fmt.Sprintf("([&](auto v){ v.push_back(%s); return v; })(%s)", args[1], args[0])
			} else if base == "sum" && len(args) == 1 {
				expr = fmt.Sprintf("std::accumulate(%s.begin(), %s.end(), 0)", args[0], args[0])
			} else if base == "avg" && len(args) == 1 {
				expr = fmt.Sprintf("([&](auto v){ int s=0; for(auto x:v) s+=x; return v.empty()?0:(double)s/v.size(); })(%s)", args[0])
			} else if base == "count" && len(args) == 1 {
				expr = fmt.Sprintf("((int)%s.size())", args[0])
			} else if base == "min" && len(args) == 1 {
				expr = fmt.Sprintf("(*std::min_element(%s.begin(), %s.end()))", args[0], args[0])
			} else if base == "max" && len(args) == 1 {
				expr = fmt.Sprintf("(*std::max_element(%s.begin(), %s.end()))", args[0], args[0])
			} else {
				expr = base + "(" + strings.Join(args, ", ") + ")"
			}
			base = expr
		} else if op.Index != nil {
			if op.Index.Start == nil {
				return "", fmt.Errorf("unsupported empty index")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s[%s]", expr, idx)
		} else if op.Field != nil {
			expr = fmt.Sprintf("%s.%s", expr, op.Field.Name)
		} else if op.Cast != nil {
			t, err := c.compileType(op.Cast.Type)
			if err != nil {
				return "", err
			}
			if t == "int" {
				expr = fmt.Sprintf("std::stoi(%s)", expr)
			} else if t == "std::string" {
				expr = fmt.Sprintf("std::to_string(%s)", expr)
			} else {
				expr = fmt.Sprintf("(%s)(%s)", t, expr)
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems = append(elems, s)
		}
		return "std::vector<int>{" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
		pairs := []string{}
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs = append(pairs, fmt.Sprintf("{%s, %s}", k, v))
		}
		return "std::unordered_map<std::string,int>{" + strings.Join(pairs, ", ") + "}", nil
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
	case p.Call != nil:
		name := p.Call.Func
		args := []string{}
		for _, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, s)
		}
		switch name {
		case "print":
			return fmt.Sprintf("print(%s)", strings.Join(args, ", ")), nil
		case "len":
			if len(args) == 1 {
				return fmt.Sprintf("%s.size()", args[0]), nil
			}
		case "append":
			if len(args) == 2 {
				return fmt.Sprintf("([&](auto v){ v.push_back(%s); return v; })(%s)", args[1], args[0]), nil
			}
		case "sum":
			if len(args) == 1 {
				return fmt.Sprintf("std::accumulate(%s.begin(), %s.end(), 0)", args[0], args[0]), nil
			}
		case "avg":
			if len(args) == 1 {
				return fmt.Sprintf("([&](auto v){ int s=0; for(auto x:v) s+=x; return v.empty()?0:(double)s/v.size(); })(%s)", args[0]), nil
			}
		case "count":
			if len(args) == 1 {
				return fmt.Sprintf("((int)%s.size())", args[0]), nil
			}
		case "min":
			if len(args) == 1 {
				return fmt.Sprintf("(*std::min_element(%s.begin(), %s.end()))", args[0], args[0]), nil
			}
		case "max":
			if len(args) == 1 {
				return fmt.Sprintf("(*std::max_element(%s.begin(), %s.end()))", args[0], args[0]), nil
			}
		}
		return name + "(" + strings.Join(args, ", ") + ")", nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	default:
		return "", fmt.Errorf("unsupported primary at %v", p.Pos)
	}
}

func (c *Compiler) compileMatchExpr(mx *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(mx.Target)
	if err != nil {
		return "", err
	}
	elseExpr := ""
	cases := make([]*parser.MatchCase, 0, len(mx.Cases))
	for _, cs := range mx.Cases {
		if name, ok := c.simpleIdentifier(cs.Pattern); ok && name == "_" {
			e, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			elseExpr = e
		} else {
			cases = append(cases, cs)
		}
	}
	if elseExpr == "" {
		if len(cases) > 0 {
			first, err := c.compileExpr(cases[0].Result)
			if err != nil {
				return "", err
			}
			elseExpr = fmt.Sprintf("decltype(%s){}", first)
		} else {
			elseExpr = "0"
		}
	}
	var buf strings.Builder
	buf.WriteString("([&]() { auto __v = ")
	buf.WriteString(target)
	buf.WriteString("; ")
	for i, cs := range cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if i == 0 {
			buf.WriteString("if (__v == ")
		} else {
			buf.WriteString("else if (__v == ")
		}
		buf.WriteString(pat)
		buf.WriteString(") return ")
		buf.WriteString(res)
		buf.WriteString("; ")
	}
	buf.WriteString("return ")
	buf.WriteString(elseExpr)
	buf.WriteString("; })()")
	return buf.String(), nil
}

func (c *Compiler) simpleIdentifier(e *parser.Expr) (string, bool) {
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

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	if l.Int != nil {
		return fmt.Sprint(*l.Int), nil
	}
	if l.Float != nil {
		return fmt.Sprint(*l.Float), nil
	}
	if l.Bool != nil {
		if bool(*l.Bool) {
			return "true", nil
		}
		return "false", nil
	}
	if l.Str != nil {
		return fmt.Sprintf("\"%s\"", *l.Str), nil
	}
	if l.Null {
		return "nullptr", nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "auto", nil
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int", nil
		case "string":
			return "std::string", nil
		default:
			return *t.Simple, nil
		}
	}
	return "auto", nil
}

// compileFunExpr converts an anonymous function expression to a C++ lambda.
func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	return c.compileLambda(fn.Params, fn.ExprBody, fn.BlockBody)
}

// compileLambda builds a C++ lambda from the given parameters and body.
func (c *Compiler) compileLambda(params []*parser.Param, exprBody *parser.Expr, stmts []*parser.Statement) (string, error) {
	var buf strings.Builder
	buf.WriteString("[=](")
	for i, p := range params {
		if i > 0 {
			buf.WriteString(", ")
		}
		typ, err := c.compileType(p.Type)
		if err != nil {
			return "", err
		}
		buf.WriteString(typ)
		buf.WriteByte(' ')
		buf.WriteString(p.Name)
	}
	buf.WriteString(") {")

	sub := &Compiler{indent: 1, tmp: c.tmp}
	if exprBody != nil {
		e, err := sub.compileExpr(exprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + e + ";")
	} else {
		for _, st := range stmts {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	body := sub.buf.String()
	if body != "" {
		buf.WriteByte('\n')
		buf.WriteString(body)
	}
	buf.WriteString("}")
	c.tmp = sub.tmp
	return buf.String(), nil
}
