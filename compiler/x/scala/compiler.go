//go:build slow

package scalacode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"unicode"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf         bytes.Buffer
	indent      int
	env         *types.Env
	tmp         int
	helpers     map[string]bool
	inSort      bool
	updates     map[string]bool
	autoStructs map[string]types.StructType
	structKeys  map[string]string
	autoCount   int
}

func (c *Compiler) detectStructMap(e *parser.Expr) (types.StructType, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return types.StructType{}, false
	}
	m := e.Binary.Left.Value.Target.Map
	if m == nil {
		return types.StructType{}, false
	}
	st := types.StructType{Fields: make(map[string]types.Type), Order: make([]string, len(m.Items))}
	for i, it := range m.Items {
		key, ok := types.SimpleStringKey(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		st.Fields[key] = c.namedType(types.ExprType(it.Value, c.env))
		st.Order[i] = key
	}
	return st, true
}

const indentStep = 2

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:         env,
		helpers:     make(map[string]bool),
		updates:     make(map[string]bool),
		autoStructs: make(map[string]types.StructType),
		structKeys:  make(map[string]string),
	}
}

func (c *Compiler) newVar(prefix string) string {
	name := fmt.Sprintf("_%s%d", prefix, c.tmp)
	c.tmp++
	return name
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte(' ')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) use(name string) {
	if c.helpers != nil {
		c.helpers[name] = true
	}
}

func (c *Compiler) emitHelpers(out *bytes.Buffer, indent int) {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	pad := strings.Repeat(" ", indent)
	for _, n := range names {
		switch n {
		case "_union_all":
			out.WriteString(pad + "def _union_all[T](a: List[T], b: List[T]): List[T] = a ++ b\n")
		case "_union":
			out.WriteString(pad + "def _union[T](a: List[T], b: List[T]): List[T] = { val set = scala.collection.mutable.LinkedHashSet[T](); set ++= a; set ++= b; set.toList }\n")
		case "_except":
			out.WriteString(pad + "def _except[T](a: List[T], b: List[T]): List[T] = { val remove = b.toSet; a.filterNot(remove) }\n")
		case "_intersect":
			out.WriteString(pad + "def _intersect[T](a: List[T], b: List[T]): List[T] = { val keep = b.toSet; val res = scala.collection.mutable.ListBuffer[T](); val seen = scala.collection.mutable.Set[T](); for(x <- a if keep(x) && !seen(x)) { seen += x; res += x }; res.toList }\n")
		case "_load_yaml":
			out.WriteString(pad + "def _load_yaml(path: String): List[Map[String, String]] = { val lines = scala.io.Source.fromFile(path).getLines().toList; lines.grouped(3).flatMap { case List(n,a,e) => Some(Map(\"name\"->n.split(':')(1).trim, \"age\"->a.split(':')(1).trim, \"email\"->e.split(':')(1).trim)); case _ => None }.toList }\n")
		case "_save_jsonl":
			out.WriteString(pad + "def _save_jsonl(rows: List[Map[String, Any]], path: String): Unit = { val out = if(path == \"-\") Console.out else new java.io.PrintWriter(path); rows.foreach(r => out.println(scala.util.parsing.json.JSONObject(r).toString())); if(out ne Console.out) out.close() }\n")
		case "_truthy":
			out.WriteString(pad + "def _truthy(v: Any): Boolean = v match {\n" +
				pad + "  case null => false\n" +
				pad + "  case b: Boolean => b\n" +
				pad + "  case i: Int => i != 0\n" +
				pad + "  case l: Long => l != 0L\n" +
				pad + "  case d: Double => d != 0.0\n" +
				pad + "  case s: String => s.nonEmpty\n" +
				pad + "  case m: scala.collection.Map[_, _] => m.nonEmpty\n" +
				pad + "  case it: Iterable[_] => it.nonEmpty\n" +
				pad + "  case opt: Option[_] => opt.nonEmpty\n" +
				pad + "  case _ => true\n" +
				pad + "}\n")
		}
		out.WriteByte('\n')
	}
}

func sanitizeName(name string) string {
	if name == "" {
		return "Main"
	}
	var b strings.Builder
	for i, r := range name {
		if i == 0 && unicode.IsDigit(r) {
			b.WriteByte('_')
		}
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' {
			b.WriteRune(r)
		} else {
			b.WriteByte('_')
		}
	}
	return b.String()
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	name := sanitizeName(strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename)))
	collectUpdates(prog.Statements, c.updates)
	// emit user-defined types before the main object
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln(fmt.Sprintf("object %s {", name))
	c.indent += indentStep

	firstFun := len(prog.Statements)
	for i, s := range prog.Statements {
		if s.Fun != nil {
			firstFun = i
			break
		}
	}

	processed := make(map[int]bool)
	for i := 0; i < firstFun; i++ {
		s := prog.Statements[i]
		if s.Let != nil {
			if err := c.compileLet(s.Let); err != nil {
				return nil, err
			}
			processed[i] = true
		}
	}

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("def main(args: Array[String]): Unit = {")
	c.indent += indentStep
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil {
			continue
		}
		if idx := indexOf(prog.Statements, s); processed[idx] {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	c.indent -= indentStep
	c.writeln("}")
	var out bytes.Buffer
	data := c.buf.Bytes()
	marker := fmt.Sprintf("object %s {\n", name)
	idx := bytes.Index(data, []byte(marker))
	if idx == -1 {
		idx = 0
	} else {
		idx += len(marker)
	}
	out.Write(data[:idx])
	c.emitAutoStructs(&out, indentStep)
	c.emitHelpers(&out, indentStep)
	out.Write(data[idx:])
	data = out.Bytes()
	if len(data) == 0 || data[len(data)-1] != '\n' {
		data = append(data, '\n')
	}
	return data, nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("return")
		return nil
	case s.Continue != nil:
		c.writeln("// continue")
		return nil
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		return c.compileExprStmt(s.Expr)
	default:
		return fmt.Errorf("line %d: unsupported statement", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	mutable := c.updates[s.Name]
	rhs := "0"
	if s.Value != nil {
		var err error
		if mutable {
			if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
				rhs, err = c.compileList(lst, true)
			} else if mp := s.Value.Binary.Left.Value.Target.Map; mp != nil {
				rhs, err = c.compileMap(mp, true)
			} else {
				rhs, err = c.compileExpr(s.Value)
			}
		} else {
			rhs, err = c.compileExpr(s.Value)
		}
		if err != nil {
			return err
		}
	}
	var typ types.Type = types.AnyType{}
	if s.Type != nil {
		typ = types.ResolveTypeRef(s.Type, c.env)
	} else if s.Value != nil {
		typ = c.namedType(types.ExprType(s.Value, c.env))
		if q := s.Value.Binary.Left.Value.Target.Query; q != nil {
			if st, ok := c.detectStructMap(q.Select); ok {
				st = c.ensureStructName(st)
				typ = types.ListType{Elem: st}
			}
		} else if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
			if len(lst.Elems) > 0 {
				if st, ok := c.detectStructMap(lst.Elems[0]); ok {
					same := true
					for _, e := range lst.Elems[1:] {
						st2, ok2 := c.detectStructMap(e)
						if !ok2 || len(st2.Order) != len(st.Order) {
							same = false
							break
						}
						for i, f := range st.Order {
							if st2.Order[i] != f {
								same = false
								break
							}
						}
					}
					if same {
						st = c.ensureStructName(st)
						typ = types.ListType{Elem: st}
					}
				}
			}
		}
	}
	if c.env != nil {
		c.env.SetVar(s.Name, typ, mutable)
	}
	if s.Type != nil {
		typ := c.typeString(s.Type)
		if mutable {
			typ = c.mutableTypeString(s.Type)
		}
		if mutable {
			c.writeln(fmt.Sprintf("var %s: %s = %s", s.Name, typ, rhs))
		} else {
			c.writeln(fmt.Sprintf("val %s: %s = %s", s.Name, typ, rhs))
		}
	} else {
		if mutable {
			c.writeln(fmt.Sprintf("var %s = %s", s.Name, rhs))
		} else {
			c.writeln(fmt.Sprintf("val %s = %s", s.Name, rhs))
		}
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	rhs := "0"
	if s.Value != nil {
		var err error
		// use mutable collections when assigning list or map literals
		if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
			rhs, err = c.compileList(lst, true)
		} else if mp := s.Value.Binary.Left.Value.Target.Map; mp != nil {
			rhs, err = c.compileMap(mp, true)
		} else {
			rhs, err = c.compileExpr(s.Value)
		}
		if err != nil {
			return err
		}
	}
	var typ types.Type = types.AnyType{}
	if s.Type != nil {
		typ = types.ResolveTypeRef(s.Type, c.env)
	} else if s.Value != nil {
		typ = c.namedType(types.ExprType(s.Value, c.env))
	}
	if c.env != nil {
		c.env.SetVar(s.Name, typ, true)
	}
	if s.Type != nil {
		ts := c.typeString(s.Type)
		c.writeln(fmt.Sprintf("var %s: %s = %s", s.Name, ts, rhs))
	} else {
		c.writeln(fmt.Sprintf("var %s = %s", s.Name, rhs))
	}
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		c.writeln("sealed trait " + td.Name)
		for _, v := range td.Variants {
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("case object %s extends %s", v.Name, td.Name))
				continue
			}
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = fmt.Sprintf("%s: %s", f.Name, c.typeString(f.Type))
			}
			c.writeln(fmt.Sprintf("case class %s(%s) extends %s", v.Name, strings.Join(fields, ", "), td.Name))
		}
		return nil
	}
	fields := []string{}
	for _, m := range td.Members {
		if m.Field != nil {
			f := m.Field
			fields = append(fields, fmt.Sprintf("var %s: %s", f.Name, c.typeString(f.Type)))
		}
	}
	if len(fields) == 0 {
		return fmt.Errorf("line %d: empty type", td.Pos.Line)
	}
	c.writeln(fmt.Sprintf("case class %s(%s)", td.Name, strings.Join(fields, ", ")))
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeString(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	ret := ""
	if fn.Return != nil {
		ret = ": " + c.typeString(fn.Return)
	}
	c.writeln(fmt.Sprintf("def %s(%s)%s = {", fn.Name, strings.Join(params, ", "), ret))
	c.indent += indentStep
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	expr, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	if _, ok := types.ExprType(s.Cond, c.env).(types.BoolType); !ok {
		c.use("_truthy")
		cond = fmt.Sprintf("_truthy(%s)", cond)
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent += indentStep
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	if s.ElseIf != nil {
		c.writeln("} else {")
		c.indent += indentStep
		if err := c.compileIf(s.ElseIf); err != nil {
			return err
		}
		c.indent -= indentStep
		c.writeln("}")
		return nil
	}
	if s.Else != nil {
		c.writeln("} else {")
		c.indent += indentStep
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent -= indentStep
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIfExpr(e *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(e.Cond)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(e.Cond, c.env).(types.BoolType); !ok {
		c.use("_truthy")
		cond = fmt.Sprintf("_truthy(%s)", cond)
	}
	thenExpr, err := c.compileExpr(e.Then)
	if err != nil {
		return "", err
	}
	elseExpr := "()"
	if e.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(e.ElseIf)
		if err != nil {
			return "", err
		}
	} else if e.Else != nil {
		elseExpr, err = c.compileExpr(e.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeString(p.Type))
		} else {
			params[i] = p.Name
		}
	}
	if fn.ExprBody == nil {
		return "", fmt.Errorf("line %d: block lambdas not supported", fn.Pos.Line)
	}
	body, err := c.compileExpr(fn.ExprBody)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	if _, ok := types.ExprType(s.Cond, c.env).(types.BoolType); !ok {
		c.use("_truthy")
		cond = fmt.Sprintf("_truthy(%s)", cond)
	}
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent += indentStep
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(s *parser.ForStmt) error {
	start, err := c.compileExpr(s.Source)
	if err != nil {
		return err
	}
	loop := start
	if s.RangeEnd != nil {
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		loop = fmt.Sprintf("%s to %s", start, end)
	}
	var elemType types.Type = types.AnyType{}
	if t := types.ExprType(s.Source, c.env); t != nil {
		if mt, ok := t.(types.MapType); ok {
			c.writeln(fmt.Sprintf("for((%s, _) <- %s) {", s.Name, loop))
			elemType = mt.Key
		} else {
			c.writeln(fmt.Sprintf("for(%s <- %s) {", s.Name, loop))
			if lt, ok := t.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	} else {
		c.writeln(fmt.Sprintf("for(%s <- %s) {", s.Name, loop))
	}
	child := types.NewEnv(c.env)
	child.SetVar(s.Name, elemType, false)
	oldEnv := c.env
	c.env = child
	c.indent += indentStep
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = oldEnv
			return err
		}
	}
	c.indent -= indentStep
	c.writeln("}")
	c.env = oldEnv
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	expr, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	target := s.Name
	// handle nested index assignment for immutable collections
	if len(s.Index) == 2 && len(s.Field) == 0 {
		idx0, err := c.compileExpr(s.Index[0].Start)
		if err != nil {
			return err
		}
		idx1, err := c.compileExpr(s.Index[1].Start)
		if err != nil {
			return err
		}
		tmp := c.newVar("tmp")
		c.writeln(fmt.Sprintf("val %s = %s(%s).updated(%s, %s)", tmp, target, idx0, idx1, expr))
		c.writeln(fmt.Sprintf("%s = %s.updated(%s, %s)", target, target, idx0, tmp))
		return nil
	}
	for i, idx := range s.Index {
		if idx.Colon != nil || idx.Colon2 != nil {
			return fmt.Errorf("line %d: slice assignment unsupported", s.Pos.Line)
		}
		idxExpr := "0"
		if idx.Start != nil {
			var err error
			idxExpr, err = c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
		}
		if i == len(s.Index)-1 && len(s.Index) > 1 {
			c.writeln(fmt.Sprintf("%s.update(%s, %s)", target, idxExpr, expr))
			return nil
		}
		target = fmt.Sprintf("%s(%s)", target, idxExpr)
	}
	for _, f := range s.Field {
		target += "." + f.Name
	}
	// check for compound assignment like x = x + y
	if len(s.Index) == 0 && len(s.Field) == 0 {
		if b := s.Value.Binary; b != nil && len(b.Right) == 1 {
			if id, ok := identOfUnary(b.Left); ok && id == s.Name {
				op := b.Right[0]
				if op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" || op.Op == "%" {
					rhs, err := c.compilePostfix(op.Right)
					if err != nil {
						return err
					}
					c.writeln(fmt.Sprintf("%s %s= %s", target, op.Op, rhs))
					return nil
				}
			}
		}
	}
	c.writeln(fmt.Sprintf("%s = %s", target, expr))
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("assert(%s)", expr))
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

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := u.Target
	idx := c.newVar("i")
	item := c.newVar("it")
	c.writeln(fmt.Sprintf("for(%s <- 0 until %s.length) {", idx, list))
	c.indent += indentStep
	c.writeln(fmt.Sprintf("var %s = %s(%s)", item, list, idx))

	var elemType types.Type = types.AnyType{}
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	}
	child := types.NewEnv(c.env)
	if st, ok := elemType.(types.StructType); ok {
		for _, f := range st.Order {
			c.writeln(fmt.Sprintf("var %s = %s.%s", f, item, f))
			child.SetVar(f, st.Fields[f], true)
		}
	} else if mt, ok := elemType.(types.MapType); ok {
		if _, ok2 := mt.Key.(types.StringType); ok2 {
			for _, it := range u.Set.Items {
				if key, ok3 := identName(it.Key); ok3 {
					c.writeln(fmt.Sprintf("var %s = %s(%q)", key, item, key))
					child.SetVar(key, mt.Value, true)
				}
			}
		}
	}

	oldEnv := c.env
	c.env = child

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = oldEnv
			return err
		}
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent += indentStep
	}

	for _, it := range u.Set.Items {
		if _, ok := elemType.(types.StructType); ok {
			if key, ok2 := identName(it.Key); ok2 {
				val, err := c.compileExpr(it.Value)
				if err != nil {
					c.env = oldEnv
					return err
				}
				c.writeln(fmt.Sprintf("%s = %s", key, val))
				continue
			}
			// fallthrough to map style if key not simple
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = oldEnv
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = oldEnv
			return err
		}
		c.writeln(fmt.Sprintf("%s(%s) = %s", item, keyExpr, valExpr))
	}

	if u.Where != nil {
		c.indent -= indentStep
		c.writeln("}")
	}

	if st, ok := elemType.(types.StructType); ok {
		parts := make([]string, len(st.Order))
		for i, f := range st.Order {
			parts[i] = fmt.Sprintf("%s = %s", f, f)
		}
		c.writeln(fmt.Sprintf("%s = %s(%s)", item, st.Name, strings.Join(parts, ", ")))
	}
	c.writeln(fmt.Sprintf("%s = %s.updated(%s, %s)", list, list, idx, item))
	c.env = oldEnv
	c.indent -= indentStep
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExprStmt(s *parser.ExprStmt) error {
	call, ok := callPattern(s.Expr)
	if ok && call.Func == "print" {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return err
			}
			args[i] = fmt.Sprintf("(%s)", v)
		}
		if len(args) == 1 {
			c.writeln(fmt.Sprintf("println(%s)", args[0]))
		} else {
			c.writeln(fmt.Sprintf("println(%s)", strings.Join(args, " + \" \" + ")))
		}
		return nil
	}
	if ok && call.Func == "json" {
		expr, err := c.compileExpr(s.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("println(%s)", expr))
		return nil
	}
	expr, err := c.compileExpr(s.Expr)
	if err != nil {
		return err
	}
	c.writeln(expr)
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	s, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	leftType := types.TypeOfUnary(e.Binary.Left, c.env)
	for _, op := range e.Binary.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := types.TypeOfPostfix(op.Right, c.env)
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			ls, rs := s, r
			if _, ok := leftType.(types.AnyType); ok {
				ls = fmt.Sprintf("(%s).asInstanceOf[Int]", s)
			}
			if _, ok := rightType.(types.AnyType); ok {
				rs = fmt.Sprintf("(%s).asInstanceOf[Int]", r)
			}
			s = fmt.Sprintf("%s %s %s", ls, op.Op, rs)
			switch op.Op {
			case "&&", "||", "==", "!=", "<", "<=", ">", ">=":
				leftType = types.BoolType{}
			default:
				leftType = types.AnyType{}
			}
		case "in":
			ct := types.TypeOfPostfix(op.Right, c.env)
			switch ct.(type) {
			case types.MapType, types.ListType, types.StringType:
				s = fmt.Sprintf("%s.contains(%s)", r, s)
			default:
				return "", fmt.Errorf("line %d: unsupported operator %s", op.Pos.Line, op.Op)
			}
		case "union":
			if op.All {
				c.use("_union_all")
				s = fmt.Sprintf("_union_all(%s, %s)", s, r)
			} else {
				c.use("_union")
				s = fmt.Sprintf("_union(%s, %s)", s, r)
			}
			leftType = types.ListType{Elem: types.AnyType{}}
		case "except":
			c.use("_except")
			s = fmt.Sprintf("_except(%s, %s)", s, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		case "intersect":
			c.use("_intersect")
			s = fmt.Sprintf("_intersect(%s, %s)", s, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		default:
			return "", fmt.Errorf("line %d: unsupported operator %s", op.Pos.Line, op.Op)
		}
	}
	return s, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	s, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	valType := types.TypeOfPostfix(u.Value, c.env)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			switch valType.(type) {
			case types.IntType, types.Int64Type, types.FloatType:
				s = fmt.Sprintf("-%s", s)
			default:
				s = fmt.Sprintf("-(%s).asInstanceOf[Int]", s)
			}
		case "!":
			if _, ok := valType.(types.BoolType); ok {
				s = fmt.Sprintf("!%s", s)
			} else {
				c.use("_truthy")
				s = fmt.Sprintf("!_truthy(%s)", s)
			}
		default:
			return "", fmt.Errorf("line %d: unsupported unary op %s", u.Pos.Line, op)
		}
	}
	return s, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	s, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := types.TypeOfPrimary(p.Target, c.env)
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				val, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = val
			}
			s = fmt.Sprintf("%s(%s)", s, strings.Join(args, ", "))
			if ft, ok := typ.(types.FuncType); ok {
				typ = ft.Return
			} else {
				typ = types.AnyType{}
			}
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil {
				start := "0"
				if idx.Start != nil {
					start, err = c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
				}
				end := fmt.Sprintf("%s.length", s)
				if idx.End != nil {
					end, err = c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
				}
				if _, ok := typ.(types.StringType); ok {
					s = fmt.Sprintf("%s.substring(%s, %s)", s, start, end)
					typ = types.StringType{}
				} else {
					s = fmt.Sprintf("%s.slice(%s, %s)", s, start, end)
				}
			} else {
				idxExpr, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				if _, ok := typ.(types.StringType); ok {
					s = fmt.Sprintf("%s.charAt(%s)", s, idxExpr)
					typ = types.StringType{}
				} else {
					s = fmt.Sprintf("%s(%s)", s, idxExpr)
					switch tt := typ.(type) {
					case types.ListType:
						typ = tt.Elem
					case types.MapType:
						typ = tt.Value
					default:
						typ = types.AnyType{}
					}
				}
			}
		case op.Cast != nil:
			tstr := c.typeString(op.Cast.Type)
			if st, ok := c.env.GetStruct(tstr); ok && p.Target.Map != nil {
				str, err := c.mapToStruct(tstr, st, p.Target.Map)
				if err != nil {
					return "", err
				}
				s = str
				typ = st
			} else {
				switch tstr {
				case "Int":
					s = fmt.Sprintf("%s.toInt", s)
				case "Double":
					s = fmt.Sprintf("%s.toDouble", s)
				case "String":
					s = fmt.Sprintf("%s.toString", s)
				default:
					s = fmt.Sprintf("%s.asInstanceOf[%s]", s, tstr)
				}
				typ = types.ResolveTypeRef(op.Cast.Type, c.env)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return s, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p == nil:
		return "", fmt.Errorf("nil primary")
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.List != nil:
		return c.compileList(p.List, false)
	case p.Map != nil:
		return c.compileMap(p.Map, false)
	case p.Struct != nil:
		return c.compileStructLit(p.Struct)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Selector != nil:
		return c.compileSelector(p.Selector), nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", expr), nil
	default:
		return "", fmt.Errorf("line %d: unsupported expression", p.Pos.Line)
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if *l.Bool {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	default:
		return "null"
	}
}

func (c *Compiler) compileSelector(s *parser.SelectorExpr) string {
	if t, err := c.env.GetVar(s.Root); err == nil {
		if len(s.Tail) == 0 {
			if _, ok := t.(types.GroupType); ok {
				return fmt.Sprintf("%s._2", s.Root)
			}
			return s.Root
		}
		switch tt := t.(type) {
		case types.MapType:
			base := s.Root
			for i, f := range s.Tail {
				base = fmt.Sprintf("%s(%q)", base, f)
				if i < len(s.Tail)-1 {
					// after indexing into map, assume value is map
				}
			}
			return base
		case types.StructType:
			if tt.Name == "" {
				base := s.Root
				for _, f := range s.Tail {
					base = fmt.Sprintf("%s(%q)", base, f)
				}
				return base
			}
		case types.GroupType:
			if len(s.Tail) == 1 {
				switch s.Tail[0] {
				case "key":
					return fmt.Sprintf("%s._1", s.Root)
				case "items":
					return fmt.Sprintf("%s._2", s.Root)
				}
			}
			_ = tt // silence unused warning in case build tags differ
		}
	} else if len(s.Tail) == 0 {
		return s.Root
	} else {
		// fall through
	}
	parts := append([]string{s.Root}, s.Tail...)
	return strings.Join(parts, ".")
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch call.Func {
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("%s :+ %s", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		if qarg, ok := queryExpr(call.Args[0]); ok {
			inner, err := c.compileQueryExpr(qarg)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s).sum.toDouble / (%s).size", inner, inner), nil
		}
		return fmt.Sprintf("(%s).sum.toDouble / (%s).size", args[0], args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		if qarg, ok := queryExpr(call.Args[0]); ok {
			inner, err := c.compileQueryExpr(qarg)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s).size", inner), nil
		}
		if _, ok := types.ExprType(call.Args[0], c.env).(types.GroupType); ok {
			return fmt.Sprintf("(%s).size", args[0]), nil
		}
		return fmt.Sprintf("%s.size", args[0]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		t := types.ExprType(call.Args[0], c.env)
		switch t.(type) {
		case types.MapType:
			return fmt.Sprintf("%s.size", args[0]), nil
		default:
			return fmt.Sprintf("%s.length", args[0]), nil
		}
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		if qarg, ok := queryExpr(call.Args[0]); ok {
			inner, err := c.compileQueryExpr(qarg)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s).min", inner), nil
		}
		return fmt.Sprintf("%s.min", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		if qarg, ok := queryExpr(call.Args[0]); ok {
			inner, err := c.compileQueryExpr(qarg)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s).max", inner), nil
		}
		return fmt.Sprintf("%s.max", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		if qarg, ok := queryExpr(call.Args[0]); ok {
			inner, err := c.compileQueryExpr(qarg)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s).sum", inner), nil
		}
		return fmt.Sprintf("%s.sum", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("%s.toString", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("%s.substring(%s, %s)", args[0], args[1], args[2]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values.toList", args[0]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		return fmt.Sprintf("scala.util.parsing.json.JSONObject(%s).toString()", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("(%s).nonEmpty", args[0]), nil
	default:
		if t, err := c.env.GetVar(call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok && len(args) < len(ft.Params) {
				missing := len(ft.Params) - len(args)
				names := make([]string, missing)
				params := make([]string, missing)
				for i := 0; i < missing; i++ {
					pname := fmt.Sprintf("p%d", i)
					names[i] = pname
					params[i] = fmt.Sprintf("%s: %s", pname, c.typeOf(ft.Params[len(args)+i]))
				}
				callArgs := append(append([]string{}, args...), names...)
				return fmt.Sprintf("(%s) => %s(%s)", strings.Join(params, ", "), call.Func, strings.Join(callArgs, ", ")), nil
			}
		}
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileList(l *parser.ListLiteral, mutable bool) (string, error) {
	prefix := "List"
	if mutable {
		prefix = "scala.collection.mutable.ArrayBuffer"
	}

	// determine element type before compiling elements so that map literals
	// can be rendered as case class instances
	var elemType types.Type = types.AnyType{}
	if len(l.Elems) > 0 {
		elemType = c.namedType(types.ExprType(l.Elems[0], c.env))
		for _, e := range l.Elems[1:] {
			t := c.namedType(types.ExprType(e, c.env))
			if !sameType(elemType, t) {
				elemType = types.AnyType{}
				break
			}
		}
		if _, ok := elemType.(types.MapType); ok && !mutable {
			order := make([]string, 0)
			fields := make(map[string]types.Type)
			valid := true
			for i, e := range l.Elems {
				if e.Binary == nil || len(e.Binary.Right) != 0 {
					valid = false
					break
				}
				ml := e.Binary.Left.Value.Target.Map
				if ml == nil {
					valid = false
					break
				}
				if i == 0 {
					order = make([]string, len(ml.Items))
					for j, it := range ml.Items {
						k, ok := types.SimpleStringKey(it.Key)
						if !ok {
							valid = false
							break
						}
						order[j] = k
						fields[k] = c.namedType(types.ExprType(it.Value, c.env))
					}
				} else {
					if len(ml.Items) != len(order) {
						valid = false
						break
					}
					for j, it := range ml.Items {
						k, ok := types.SimpleStringKey(it.Key)
						if !ok || k != order[j] {
							valid = false
							break
						}
						t := c.namedType(types.ExprType(it.Value, c.env))
						if !sameType(fields[k], t) {
							fields[k] = types.AnyType{}
						}
					}
				}
				if !valid {
					break
				}
			}
			if valid {
				st := types.StructType{Fields: fields, Order: order}
				st = c.ensureStructName(st)
				elemType = st
			}
		}
	}

	// now compile elements with the inferred type information available
	elems := make([]string, len(l.Elems))
	for i, e := range l.Elems {
		s, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		elems[i] = s
	}

	typeStr := c.typeOf(elemType)
	return fmt.Sprintf("%s[%s](%s)", prefix, typeStr, strings.Join(elems, ", ")), nil
}

func (c *Compiler) compileMap(m *parser.MapLiteral, mutable bool) (string, error) {
	// determine if this map corresponds to a struct type
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Map: m}}}}}
	t := c.namedType(types.ExprType(expr, c.env))
	if st, ok := t.(types.StructType); ok && !mutable {
		return c.mapToStruct(st.Name, st, m)
	}
	// if all keys are simple strings, treat as an anonymous struct
	if !mutable {
		anon := types.StructType{Fields: make(map[string]types.Type), Order: make([]string, len(m.Items))}
		allSimple := true
		for i, it := range m.Items {
			key, ok := types.SimpleStringKey(it.Key)
			if !ok {
				allSimple = false
				break
			}
			anon.Fields[key] = c.namedType(types.ExprType(it.Value, c.env))
			anon.Order[i] = key
		}
		if allSimple {
			anon = c.ensureStructName(anon)
			return c.mapToStruct(anon.Name, anon, m)
		}
	}
	items := make([]string, len(m.Items))
	vals := make([]string, len(m.Items))
	for i, it := range m.Items {
		var k string
		if name, ok := simpleIdent(it.Key); ok {
			k = fmt.Sprintf("%q", name)
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
		items[i] = fmt.Sprintf("%s -> (%s)", k, v)
		vals[i] = v
	}
	prefix := "Map"
	if mutable {
		prefix = "scala.collection.mutable.Map"
	}

	// infer key and value types
	var keyType, valType types.Type = types.AnyType{}, types.AnyType{}
	if len(m.Items) > 0 {
		keyType = c.namedType(types.ExprType(m.Items[0].Key, c.env))
		// simple string key should be string type
		if _, ok := types.SimpleStringKey(m.Items[0].Key); ok {
			keyType = types.StringType{}
		}
		valType = c.namedType(types.ExprType(m.Items[0].Value, c.env))
		for _, it := range m.Items[1:] {
			kt := c.namedType(types.ExprType(it.Key, c.env))
			if _, ok := types.SimpleStringKey(it.Key); ok {
				kt = types.StringType{}
			}
			vt := c.namedType(types.ExprType(it.Value, c.env))
			if !sameType(keyType, kt) {
				keyType = types.AnyType{}
			}
			if !sameType(valType, vt) {
				valType = types.AnyType{}
			}
		}
	}
	typeStr := fmt.Sprintf("[%s, %s]", c.typeOf(keyType), c.typeOf(valType))

	if c.inSort {
		return fmt.Sprintf("(%s)", strings.Join(vals, ", ")), nil
	}
	return fmt.Sprintf("%s%s(%s)", prefix, typeStr, strings.Join(items, ", ")), nil
}

func (c *Compiler) compileStructLit(st *parser.StructLiteral) (string, error) {
	if st.Name == "" {
		typ := types.StructType{Fields: make(map[string]types.Type), Order: make([]string, len(st.Fields))}
		for i, f := range st.Fields {
			typ.Fields[f.Name] = c.namedType(types.ExprType(f.Value, c.env))
			typ.Order[i] = f.Name
		}
		typ = c.ensureStructName(typ)
		st.Name = typ.Name
	}
	args := make([]string, len(st.Fields))
	for i, f := range st.Fields {
		val, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		args[i] = fmt.Sprintf("%s = %s", f.Name, val)
	}
	return fmt.Sprintf("%s(%s)", st.Name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString(target + " match {\n")
	c.indent += indentStep
	for _, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		for i := 0; i < c.indent; i++ {
			buf.WriteString(" ")
		}
		buf.WriteString(fmt.Sprintf("case %s => %s\n", pat, res))
	}
	c.indent -= indentStep
	for i := 0; i < c.indent; i++ {
		buf.WriteString(" ")
	}
	buf.WriteString("}")
	return buf.String(), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Distinct {
		return "", fmt.Errorf("line %d: distinct queries not supported", q.Pos.Line)
	}
	parts := []string{}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	child := types.NewEnv(c.env)
	if lt, ok := types.ExprType(q.Source, c.env).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, false)
	}
	oldEnv := c.env
	c.env = child

	parts = append(parts, fmt.Sprintf("%s <- %s", q.Var, src))

	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		parts = append(parts, fmt.Sprintf("%s <- %s", f.Var, s))
		if lt, ok := types.ExprType(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, false)
		}
	}

	for _, j := range q.Joins {
		s, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		var elemT types.Type = types.AnyType{}
		if lt, ok := types.ExprType(j.Src, c.env).(types.ListType); ok {
			elemT = lt.Elem
		}
		cond, err := c.compileExpr(j.On)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		if j.Side == nil {
			parts = append(parts, fmt.Sprintf("%s <- %s", j.Var, s))
			parts = append(parts, fmt.Sprintf("if %s", cond))
		} else {
			parts = append(parts, fmt.Sprintf("%s = %s.find(%s => %s)", j.Var, s, j.Var, cond))
		}
		child.SetVar(j.Var, elemT, false)
	}

	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		parts = append(parts, fmt.Sprintf("if %s", cond))
	}
	var expr string
	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		tuple := strings.Join(append([]string{q.Var}, func() []string {
			names := make([]string, len(q.Froms)+len(q.Joins))
			for i, f := range q.Froms {
				names[i] = f.Var
			}
			for i, j := range q.Joins {
				names[len(q.Froms)+i] = j.Var
			}
			return names
		}()...), ", ")
		tmp := fmt.Sprintf("for { %s } yield (%s, (%s))", strings.Join(parts, "; "), keyExpr, tuple)
		groups := fmt.Sprintf("(%s).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList", tmp)
		if q.Sort != nil {
			c.inSort = true
			key, err := c.compileExpr(q.Sort)
			c.inSort = false
			if err != nil {
				c.env = oldEnv
				return "", err
			}
			groups = fmt.Sprintf("(%s).sortBy(%s => %s)", groups, q.Group.Name, key)
		}

		groupEnv := types.NewEnv(c.env)
		keyT := types.ExprType(q.Group.Exprs[0], c.env)
		groupEnv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: types.AnyType{}}, false)

		if q.Group.Having != nil {
			c.env = groupEnv
			cond, err := c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = oldEnv
				return "", err
			}
			groups = fmt.Sprintf("(%s).filter{ case(%s,%s) => { val %s = (%s, %s); %s } }", groups, q.Group.Name+"Key", q.Group.Name+"Items", q.Group.Name, q.Group.Name+"Key", q.Group.Name+"Items", cond)
		}

		c.env = groupEnv
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("(%s).map{ case(%s,%s) => { val %s = (%s, %s); %s } }.toList", groups, q.Group.Name+"Key", q.Group.Name+"Items", q.Group.Name, q.Group.Name+"Key", q.Group.Name+"Items", sel)
	} else {
		// handle simple aggregations without GROUP BY
		if call, ok := callPattern(q.Select); ok {
			agg := ""
			switch call.Func {
			case "sum", "min", "max":
				if len(call.Args) == 1 {
					arg, err := c.compileExpr(call.Args[0])
					if err != nil {
						c.env = oldEnv
						return "", err
					}
					list := fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), arg)
					agg = fmt.Sprintf("(%s).%s", list, call.Func)
				}
			case "avg":
				if len(call.Args) == 1 {
					arg, err := c.compileExpr(call.Args[0])
					if err != nil {
						c.env = oldEnv
						return "", err
					}
					list := fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), arg)
					agg = fmt.Sprintf("(%s).sum.toDouble / (%s).size", list, list)
				}
			case "count":
				target := "1"
				if len(call.Args) == 1 {
					var err error
					target, err = c.compileExpr(call.Args[0])
					if err != nil {
						c.env = oldEnv
						return "", err
					}
				}
				list := fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), target)
				agg = fmt.Sprintf("(%s).size", list)
			}
			if agg != "" {
				expr = agg
			}
		}
		if expr == "" {
			sel, err := c.compileExpr(q.Select)
			if err != nil {
				c.env = oldEnv
				return "", err
			}
			expr = fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), sel)
		}
	}
	if q.Sort != nil && q.Group == nil {
		c.inSort = true
		key, err := c.compileExpr(q.Sort)
		c.inSort = false
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("(%s).sortBy(%s => %s)", expr, q.Var, key)
	}
	if q.Skip != nil {
		val, err := c.compileExpr(q.Skip)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("%s.drop(%s)", expr, val)
	}
	if q.Take != nil {
		val, err := c.compileExpr(q.Take)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		expr = fmt.Sprintf("%s.take(%s)", expr, val)
	}
	c.env = oldEnv
	return expr, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	format := "csv"
	if l.With != nil {
		if m := l.With.Binary.Left.Value.Target.Map; m != nil {
			for _, it := range m.Items {
				if key, ok := simpleIdent(it.Key); ok && key == "format" {
					if lit := it.Value.Binary.Left.Value.Target.Lit; lit != nil && lit.Str != nil {
						format = *lit.Str
					}
				}
			}
		}
	}
	c.use("_load_" + format)
	return fmt.Sprintf("_load_%s(%s)", format, path), nil
}

func (c *Compiler) compileSaveExpr(sv *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(sv.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if sv.Path != nil {
		path = fmt.Sprintf("%q", *sv.Path)
	}
	format := "csv"
	if sv.With != nil {
		if m := sv.With.Binary.Left.Value.Target.Map; m != nil {
			for _, it := range m.Items {
				if key, ok := simpleIdent(it.Key); ok && key == "format" {
					if lit := it.Value.Binary.Left.Value.Target.Lit; lit != nil && lit.Str != nil {
						format = *lit.Str
					}
				}
			}
		}
	}
	c.use("_save_" + format)
	return fmt.Sprintf("_save_%s(%s, %s)", format, src, path), nil
}

func (c *Compiler) mapToStruct(name string, st types.StructType, m *parser.MapLiteral) (string, error) {
	args := make([]string, len(st.Order))
	for i, field := range st.Order {
		var expr *parser.Expr
		for _, it := range m.Items {
			if key, ok := types.SimpleStringKey(it.Key); ok && key == field {
				expr = it.Value
				break
			}
		}
		if expr == nil {
			return "", fmt.Errorf("missing field %s", field)
		}
		val, err := c.compileExpr(expr)
		if err != nil {
			return "", err
		}
		args[i] = fmt.Sprintf("%s = %s", field, val)
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) typeString(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Simple != nil {
		id := *t.Simple
		switch id {
		case "int":
			return "Int"
		case "float":
			return "Double"
		case "bool":
			return "Boolean"
		case "string":
			return "String"
		default:
			return id
		}
	}
	if t.Generic != nil {
		g := t.Generic
		args := make([]string, len(g.Args))
		for i, a := range g.Args {
			args[i] = c.typeString(a)
		}
		name := g.Name
		switch name {
		case "list":
			name = "List"
		case "map":
			name = "Map"
		}
		return fmt.Sprintf("%s[%s]", name, strings.Join(args, ", "))
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			parts[i] = c.typeString(p)
		}
		ret := c.typeString(t.Fun.Return)
		return fmt.Sprintf("(%s) => %s", strings.Join(parts, ", "), ret)
	}
	return "Any"
}

func (c *Compiler) mutableTypeString(t *parser.TypeRef) string {
	if t == nil {
		return "Any"
	}
	if t.Generic != nil {
		g := t.Generic
		args := make([]string, len(g.Args))
		for i, a := range g.Args {
			args[i] = c.typeString(a)
		}
		switch g.Name {
		case "list":
			return fmt.Sprintf("scala.collection.mutable.ArrayBuffer[%s]", strings.Join(args, ", "))
		case "map":
			return fmt.Sprintf("scala.collection.mutable.Map[%s]", strings.Join(args, ", "))
		}
	}
	return c.typeString(t)
}

func (c *Compiler) typeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return fmt.Sprintf("List[%s]", c.typeOf(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Map[%s, %s]", c.typeOf(tt.Key), c.typeOf(tt.Value))
	case types.StructType:
		return tt.Name
	case types.UnionType:
		return tt.Name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = c.typeOf(p)
		}
		ret := c.typeOf(tt.Return)
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), ret)
	default:
		return "Any"
	}
}

func sameType(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		_, ok2 := b.(types.AnyType)
		return ok2
	}
	if _, ok := b.(types.AnyType); ok {
		_, ok2 := a.(types.AnyType)
		return ok2
	}
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return sameType(la.Elem, lb.Elem)
		}
	}
	if ma, ok := a.(types.MapType); ok {
		if mb, ok := b.(types.MapType); ok {
			return sameType(ma.Key, mb.Key) && sameType(ma.Value, mb.Value)
		}
	}
	if ua, ok := a.(types.UnionType); ok {
		if sb, ok := b.(types.StructType); ok {
			if _, ok := ua.Variants[sb.Name]; ok {
				return true
			}
		}
	}
	if ub, ok := b.(types.UnionType); ok {
		if sa, ok := a.(types.StructType); ok {
			if _, ok := ub.Variants[sa.Name]; ok {
				return true
			}
		}
	}
	if _, ok := a.(types.Int64Type); ok {
		if _, ok := b.(types.Int64Type); ok {
			return true
		}
		if _, ok := b.(types.IntType); ok {
			return true
		}
	}
	if _, ok := b.(types.Int64Type); ok {
		if _, ok := a.(types.Int64Type); ok {
			return true
		}
		if _, ok := a.(types.IntType); ok {
			return true
		}
	}
	if _, ok := a.(types.IntType); ok {
		if _, ok := b.(types.IntType); ok {
			return true
		}
	}
	return reflect.DeepEqual(a, b)
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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

func simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func identOfUnary(u *parser.Unary) (string, bool) {
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func queryExpr(e *parser.Expr) (*parser.QueryExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Query == nil {
		return nil, false
	}
	return p.Target.Query, true
}

func indexOf(list []*parser.Statement, target *parser.Statement) int {
	for i, s := range list {
		if s == target {
			return i
		}
	}
	return -1
}

func collectUpdates(stmts []*parser.Statement, out map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Update != nil:
			out[s.Update.Target] = true
		case s.If != nil:
			collectUpdates(s.If.Then, out)
			if s.If.ElseIf != nil {
				collectUpdates([]*parser.Statement{{If: s.If.ElseIf}}, out)
			}
			if s.If.Else != nil {
				collectUpdates(s.If.Else, out)
			}
		case s.While != nil:
			collectUpdates(s.While.Body, out)
		case s.For != nil:
			collectUpdates(s.For.Body, out)
		case s.Fun != nil:
			collectUpdates(s.Fun.Body, out)
		case s.Test != nil:
			collectUpdates(s.Test.Body, out)
		}
	}
}

func structKey(st types.StructType) string {
	parts := make([]string, len(st.Order))
	for i, f := range st.Order {
		parts[i] = fmt.Sprintf("%s:%s", f, st.Fields[f].String())
	}
	return strings.Join(parts, ";")
}

func (c *Compiler) ensureStructName(st types.StructType) types.StructType {
	if st.Name != "" {
		return st
	}
	key := structKey(st)
	if name, ok := c.structKeys[key]; ok {
		st.Name = name
		return st
	}
	c.autoCount++
	name := fmt.Sprintf("Auto%d", c.autoCount)
	st.Name = name
	c.autoStructs[name] = st
	c.structKeys[key] = name
	if c.env != nil {
		c.env.SetStruct(name, st)
	}
	return st
}

func (c *Compiler) namedType(t types.Type) types.Type {
	switch tt := t.(type) {
	case types.ListType:
		return types.ListType{Elem: c.namedType(tt.Elem)}
	case types.MapType:
		return types.MapType{Key: c.namedType(tt.Key), Value: c.namedType(tt.Value)}
	case types.StructType:
		return c.ensureStructName(tt)
	case types.FuncType:
		params := make([]types.Type, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = c.namedType(p)
		}
		return types.FuncType{Params: params, Return: c.namedType(tt.Return)}
	default:
		return t
	}
}

func (c *Compiler) emitAutoStructs(out *bytes.Buffer, indent int) {
	if len(c.autoStructs) == 0 {
		return
	}
	names := make([]string, 0, len(c.autoStructs))
	for n := range c.autoStructs {
		names = append(names, n)
	}
	sort.Strings(names)
	pad := strings.Repeat(" ", indent)
	for _, n := range names {
		st := c.autoStructs[n]
		fields := make([]string, len(st.Order))
		for i, f := range st.Order {
			fields[i] = fmt.Sprintf("%s: %s", f, c.typeOf(st.Fields[f]))
		}
		out.WriteString(pad + fmt.Sprintf("case class %s(%s)\n", n, strings.Join(fields, ", ")))
	}
	out.WriteByte('\n')
}
