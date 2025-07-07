//go:build archived

package gocode

import (
	"fmt"
	"sort"

	"mochi/parser"
	"mochi/types"
)

func analyzeVarUsage(stmts []*parser.Statement) map[any]bool {
	usage := make(map[any]bool)
	for i, s := range stmts {
		var name string
		var key any
		switch {
		case s.Let != nil:
			name = s.Let.Name
			key = s.Let
		case s.Var != nil:
			name = s.Var.Name
			key = s.Var
		}
		if key != nil {
			used := false
			for j := i + 1; j < len(stmts); j++ {
				if stmtReadsVar(stmts[j], name) {
					used = true
					break
				}
			}
			usage[key] = used
		}
	}
	return usage
}

func (c *Compiler) compileStmtList(stmts []*parser.Statement) error {
	old := c.varUsage
	c.varUsage = analyzeVarUsage(stmts)
	for _, s := range stmts {
		if err := c.compileStmt(s); err != nil {
			c.varUsage = old
			return err
		}
	}
	c.varUsage = old
	return nil
}

func (c *Compiler) varUsed(stmt any) bool {
	if c.varUsage == nil {
		return true
	}
	if u, ok := c.varUsage[stmt]; ok {
		return u
	}
	return true
}

func (c *Compiler) compileTypeDecls(prog *parser.Program) error {
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileFunDecls(prog *parser.Program) error {
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileTestBlocks(prog *parser.Program) error {
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileMainFunc(prog *parser.Program) error {
	hasTests := hasTest(prog)
	// Emit declarations for variables referenced by later test blocks and
	// initialise them inside main. Initialising complex expressions at the
	// package level can produce invalid Go code (e.g. loops outside of
	// functions).
	initAssigns := []*parser.AssignStmt{}
	for i, s := range prog.Statements {
		if s.Let == nil && s.Var == nil {
			continue
		}
		if hasLaterTest(prog, i) {
			if err := c.compileGlobalVarDecl(s); err != nil {
				return err
			}
			if s.Let != nil && s.Let.Value != nil {
				initAssigns = append(initAssigns, &parser.AssignStmt{Name: s.Let.Name, Value: s.Let.Value})
			}
			if s.Var != nil && s.Var.Value != nil {
				initAssigns = append(initAssigns, &parser.AssignStmt{Name: s.Var.Name, Value: s.Var.Value})
			}
		}
	}

	c.writeln("func main() {")
	c.indent++
	if hasTests {
		c.writeln("failures := 0")
	}
	if len(c.externObjects) > 0 {
		c.imports["mochi/runtime/ffi/extern"] = true
		names := make([]string, 0, len(c.externObjects))
		for name := range c.externObjects {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			c.writeln(fmt.Sprintf("if _, ok := extern.Get(%q); !ok { panic(\"extern object not registered: %s\") }", name, name))
		}
	}
	for _, a := range initAssigns {
		if err := c.compileAssign(a); err != nil {
			return err
		}
	}
	body := []*parser.Statement{}
	for i, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if (s.Let != nil || s.Var != nil) && hasLaterTest(prog, i) {
			continue
		}
		body = append(body, s)
	}
	if err := c.compileStmtList(body); err != nil {
		return err
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			if hasTests {
				c.writeln("{")
				c.indent++
				c.writeln(fmt.Sprintf("printTestStart(%q)", s.Test.Name))
				c.writeln("start := time.Now()")
				c.writeln("var failed error")
				c.writeln("func() {")
				c.indent++
				c.writeln("defer func() { if r := recover(); r != nil { failed = fmt.Errorf(\"%v\", r) } }()")
				c.writeln(fmt.Sprintf("%s()", name))
				c.indent--
				c.writeln("}()")
				c.writeln("if failed != nil {")
				c.indent++
				c.writeln("failures++")
				c.writeln("printTestFail(failed, time.Since(start))")
				c.indent--
				c.writeln("} else {")
				c.indent++
				c.writeln("printTestPass(time.Since(start))")
				c.indent--
				c.writeln("}")
				c.indent--
				c.writeln("}")
			} else {
				c.writeln(fmt.Sprintf("%s()", name))
			}
		}
	}
	if hasTests {
		c.writeln("if failures > 0 {")
		c.indent++
		c.writeln("fmt.Printf(\"\\n[FAIL] %d test(s) failed.\\n\", failures)")
		c.indent--
		c.writeln("}")
	}
	if c.usesHandlers {
		for _, dn := range c.handlerDones {
			c.writeln(fmt.Sprintf("<-%s", dn))
		}
		for _, sn := range c.streams {
			c.writeln(fmt.Sprintf("%s.Close()", sn))
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

// compileGlobalVarDecl emits a package-level variable declaration without
// initialisation. The actual value will be assigned inside main.
func (c *Compiler) compileGlobalVarDecl(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		var t types.Type = types.AnyType{}
		if c.env != nil {
			if s.Let.Type != nil {
				t = c.resolveTypeRef(s.Let.Type)
			} else if s.Let.Value != nil {
				t = c.inferExprType(s.Let.Value)
				if ll := s.Let.Value.Binary.Left.Value.Target.List; ll != nil {
					if st, ok := c.inferStructFromList(ll, s.Let.Name); ok {
						t = types.ListType{Elem: st}
						c.env.SetStruct(st.Name, st)
						c.compileStructType(st)
					}
				}
			} else if old, err := c.env.GetVar(s.Let.Name); err == nil {
				t = old
			}
			c.env.SetVar(s.Let.Name, t, false)
		}
		c.writeln(fmt.Sprintf("var %s %s", name, goType(t)))
		if !c.varUsed(s.Let) {
			c.writeln(fmt.Sprintf("_ = %s", name))
		}
	case s.Var != nil:
		name := sanitizeName(s.Var.Name)
		var t types.Type = types.AnyType{}
		if c.env != nil {
			if s.Var.Type != nil {
				t = c.resolveTypeRef(s.Var.Type)
			} else if s.Var.Value != nil {
				t = c.inferExprTypeHint(s.Var.Value, t)
				if ll := s.Var.Value.Binary.Left.Value.Target.List; ll != nil {
					if st, ok := c.inferStructFromList(ll, s.Var.Name); ok {
						t = types.ListType{Elem: st}
						c.env.SetStruct(st.Name, st)
						c.compileStructType(st)
					}
				}
			} else if old, err := c.env.GetVar(s.Var.Name); err == nil {
				t = old
			}
			c.env.SetVar(s.Var.Name, t, true)
		}
		c.writeln(fmt.Sprintf("var %s %s", name, goType(t)))
		if !c.varUsed(s.Var) {
			c.writeln(fmt.Sprintf("_ = %s", name))
		}
	}
	return nil
}

func hasLaterTest(prog *parser.Program, idx int) bool {
	for _, s := range prog.Statements[idx+1:] {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func exprUsesVarFun(fn *parser.FunExpr, name string) bool {
	for _, s := range fn.BlockBody {
		if stmtUsesVar(s, name) {
			return true
		}
	}
	if fn.ExprBody != nil && exprUsesVar(fn.ExprBody, name) {
		return true
	}
	return false
}

func stmtUsesVar(s *parser.Statement, name string) bool {
	switch {
	case s.Let != nil:
		if s.Let.Name == name {
			return true
		}
		return exprUsesVar(s.Let.Value, name)
	case s.Var != nil:
		if s.Var.Name == name {
			return true
		}
		return exprUsesVar(s.Var.Value, name)
	case s.Assign != nil:
		if s.Assign.Name == name {
			return true
		}
		return exprUsesVar(s.Assign.Value, name)
	case s.Expr != nil:
		return exprUsesVar(s.Expr.Expr, name)
	case s.Return != nil:
		return exprUsesVar(s.Return.Value, name)
	case s.If != nil:
		if exprUsesVar(s.If.Cond, name) {
			return true
		}
		for _, t := range s.If.Then {
			if stmtUsesVar(t, name) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if stmtUsesVar(&parser.Statement{If: s.If.ElseIf}, name) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.For != nil:
		if exprUsesVar(s.For.Source, name) || exprUsesVar(s.For.RangeEnd, name) {
			return true
		}
		for _, t := range s.For.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.While != nil:
		if exprUsesVar(s.While.Cond, name) {
			return true
		}
		for _, t := range s.While.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if stmtUsesVar(t, name) {
				return true
			}
		}
	}
	return false
}

func stmtReadsVar(s *parser.Statement, name string) bool {
	switch {
	case s.Let != nil:
		return exprUsesVar(s.Let.Value, name)
	case s.Var != nil:
		return exprUsesVar(s.Var.Value, name)
	case s.Assign != nil:
		if exprUsesVar(s.Assign.Value, name) {
			return true
		}
		for _, idx := range s.Assign.Index {
			if exprUsesVar(idx.Start, name) || exprUsesVar(idx.End, name) {
				return true
			}
		}
		return false
	case s.Expr != nil:
		return exprUsesVar(s.Expr.Expr, name)
	case s.Return != nil:
		return exprUsesVar(s.Return.Value, name)
	case s.If != nil:
		if exprUsesVar(s.If.Cond, name) {
			return true
		}
		for _, t := range s.If.Then {
			if stmtReadsVar(t, name) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if stmtReadsVar(&parser.Statement{If: s.If.ElseIf}, name) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if stmtReadsVar(t, name) {
				return true
			}
		}
	case s.For != nil:
		if exprUsesVar(s.For.Source, name) || exprUsesVar(s.For.RangeEnd, name) {
			return true
		}
		for _, t := range s.For.Body {
			if stmtReadsVar(t, name) {
				return true
			}
		}
	case s.While != nil:
		if exprUsesVar(s.While.Cond, name) {
			return true
		}
		for _, t := range s.While.Body {
			if stmtReadsVar(t, name) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if stmtReadsVar(t, name) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if stmtReadsVar(t, name) {
				return true
			}
		}
	}
	return false
}

func exprUsesVar(e *parser.Expr, name string) bool {
	if e == nil {
		return false
	}
	if unaryUsesVar(e.Binary.Left, name) {
		return true
	}
	for _, op := range e.Binary.Right {
		if postfixUsesVar(op.Right, name) {
			return true
		}
	}
	return false
}

func unaryUsesVar(u *parser.Unary, name string) bool {
	if u == nil {
		return false
	}
	return postfixUsesVar(u.Value, name)
}

func postfixUsesVar(p *parser.PostfixExpr, name string) bool {
	if p == nil {
		return false
	}
	if primaryUsesVar(p.Target, name) {
		return true
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if exprUsesVar(op.Index.Start, name) || exprUsesVar(op.Index.End, name) {
				return true
			}
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				if exprUsesVar(a, name) {
					return true
				}
			}
		}
	}
	return false
}

func primaryUsesVar(p *parser.Primary, name string) bool {
	if p == nil {
		return false
	}
	if p.Selector != nil {
		if p.Selector.Root == name && len(p.Selector.Tail) == 0 {
			return true
		}
	}
	if p.Group != nil {
		return exprUsesVar(p.Group, name)
	}
	if p.FunExpr != nil {
		return exprUsesVarFun(p.FunExpr, name)
	}
	if p.Struct != nil {
		for _, f := range p.Struct.Fields {
			if exprUsesVar(f.Value, name) {
				return true
			}
		}
	}
	if p.Call != nil {
		if p.Call.Func == name {
			return true
		}
		for _, a := range p.Call.Args {
			if exprUsesVar(a, name) {
				return true
			}
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			if exprUsesVar(e, name) {
				return true
			}
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			if exprUsesVar(it.Key, name) || exprUsesVar(it.Value, name) {
				return true
			}
		}
	}
	if p.Match != nil {
		if exprUsesVar(p.Match.Target, name) {
			return true
		}
		for _, cs := range p.Match.Cases {
			if exprUsesVar(cs.Pattern, name) || exprUsesVar(cs.Result, name) {
				return true
			}
		}
	}
	if p.Query != nil {
		if exprUsesVar(p.Query.Source, name) {
			return true
		}
		if p.Query.Where != nil && exprUsesVar(p.Query.Where, name) {
			return true
		}
		if p.Query.Sort != nil && exprUsesVar(p.Query.Sort, name) {
			return true
		}
		if p.Query.Skip != nil && exprUsesVar(p.Query.Skip, name) {
			return true
		}
		if p.Query.Take != nil && exprUsesVar(p.Query.Take, name) {
			return true
		}
		if exprUsesVar(p.Query.Select, name) {
			return true
		}
	}
	if p.Load != nil {
		// path is a string literal; no variable usage
	}
	if p.Save != nil {
		if exprUsesVar(p.Save.Src, name) {
			return true
		}
	}
	return false
}

func pureFunExpr(e *parser.Expr) *parser.FunExpr {
	if e == nil {
		return nil
	}
	if len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	return p.Target.FunExpr
}
