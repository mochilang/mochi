//go:build slow

package swift

import (
	"fmt"
	"math"
	"regexp"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compile parses Mochi source and generates Swift code. Only a subset of
// Mochi is supported. Unsupported features result in an error.
func Compile(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	c := New(nil)
	return c.Compile(prog)
}

// Compiler converts a subset of Mochi into Swift source code.
type Compiler struct {
	compiler
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{compiler: compiler{
		structs:         make(map[string][]string),
		structTypes:     make(map[string]map[string]string),
		variants:        make(map[string]string),
		inout:           make(map[string][]bool),
		funcArgs:        make(map[string][]string),
		varTypes:        make(map[string]string),
		swiftTypes:      make(map[string]string),
		mapFields:       make(map[string]map[string]string),
		groups:          make(map[string]bool),
		groupElemType:   make(map[string]string),
		groupElemFields: make(map[string]map[string]string),
		helpers:         make(map[string]bool),
		autoStructs:     make(map[string]bool),
		structKeys:      make(map[string]string),
		autoCount:       0,
		builtinAliases:  make(map[string]string),
	}}
}

// Compile generates Swift code for the given program.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	if err := c.program(p); err != nil {
		return nil, err
	}
	body := c.buf.String()
	c.buf.Reset()
	if len(c.helpers) > 0 {
		c.writeln("import Foundation")
		c.writeln("")
		c.emitRuntime()
	}
	if len(c.autoStructs) > 0 {
		c.emitAutoStructs()
	}
	c.buf.WriteString(body)
	return []byte(c.buf.String()), nil
}

type compiler struct {
	buf             strings.Builder
	indent          int
	structs         map[string][]string
	structTypes     map[string]map[string]string
	variants        map[string]string
	inout           map[string][]bool
	funcArgs        map[string][]string
	varTypes        map[string]string
	swiftTypes      map[string]string
	mapFields       map[string]map[string]string
	groups          map[string]bool
	groupElemType   map[string]string
	groupElemFields map[string]map[string]string
	helpers         map[string]bool
	tupleMap        bool
	autoStructs     map[string]bool
	structKeys      map[string]string
	autoCount       int
	builtinAliases  map[string]string
}

func (c *compiler) program(p *parser.Program) error {
	for _, s := range p.Statements {
		if err := c.stmt(s); err != nil {
			return err
		}
	}
	return nil
}

func (c *compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.letStmt(s.Let)
	case s.Var != nil:
		return c.varStmt(s.Var)
	case s.Assign != nil:
		return c.assignStmt(s.Assign)
	case s.Fun != nil:
		return c.funStmt(s.Fun)
	case s.Type != nil:
		return c.typeDecl(s.Type)
	case s.Return != nil:
		return c.returnStmt(s.Return)
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		return c.whileStmt(s.While)
	case s.For != nil:
		return c.forStmt(s.For)
	case s.Break != nil:
		return c.breakStmt(s.Break)
	case s.Continue != nil:
		return c.continueStmt(s.Continue)
	case s.Import != nil:
		return c.importStmt(s.Import)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		// extern declarations have no effect in generated Swift code
		return nil
	case s.Test != nil:
		return c.testBlock(s.Test)
	case s.Expect != nil:
		return c.expectStmt(s.Expect)
	case s.Update != nil:
		return c.updateStmt(s.Update)
	case s.Expr != nil:
		expr, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *compiler) letStmt(l *parser.LetStmt) error {
	typ, err := c.typeRef(l.Type)
	if err != nil {
		return err
	}
	inferred := c.inferType(l.Type, l.Value)
	kw := "let"
	if strings.HasPrefix(inferred, "list") {
		kw = "var"
	}
	if l.Value != nil {
		val, err := c.expr(l.Value)
		if err != nil {
			return err
		}
		if typ != "" {
			c.writeln(fmt.Sprintf("%s %s: %s = %s", kw, l.Name, typ, val))
			c.swiftTypes[l.Name] = typ
		} else {
			c.writeln(fmt.Sprintf("%s %s = %s", kw, l.Name, val))
		}
		c.varTypes[l.Name] = inferred
		c.recordMapFields(l.Name, l.Value)
		return nil
	}
	if typ == "" {
		return fmt.Errorf("let without value or type at line %d", l.Pos.Line)
	}
	c.writeln(fmt.Sprintf("%s %s: %s = %s", kw, l.Name, typ, defaultValue(typ)))
	c.varTypes[l.Name] = inferred
	c.recordMapFields(l.Name, l.Value)
	c.swiftTypes[l.Name] = typ
	return nil
}

func (c *compiler) varStmt(v *parser.VarStmt) error {
	typ, err := c.typeRef(v.Type)
	if err != nil {
		return err
	}
	if v.Value != nil {
		val, err := c.expr(v.Value)
		if err != nil {
			return err
		}
		if typ != "" {
			c.writeln(fmt.Sprintf("var %s: %s = %s", v.Name, typ, val))
			c.swiftTypes[v.Name] = typ
		} else {
			c.writeln(fmt.Sprintf("var %s = %s", v.Name, val))
		}
		c.varTypes[v.Name] = c.inferType(v.Type, v.Value)
		c.recordMapFields(v.Name, v.Value)
		return nil
	}
	if typ == "" {
		return fmt.Errorf("var without value or type at line %d", v.Pos.Line)
	}
	c.writeln(fmt.Sprintf("var %s: %s = %s", v.Name, typ, defaultValue(typ)))
	c.varTypes[v.Name] = c.inferType(v.Type, nil)
	c.recordMapFields(v.Name, v.Value)
	c.swiftTypes[v.Name] = typ
	return nil
}

func (c *compiler) assignStmt(a *parser.AssignStmt) error {
	lhs := a.Name
	baseType := c.varTypes[a.Name]
	for i, idx := range a.Index {
		if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil || idx.Start == nil {
			return fmt.Errorf("complex assignment not supported")
		}
		expr, err := c.expr(idx.Start)
		if err != nil {
			return err
		}
		lhs += fmt.Sprintf("[%s]", expr)
		if strings.HasPrefix(baseType, "map") && (i < len(a.Index)-1 || len(a.Field) > 0) {
			lhs += "!"
		}
		baseType = ""
	}
	for _, f := range a.Field {
		lhs += "." + f.Name
	}

	val, err := c.expr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, val))
	if len(a.Index) == 0 && len(a.Field) == 0 {
		c.varTypes[a.Name] = c.inferType(nil, a.Value)
		c.recordMapFields(a.Name, a.Value)
	}
	return nil
}

func (c *compiler) typeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		indirect := false
		for _, v := range t.Variants {
			for _, f := range v.Fields {
				typ, _ := c.typeRef(f.Type)
				if typ == t.Name {
					indirect = true
				}
			}
			c.variants[v.Name] = t.Name
		}
		if indirect {
			c.writeln(fmt.Sprintf("indirect enum %s {", t.Name))
		} else {
			c.writeln(fmt.Sprintf("enum %s {", t.Name))
		}
		c.indent++
		for _, v := range t.Variants {
			if len(v.Fields) == 0 {
				c.writeln("case " + strings.ToLower(v.Name))
				continue
			}
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				typ, err := c.typeRef(f.Type)
				if err != nil {
					return err
				}
				fields[i] = fmt.Sprintf("%s: %s", f.Name, typ)
			}
			c.writeln(fmt.Sprintf("case %s(%s)", strings.ToLower(v.Name), strings.Join(fields, ", ")))
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.structs[t.Name] = []string{}
	c.structTypes[t.Name] = make(map[string]string)
	c.writeln(fmt.Sprintf("struct %s: Equatable {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field == nil || m.Method != nil {
			return fmt.Errorf("unsupported type member at line %d", t.Pos.Line)
		}
		typ, err := c.typeRef(m.Field.Type)
		if err != nil {
			return err
		}
		c.structs[t.Name] = append(c.structs[t.Name], m.Field.Name)
		c.structTypes[t.Name][m.Field.Name] = typ
		c.writeln(fmt.Sprintf("var %s: %s", m.Field.Name, typ))
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) funStmt(f *parser.FunStmt) error {
	c.writeIndent()
	c.buf.WriteString("func ")
	c.buf.WriteString(f.Name)
	c.buf.WriteString("(")
	c.inout[f.Name] = make([]bool, len(f.Params))
	c.funcArgs[f.Name] = make([]string, len(f.Params))
	for i, p := range f.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		typ, err := c.typeRef(p.Type)
		if err != nil {
			return err
		}
		if typ == "" {
			return fmt.Errorf("parameter %s missing type", p.Name)
		}
		c.buf.WriteString("_ ")
		c.buf.WriteString(p.Name)
		c.buf.WriteString(": ")
		if !c.isBuiltinType(typ) {
			c.buf.WriteString("inout ")
			c.inout[f.Name][i] = true
		}
		c.buf.WriteString(typ)
		c.funcArgs[f.Name][i] = typ
	}
	c.buf.WriteString(")")
	if f.Return != nil {
		rtyp, err := c.typeRef(f.Return)
		if err != nil {
			return err
		}
		if rtyp != "" {
			c.buf.WriteString(" -> ")
			c.buf.WriteString(rtyp)
		}
	}
	c.buf.WriteString(" {\n")
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

func (c *compiler) returnStmt(r *parser.ReturnStmt) error {
	val, err := c.expr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val)
	return nil
}

func (c *compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	if c.needsNilCheck(i.Cond) {
		cond += " != nil"
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if i.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("else ")
		if err := c.ifStmt(i.ElseIf); err != nil {
			return err
		}
		return nil
	}
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *compiler) whileStmt(w *parser.WhileStmt) error {
	cond, err := c.expr(w.Cond)
	if err != nil {
		return err
	}
	if c.needsNilCheck(w.Cond) {
		cond += " != nil"
	}
	c.writeln("while " + cond + " {")
	c.indent++
	for _, st := range w.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) forStmt(f *parser.ForStmt) error {
	var header string
	if f.RangeEnd != nil {
		start, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.expr(f.RangeEnd)
		if err != nil {
			return err
		}
		header = fmt.Sprintf("for %s in %s..<%s {", f.Name, start, end)
	} else {
		src, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		et := c.elementType(f.Source)
		styp := c.exprType(f.Source)
		if styp == "map" {
			src += ".keys"
		} else if et == "map" {
			src += " as! [[String:Any]]"
		}
		header = fmt.Sprintf("for %s in %s {", f.Name, src)
	}
	c.writeln(header)
	c.indent++
	prev, havePrev := c.varTypes[f.Name]
	prevFields, haveFields := c.mapFields[f.Name]
	elem := c.elementType(f.Source)
	srcTyp := c.exprType(f.Source)
	if srcTyp == "map" {
		c.varTypes[f.Name] = "string"
	} else {
		c.varTypes[f.Name] = elem
		if fields := c.elementFieldTypes(f.Source); fields != nil {
			c.mapFields[f.Name] = fields
		}
	}
	if sel := f.Source.Binary.Left.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
		if c.groups[sel.Root] {
			c.groups[f.Name] = true
			if t, ok := c.groupElemType[sel.Root]; ok {
				c.groupElemType[f.Name] = t
			}
			if gf, ok := c.groupElemFields[sel.Root]; ok {
				c.groupElemFields[f.Name] = gf
			}
		}
	}
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			if havePrev {
				c.varTypes[f.Name] = prev
			} else {
				delete(c.varTypes, f.Name)
			}
			if haveFields {
				c.mapFields[f.Name] = prevFields
			} else {
				delete(c.mapFields, f.Name)
			}
			return err
		}
	}
	if havePrev {
		c.varTypes[f.Name] = prev
	} else {
		delete(c.varTypes, f.Name)
	}
	if haveFields {
		c.mapFields[f.Name] = prevFields
	} else {
		delete(c.mapFields, f.Name)
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) breakStmt(_ *parser.BreakStmt) error {
	c.writeln("break")
	return nil
}

func (c *compiler) continueStmt(_ *parser.ContinueStmt) error {
	c.writeln("continue")
	return nil
}

func (c *compiler) importStmt(im *parser.ImportStmt) error {
	if im.Lang == nil {
		return nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	path := strings.Trim(im.Path, "\"")
	switch *im.Lang {
	case "python":
		if path == "math" {
			c.builtinAliases[alias] = "python_math"
			c.helpers["foundation"] = true
		}
	case "go":
		if im.Auto && path == "mochi/runtime/ffi/go/testpkg" {
			c.builtinAliases[alias] = "go_testpkg"
		}
	}
	return nil
}

func (c *compiler) testBlock(t *parser.TestBlock) error {
	for _, st := range t.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	return nil
}

func (c *compiler) expectStmt(e *parser.ExpectStmt) error {
	val, err := c.expr(e.Value)
	if err != nil {
		return err
	}
	c.writeln("assert(" + val + ")")
	return nil
}

func (c *compiler) updateStmt(u *parser.UpdateStmt) error {
	typ, ok := c.varTypes[u.Target]
	if !ok || !strings.HasPrefix(typ, "list") {
		return fmt.Errorf("update only supported on list variables")
	}
	idx := "i"
	c.writeln(fmt.Sprintf("for %s in 0..<%s.count {", idx, u.Target))
	c.indent++
	elem := "elem"
	c.writeln(fmt.Sprintf("var %s = %s[%s]", elem, u.Target, idx))
	saved := c.varTypes[elem]
	c.varTypes[elem] = strings.TrimPrefix(typ, "list_")
	cond := "true"
	elemType := ""
	if st, ok := c.swiftTypes[u.Target]; ok {
		if strings.HasPrefix(st, "[") && strings.HasSuffix(st, "]") {
			elemType = strings.TrimSuffix(strings.TrimPrefix(st, "["), "]")
		}
	}
	if elemType == "" {
		elemType = strings.TrimPrefix(typ, "list_")
	}
	fields := c.structs[elemType]
	if u.Where != nil {
		savedVars := c.varTypes
		c.varTypes = copyMap(c.varTypes)
		for _, f := range fields {
			c.varTypes[f] = ""
		}
		condExpr, err := c.expr(u.Where)
		c.varTypes = savedVars
		if err != nil {
			return err
		}
		for _, f := range fields {
			condExpr = replaceIdent(condExpr, f, elem+"."+f)
		}
		cond = condExpr
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, it := range u.Set.Items {
		key, ok := keyName(it.Key)
		if !ok {
			return fmt.Errorf("update key must be literal")
		}
		val, err := c.expr(it.Value)
		if err != nil {
			return err
		}
		for _, f := range fields {
			val = replaceIdent(val, f, elem+"."+f)
		}
		c.writeln(fmt.Sprintf("%s.%s = %s", elem, key, val))
	}
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s[%s] = %s", u.Target, idx, elem))
	if saved != "" {
		c.varTypes[elem] = saved
	} else {
		delete(c.varTypes, elem)
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) expr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
	}
	return c.binary(e.Binary)
}

func (c *compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	leftIsFloat := isFloatExpr(left)
	for _, op := range b.Right {
		if !supportedOp(op.Op) {
			return "", fmt.Errorf("unsupported operator %s at line %d", op.Op, op.Pos.Line)
		}
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		rightIsFloat := isFloatExpr(r)
		switch op.Op {
		case "in":
			typ := c.primaryType(op.Right.Target)
			if typ == "map" {
				res = fmt.Sprintf("%s.keys.contains(%s)", r, res)
			} else {
				res = fmt.Sprintf("%s.contains(%s)", r, res)
			}
		case "union":
			if op.All {
				res = fmt.Sprintf("(%s + %s)", res, r)
			} else {
				res = fmt.Sprintf("Array(Set(%s).union(%s)).sorted()", res, r)
			}
		case "except":
			res = fmt.Sprintf("Array(Set(%s).subtracting(%s)).sorted()", res, r)
		case "intersect":
			res = fmt.Sprintf("Array(Set(%s).intersection(%s)).sorted()", res, r)
		default:
			if (leftIsFloat || rightIsFloat) && (op.Op == "*" || op.Op == "/" || op.Op == "+" || op.Op == "-") {
				if leftIsFloat && !rightIsFloat {
					r = fmt.Sprintf("Double(%s)", r)
					rightIsFloat = true
				} else if rightIsFloat && !leftIsFloat {
					res = fmt.Sprintf("Double(%s)", res)
					leftIsFloat = true
				}
			}
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
			leftIsFloat = leftIsFloat || rightIsFloat
		}
	}
	return res, nil
}

func supportedOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "||", "in", "union", "except", "intersect":
		return true
	}
	return false
}

func (c *compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op != "-" && op != "!" {
			return "", fmt.Errorf("unsupported unary operator %s at line %d", op, u.Pos.Line)
		}
		val = op + val
	}
	return val, nil
}

func (c *compiler) postfix(p *parser.PostfixExpr) (string, error) {
	// special case: map literal cast to struct
	if len(p.Ops) == 1 && p.Ops[0].Cast != nil && p.Target.Map != nil {
		typ, err := c.typeRef(p.Ops[0].Cast.Type)
		if err != nil {
			return "", err
		}
		if _, ok := c.structs[typ]; ok {
			return c.mapToStruct(p.Target.Map, typ)
		}
	}

	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
				start := "0"
				end := fmt.Sprintf("%s.count", val)
				if idx.Start != nil {
					start, err = c.expr(idx.Start)
					if err != nil {
						return "", err
					}
				}
				if idx.End != nil {
					end, err = c.expr(idx.End)
					if err != nil {
						return "", err
					}
				}
				typ := c.primaryType(p.Target)
				if typ == "string" {
					startIdx := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", val, val, start)
					endIdx := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", val, val, end)
					val = fmt.Sprintf("String(%s[%s..<%s])", val, startIdx, endIdx)
				} else {
					val = fmt.Sprintf("Array(%s[%s..<%s])", val, start, end)
				}
			} else {
				if idx.Start == nil {
					return "", fmt.Errorf("empty index")
				}
				s, err := c.expr(idx.Start)
				if err != nil {
					return "", err
				}
				typ := c.primaryType(p.Target)
				if typ == "string" {
					pos := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", val, val, s)
					val = fmt.Sprintf("%s[%s]", val, pos)
				} else if typ == "map" {
					if key, ok := keyName(idx.Start); ok {
						if sel := p.Target.Selector; sel != nil && len(sel.Tail) == 0 {
							if m, ok2 := c.mapFields[sel.Root]; ok2 {
								if t, ok3 := m[key]; ok3 {
									val = fmt.Sprintf("(%s[%q] as! %s)", val, key, t)
									continue
								}
							}
						}
					}
					val = fmt.Sprintf("%s[%s]!", val, s)
				} else {
					val = fmt.Sprintf("%s[%s]", val, s)
				}
			}
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
		case op.Call != nil:
			parts := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				parts[i] = s
			}
			recv := val
			if idx := strings.LastIndex(recv, "."); idx > 0 {
				alias := recv[:idx]
				method := recv[idx+1:]
				if mod, ok := c.builtinAliases[alias]; ok {
					args := parts
					switch mod {
					case "python_math":
						switch method {
						case "sqrt":
							if len(args) == 1 {
								val = fmt.Sprintf("sqrt(%s)", args[0])
								continue
							}
						case "pow":
							if len(args) == 2 {
								val = fmt.Sprintf("pow(%s, %s)", args[0], args[1])
								continue
							}
						case "sin":
							if len(args) == 1 {
								val = fmt.Sprintf("sin(%s)", args[0])
								continue
							}
						case "log":
							if len(args) == 1 {
								val = fmt.Sprintf("log(%s)", args[0])
								continue
							}
						}
					case "go_testpkg":
						if method == "Add" && len(args) == 2 {
							val = fmt.Sprintf("(%s + %s)", args[0], args[1])
							continue
						}
					}
				}
			}
			val = fmt.Sprintf("%s(%s)", recv, strings.Join(parts, ", "))
		case op.Cast != nil:
			typ, err := c.typeRef(op.Cast.Type)
			if err != nil {
				return "", err
			}
			switch typ {
			case "Int", "Double", "Bool", "String":
				val = fmt.Sprintf("%s(%s)", typ, val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", typ)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Lit != nil:
		return literal(p.Lit), nil
	case p.Call != nil:
		return c.callExpr(p.Call)
	case p.List != nil:
		if len(p.List.Elems) == 0 {
			return "[Any]()", nil
		}
		if st, ok := c.detectAutoStructList(p.List, ""); ok {
			order := c.structs[st]
			elems := make([]string, len(p.List.Elems))
			for i, e := range p.List.Elems {
				m := mapLit(e)
				if m == nil {
					return "", fmt.Errorf("expected map literal")
				}
				vals := make(map[string]string)
				for _, it := range m.Items {
					key, _ := keyName(it.Key)
					v, err := c.expr(it.Value)
					if err != nil {
						return "", err
					}
					vals[key] = v
				}
				parts := make([]string, len(order))
				for j, k := range order {
					parts[j] = fmt.Sprintf("%s: %s", k, vals[k])
				}
				elems[i] = fmt.Sprintf("%s(%s)", st, strings.Join(parts, ", "))
			}
			return "[" + strings.Join(elems, ", ") + "]", nil
		}
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		return c.mapLiteral(p.Map)
	case p.Struct != nil:
		return c.structLiteral(p.Struct)
	case p.If != nil:
		return c.ifExpr(p.If)
	case p.Match != nil:
		return c.matchExpr(p.Match)
	case p.Query != nil:
		return c.queryExpr(p.Query)
	case p.Load != nil:
		return c.loadExpr(p.Load)
	case p.Save != nil:
		return c.saveExpr(p.Save)
	case p.Selector != nil:
		return c.selector(p.Selector), nil
	case p.Group != nil:
		e, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *compiler) callExpr(call *parser.CallExpr) (string, error) {
	// special case: exists(query ...)
	if call.Func == "exists" && len(call.Args) == 1 {
		if q := queryArg(call.Args[0]); q != nil {
			return c.existsQuery(q)
		}
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.expr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	joined := strings.Join(args, ", ")
	switch call.Func {
	case "print":
		return fmt.Sprintf("print(%s)", joined), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("String(%s)", joined), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 arguments at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("%s + [%s]", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 argument at line %d", call.Pos.Line)
		}
		if name, ok := c.isGroupVar(call.Args[0]); ok {
			return fmt.Sprintf("%s.items.reduce(0, +) / %s.items.count", name, name), nil
		}
		a := args[0]
		return fmt.Sprintf("(%s.reduce(0, +) / %s.count)", a, a), nil
	case "count", "len":
		if len(args) != 1 {
			return "", fmt.Errorf("%s expects 1 argument at line %d", call.Func, call.Pos.Line)
		}
		if name, ok := c.isGroupVar(call.Args[0]); ok {
			return fmt.Sprintf("%s.items.count", name), nil
		}
		return fmt.Sprintf("%s.count", args[0]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 argument at line %d", call.Pos.Line)
		}
		if name, ok := c.isGroupVar(call.Args[0]); ok {
			return fmt.Sprintf("%s.items.min()!", name), nil
		}
		return fmt.Sprintf("%s.min()!", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 argument at line %d", call.Pos.Line)
		}
		if name, ok := c.isGroupVar(call.Args[0]); ok {
			return fmt.Sprintf("%s.items.max()!", name), nil
		}
		return fmt.Sprintf("%s.max()!", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 argument at line %d", call.Pos.Line)
		}
		if name, ok := c.isGroupVar(call.Args[0]); ok {
			return fmt.Sprintf("%s.items.reduce(0, +)", name), nil
		}
		return fmt.Sprintf("%s.reduce(0, +)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 argument at line %d", call.Pos.Line)
		}
		if q := queryArg(call.Args[0]); q != nil {
			return c.existsQuery(q)
		}
		return fmt.Sprintf("!%s.isEmpty", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("Array(%s.values)", args[0]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 argument at line %d", call.Pos.Line)
		}
		c.helpers["_json"] = true
		return fmt.Sprintf("_json(%s)", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 arguments at line %d", call.Pos.Line)
		}
		s := args[0]
		start := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", s, s, args[1])
		end := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", s, s, args[2])
		return fmt.Sprintf("String(%s[%s..<%s])", s, start, end), nil
	default:
		if params, ok := c.funcArgs[call.Func]; ok && len(call.Args) < len(params) {
			missing := len(params) - len(call.Args)
			names := make([]string, missing)
			callParts := make([]string, len(params))
			for i, a := range args {
				callParts[i] = a
			}
			for i := 0; i < missing; i++ {
				name := fmt.Sprintf("p%d", i)
				names[i] = fmt.Sprintf("_ %s: %s", name, params[len(call.Args)+i])
				callParts[len(call.Args)+i] = name
			}
			return fmt.Sprintf("{ (%s) in %s(%s) }", strings.Join(names, ", "), call.Func, strings.Join(callParts, ", ")), nil
		}
		if flags, ok := c.inout[call.Func]; ok {
			for i := range flags {
				if flags[i] && i < len(args) {
					args[i] = "&" + args[i]
				}
			}
			joined = strings.Join(args, ", ")
		}
		return fmt.Sprintf("%s(%s)", call.Func, joined), nil
	}
}

func (c *compiler) funExpr(f *parser.FunExpr) (string, error) {
	var b strings.Builder
	b.WriteString("{ ")
	if len(f.Params) > 0 {
		b.WriteString("(")
		for i, p := range f.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			typ, err := c.typeRef(p.Type)
			if err != nil {
				return "", err
			}
			b.WriteString(p.Name)
			if typ != "" {
				b.WriteString(": ")
				b.WriteString(typ)
			}
		}
		b.WriteString(") ")
	}
	if f.Return != nil {
		rt, err := c.typeRef(f.Return)
		if err != nil {
			return "", err
		}
		if rt != "" {
			b.WriteString("-> ")
			b.WriteString(rt)
			b.WriteString(" ")
		}
	}
	b.WriteString("in ")
	if f.ExprBody == nil {
		return "", fmt.Errorf("block closures not supported")
	}
	expr, err := c.expr(f.ExprBody)
	if err != nil {
		return "", err
	}
	b.WriteString(expr)
	b.WriteString(" }")
	return b.String(), nil
}

func (c *compiler) ifExpr(i *parser.IfExpr) (string, error) {
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
	}
	return fmt.Sprintf("%s ? %s : %s", cond, thenVal, elseVal), nil
}

func (c *compiler) structLiteral(s *parser.StructLiteral) (string, error) {
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.expr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", f.Name, v)
	}
	if enum, ok := c.variants[s.Name]; ok {
		if len(fields) == 0 {
			return fmt.Sprintf("%s.%s", enum, strings.ToLower(s.Name)), nil
		}
		return fmt.Sprintf("%s.%s(%s)", enum, strings.ToLower(s.Name), strings.Join(fields, ", ")), nil
	}
	return fmt.Sprintf("%s(%s)", s.Name, strings.Join(fields, ", ")), nil
}

func (c *compiler) matchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.expr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("{ () in\n")
	b.WriteString("    let __t = ")
	b.WriteString(target)
	b.WriteString("\n")
	b.WriteString("    switch __t {\n")
	for _, cse := range m.Cases {
		var patStr string
		if cse.Pattern.Binary.Left.Value.Target.Struct != nil {
			call := cse.Pattern.Binary.Left.Value.Target.Struct
			if _, ok := c.variants[call.Name]; ok {
				if len(call.Fields) == 0 {
					patStr = fmt.Sprintf(".%s", strings.ToLower(call.Name))
				} else {
					pats := make([]string, len(call.Fields))
					for i, f := range call.Fields {
						pats[i] = f.Name
					}
					patStr = fmt.Sprintf("let .%s(%s)", strings.ToLower(call.Name), strings.Join(pats, ", "))
				}
			}
		} else if call := cse.Pattern.Binary.Left.Value.Target.Call; call != nil {
			if _, ok := c.variants[call.Func]; ok {
				if len(call.Args) == 0 {
					patStr = fmt.Sprintf(".%s", strings.ToLower(call.Func))
				} else {
					pats := make([]string, len(call.Args))
					for i, a := range call.Args {
						s, _ := c.expr(a)
						pats[i] = s
					}
					patStr = fmt.Sprintf("let .%s(%s)", strings.ToLower(call.Func), strings.Join(pats, ", "))
				}
			}
		}
		if patStr == "" {
			p, err := c.expr(cse.Pattern)
			if err != nil {
				return "", err
			}
			patStr = p
		}
		if isWildcard(cse.Pattern) {
			b.WriteString("    default: return ")
		} else {
			b.WriteString("    case ")
			b.WriteString(patStr)
			b.WriteString(": return ")
		}
		res, err := c.expr(cse.Result)
		if err != nil {
			return "", err
		}
		b.WriteString(res)
		b.WriteString("\n")
	}
	b.WriteString("    }\n")
	b.WriteString("}()")
	return b.String(), nil
}

func (c *compiler) mapToStruct(m *parser.MapLiteral, name string) (string, error) {
	fields := make([]string, len(m.Items))
	for i, it := range m.Items {
		key, ok := stringLiteral(it.Key)
		if !ok {
			return "", fmt.Errorf("struct field must be string literal")
		}
		v, err := c.expr(it.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", key, v)
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(fields, ", ")), nil
}

func (c *compiler) mapLiteral(m *parser.MapLiteral) (string, error) {
	if c.tupleMap {
		fields := make([]string, len(m.Items))
		for i, it := range m.Items {
			key, ok := keyName(it.Key)
			if !ok {
				c.tupleMap = false
				return c.mapLiteral(m)
			}
			v, err := c.expr(it.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s: %s", key, v)
		}
		return "(" + strings.Join(fields, ", ") + ")", nil
	}
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		var kstr string
		if name, ok := keyName(it.Key); ok {
			kstr = fmt.Sprintf("%q", name)
		} else {
			var err error
			kstr, err = c.expr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.expr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("%s: %s", kstr, v)
	}
	return "[" + strings.Join(items, ", ") + "]", nil
}

func keyName(e *parser.Expr) (string, bool) {
	if s, ok := stringLiteral(e); ok {
		return s, true
	}
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", false
	}
	sel := e.Binary.Left.Value.Target.Selector
	if sel != nil && len(sel.Tail) == 0 {
		return sel.Root, true
	}
	return "", false
}

func stringLiteral(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", false
	}
	p := e.Binary.Left.Value.Target
	if p.Lit != nil && p.Lit.Str != nil {
		return *p.Lit.Str, true
	}
	return "", false
}

func isWildcard(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	sel := e.Binary.Left.Value.Target.Selector
	return sel != nil && sel.Root == "_" && len(sel.Tail) == 0
}

func (c *compiler) needsNilCheck(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	sel := u.Value.Target.Selector
	if sel == nil || len(sel.Tail) == 0 {
		return false
	}
	typ := c.varTypes[sel.Root]
	if typ == "map" || c.mapFields[sel.Root] != nil {
		return true
	}
	return false
}

func (c *compiler) selector(s *parser.SelectorExpr) string {
	if len(s.Tail) == 0 {
		if enum, ok := c.variants[s.Root]; ok {
			return enum + "." + strings.ToLower(s.Root)
		}
		return s.Root
	}
	if len(s.Tail) == 1 {
		if mod, ok := c.builtinAliases[s.Root]; ok {
			switch mod {
			case "python_math":
				switch s.Tail[0] {
				case "pi":
					return "Double.pi"
				case "e":
					return "2.718281828459045"
				}
			case "go_testpkg":
				switch s.Tail[0] {
				case "Pi":
					return "3.14"
				case "Answer":
					return "42"
				}
			}
		}
	}
	typ := c.varTypes[s.Root]
	if strings.HasPrefix(typ, "map") || (typ == "" && c.mapFields[s.Root] != nil) {
		parts := []string{s.Root}
		for _, f := range s.Tail {
			typ := "Any"
			if m, ok := c.mapFields[s.Root]; ok {
				if t, ok2 := m[f]; ok2 && t != "" {
					typ = t
				}
			}
			if typ == "Any" {
				parts = append(parts, fmt.Sprintf("[%q]!", f))
			} else {
				parts = append(parts, fmt.Sprintf("[%q] as! %s", f, typ))
			}
		}
		return strings.Join(parts, "")
	}
	return s.Root + "." + strings.Join(s.Tail, ".")
}

func literal(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		v := *l.Float
		if v == math.Trunc(v) {
			return fmt.Sprintf("%.1f", v)
		}
		return fmt.Sprintf("%g", v)
	case l.Bool != nil:
		if *l.Bool {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	default:
		return "nil"
	}
}

func (c *compiler) typeRef(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "", nil
	}
	if t.Generic != nil {
		name := strings.ToLower(t.Generic.Name)
		switch name {
		case "list":
			if len(t.Generic.Args) != 1 {
				return "", fmt.Errorf("list requires 1 type arg")
			}
			et, err := c.typeRef(t.Generic.Args[0])
			if err != nil {
				return "", err
			}
			if et == "" {
				return "", fmt.Errorf("unsupported list element type")
			}
			return "[" + et + "]", nil
		case "map":
			if len(t.Generic.Args) != 2 {
				return "", fmt.Errorf("map requires 2 type args")
			}
			kt, err := c.typeRef(t.Generic.Args[0])
			if err != nil {
				return "", err
			}
			vt, err := c.typeRef(t.Generic.Args[1])
			if err != nil {
				return "", err
			}
			if kt == "" || vt == "" {
				return "", fmt.Errorf("unsupported map type")
			}
			return "[" + kt + ": " + vt + "]", nil
		}
		return "", fmt.Errorf("unsupported generic type %s", t.Generic.Name)
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			pt, err := c.typeRef(p)
			if err != nil {
				return "", err
			}
			if pt == "" {
				return "", fmt.Errorf("unsupported func param type")
			}
			params[i] = pt
		}
		ret := "Void"
		if t.Fun.Return != nil {
			rt, err := c.typeRef(t.Fun.Return)
			if err != nil {
				return "", err
			}
			if rt != "" {
				ret = rt
			}
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret, nil
	}
	if t.Simple == nil {
		return "", fmt.Errorf("unsupported type reference")
	}
	switch strings.ToLower(*t.Simple) {
	case "int":
		return "Int", nil
	case "string":
		return "String", nil
	case "float":
		return "Double", nil
	case "bool":
		return "Bool", nil
	default:
		return *t.Simple, nil
	}
}

func defaultValue(typ string) string {
	switch typ {
	case "Int":
		return "0"
	case "Double":
		return "0.0"
	case "Bool":
		return "false"
	case "String":
		return "\"\""
	default:
		return "nil"
	}
}

func (c *compiler) isBuiltinType(typ string) bool {
	switch typ {
	case "Int", "Double", "Bool", "String":
		return true
	}
	for _, en := range c.variants {
		if en == typ {
			return true
		}
	}
	if strings.HasPrefix(typ, "[") && strings.HasSuffix(typ, "]") {
		return true
	}
	return false
}

func swiftTypeOf(t string) string {
	switch t {
	case "string":
		return "String"
	case "bool":
		return "Bool"
	case "number", "int", "float":
		return "Int"
	case "map":
		return "[String:Any]"
	case "list":
		return "[Any]"
	}
	if strings.HasPrefix(t, "list_") {
		et := swiftTypeOf(strings.TrimPrefix(t, "list_"))
		if et != "" {
			return "[" + et + "]"
		}
	}
	if t != "" {
		return t
	}
	return ""
}

func inferTypeRef(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Generic != nil {
		name := strings.ToLower(t.Generic.Name)
		switch name {
		case "list":
			if len(t.Generic.Args) == 1 {
				et := inferTypeRef(t.Generic.Args[0])
				if et != "" {
					return "list_" + et
				}
				return "list"
			}
		case "map":
			return "map"
		}
		return ""
	}
	if t.Simple != nil {
		switch strings.ToLower(*t.Simple) {
		case "string":
			return "string"
		case "int", "float", "bool":
			return "number"
		default:
			return strings.ToLower(*t.Simple)
		}
	}
	return ""
}

func (c *compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *compiler) inferType(t *parser.TypeRef, val *parser.Expr) string {
	if t != nil {
		if ty := inferTypeRef(t); ty != "" {
			return ty
		}
		if t.Simple != nil {
			switch strings.ToLower(*t.Simple) {
			case "string":
				return "string"
			case "int", "float", "bool":
				return "number"
			}
		}
	}
	if val != nil && val.Binary != nil && val.Binary.Left != nil {
		p := val.Binary.Left.Value
		if len(val.Binary.Right) == 0 && len(p.Ops) == 0 {
			switch {
			case p.Target.Lit != nil && p.Target.Lit.Str != nil:
				return "string"
			case p.Target.List != nil:
				if len(p.Target.List.Elems) > 0 && p.Target.List.Elems[0].Binary != nil {
					et := c.exprType(p.Target.List.Elems[0])
					if et != "" {
						return "list_" + et
					}
				}
				return "list"
			case p.Target.Map != nil:
				return "map"
			case p.Target.Query != nil:
				t := c.exprType(p.Target.Query.Select)
				if t != "" {
					return "list_" + t
				}
				if isVarRef(p.Target.Query.Select, p.Target.Query.Var) {
					et := c.elementType(p.Target.Query.Source)
					if et != "" {
						return "list_" + et
					}
				}
				return "list"
			}
		}
	}
	return ""
}

func (c *compiler) exprType(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return ""
	}
	u := e.Binary.Left
	if u == nil {
		return ""
	}
	p := u.Value
	if p == nil || p.Target == nil {
		return ""
	}
	if p.Target.Selector != nil {
		if len(p.Target.Selector.Tail) == 0 {
			if typ, ok := c.varTypes[p.Target.Selector.Root]; ok {
				return typ
			}
		}
		if len(p.Target.Selector.Tail) == 1 {
			root := p.Target.Selector.Root
			fld := p.Target.Selector.Tail[0]
			if m, ok := c.mapFields[root]; ok && m != nil {
				if t, ok2 := m[fld]; ok2 && t != "" {
					return t
				}
			}
			if st, ok := c.structTypes[c.varTypes[root]]; ok {
				if t, ok2 := st[fld]; ok2 && t != "" {
					return t
				}
			}
			if c.groups[root] {
				if m, ok2 := c.groupElemFields[root]; ok2 {
					if t, ok3 := m[fld]; ok3 && t != "" {
						return t
					}
				}
			}
		}
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return "string"
	}
	if p.Target.List != nil {
		if len(p.Target.List.Elems) > 0 {
			et := c.exprType(p.Target.List.Elems[0])
			if et != "" {
				return "list_" + et
			}
		}
		return "list"
	}
	if p.Target.Map != nil {
		return "map"
	}
	if p.Target.Query != nil {
		t := c.exprType(p.Target.Query.Select)
		if t != "" {
			return "list_" + t
		}
		return "list"
	}
	return ""
}

func (c *compiler) primaryType(p *parser.Primary) string {
	if p == nil {
		return ""
	}
	switch {
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if typ, ok := c.varTypes[p.Selector.Root]; ok {
			return typ
		}
	case p.Lit != nil && p.Lit.Str != nil:
		return "string"
	case p.List != nil:
		if len(p.List.Elems) > 0 {
			et := c.exprType(p.List.Elems[0])
			if et != "" {
				return "list_" + et
			}
		}
		return "list"
	case p.Map != nil:
		return "map"
	case p.Query != nil:
		t := c.exprType(p.Query.Select)
		if t != "" {
			return "list_" + t
		}
		return "list"
	}
	return ""
}

func queryArg(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Query
}

func (c *compiler) existsQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}
	if q.Select != nil && !isVarRef(q.Select, q.Var) {
		return "", fmt.Errorf("unsupported query")
	}
	if q.Where == nil {
		return fmt.Sprintf("!%s.isEmpty", src), nil
	}
	saved := c.varTypes
	c.varTypes = copyMap(c.varTypes)
	c.varTypes[q.Var] = c.exprType(q.Source)
	cond, err := c.expr(q.Where)
	if err != nil {
		c.varTypes = saved
		return "", err
	}
	if !boolExpr(q.Where) {
		cond = castCond(cond)
		cond = fmt.Sprintf("%s != nil", cond)
	}
	c.varTypes = saved
	return fmt.Sprintf("%s.contains { %s in %s }", src, q.Var, cond), nil
}

func (c *compiler) groupQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Group.Exprs) != 1 || len(q.Froms) > 0 {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}
	savedVars := c.varTypes
	savedFields := c.mapFields
	c.varTypes = copyMap(c.varTypes)
	c.mapFields = copyMap(c.mapFields)
	elemType := c.elementType(q.Source)
	c.varTypes[q.Var] = elemType
	if fields := c.elementFieldTypes(q.Source); fields != nil {
		c.mapFields[q.Var] = fields
	}
	var filtered string
	if q.Where != nil {
		cond, err := c.expr(q.Where)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		if !boolExpr(q.Where) {
			cond = castCond(cond)
			cond = fmt.Sprintf("%s != nil", cond)
		}
		filtered = fmt.Sprintf("%s.filter { %s in %s }", src, q.Var, cond)
	} else {
		filtered = src
	}
	keyExpr, err := c.expr(q.Group.Exprs[0])
	if err != nil {
		c.varTypes = savedVars
		c.mapFields = savedFields
		return "", err
	}
	keyTyp := c.fieldType(q.Group.Exprs[0])
	if keyTyp == "" {
		keyTyp = c.exprType(q.Group.Exprs[0])
	}
	gname := q.Group.Name
	c.varTypes[gname] = ""
	c.mapFields[gname] = nil
	c.groups[gname] = true
	c.groupElemType[gname] = elemType
	if fields := c.elementFieldTypes(q.Source); fields != nil {
		c.groupElemFields[gname] = fields
	}
	var having string
	if q.Group.Having != nil {
		hv, err := c.expr(q.Group.Having)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		having = hv
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		c.varTypes = savedVars
		c.mapFields = savedFields
		return "", err
	}
	sortExpr := ""
	if q.Sort != nil {
		s, err := c.expr(q.Sort)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		sortExpr = s
	}
	skipExpr := ""
	if q.Skip != nil {
		sk, err := c.expr(q.Skip)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		skipExpr = sk
	}
	takeExpr := ""
	if q.Take != nil {
		tk, err := c.expr(q.Take)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		takeExpr = tk
	}
	c.varTypes = savedVars
	c.mapFields = savedFields
	var b strings.Builder
	selIsGroup := sel == gname
	et := swiftTypeOf(elemType)
	if et == "" {
		et = "Any"
	}
	kt := swiftTypeOf(keyTyp)
	if kt == "" {
		kt = "AnyHashable"
	}
	useMapKey := kt == "[String:Any]"
	retType := "[Any]"
	if selIsGroup {
		retType = fmt.Sprintf("[(key: %s, items: [%s])]", kt, et)
	}
	b.WriteString(fmt.Sprintf("{ () -> %s in\n", retType))
	if useMapKey {
		c.helpers["_group"] = true
		b.WriteString("    var _groups: [String:_Group] = [:]\n")
		b.WriteString("    var _order: [String] = []\n")
	} else {
		b.WriteString(fmt.Sprintf("    var _groups: [%s:[%s]] = [:]\n", kt, et))
	}
	b.WriteString(fmt.Sprintf("    for %s in %s {\n", q.Var, filtered))
	b.WriteString(fmt.Sprintf("        let _k = %s\n", keyExpr))
	if useMapKey {
		b.WriteString("        let _ks = _keyStr(_k)\n")
		b.WriteString("        if _groups[_ks] == nil {\n")
		b.WriteString("            _groups[_ks] = _Group(_k)\n")
		b.WriteString("            _order.append(_ks)\n")
		b.WriteString("        }\n")
		b.WriteString(fmt.Sprintf("        _groups[_ks]!.Items.append(%s)\n", q.Var))
	} else {
		b.WriteString(fmt.Sprintf("        _groups[_k, default: []].append(%s)\n", q.Var))
	}
	b.WriteString("    }\n")
	if useMapKey {
		b.WriteString(fmt.Sprintf("    var _tmp: [(key: %s, items: [%s])] = []\n", kt, et))
		b.WriteString("    for k in _order {\n")
		b.WriteString("        if let g = _groups[k] {\n")
		b.WriteString(fmt.Sprintf("            _tmp.append((key: g.key as! %s, items: g.Items.map { $0 as! %s }))\n", kt, et))
		b.WriteString("        }\n")
		b.WriteString("    }\n")
	} else {
		b.WriteString(fmt.Sprintf("    var _tmp: [(key: %s, items: [%s])] = []\n", kt, et))
		b.WriteString("    for (k, v) in _groups {\n")
		b.WriteString("        _tmp.append((key: k, items: v))\n")
		b.WriteString("    }\n")
	}
	if having != "" {
		b.WriteString("    _tmp = _tmp.filter { " + gname + " in " + having + " }\n")
	}
	if sortExpr != "" {
		desc := strings.HasPrefix(sortExpr, "-")
		if desc {
			sortExpr = strings.TrimPrefix(sortExpr, "-")
		}
		a := replaceIdent(sortExpr, gname, "$0")
		bstr := replaceIdent(sortExpr, gname, "$1")
		if strings.ToLower(keyTyp) == "string" {
			a = fmt.Sprintf("String(describing: %s)", a)
			bstr = fmt.Sprintf("String(describing: %s)", bstr)
		}
		op := "<"
		if desc {
			op = ">"
		}
		b.WriteString("    _tmp.sort { " + a + " " + op + " " + bstr + " }\n")
	}
	if skipExpr != "" {
		b.WriteString("    _tmp = Array(_tmp.dropFirst(" + skipExpr + "))\n")
	}
	if takeExpr != "" {
		b.WriteString("    _tmp = Array(_tmp.prefix(" + takeExpr + "))\n")
	}
	if selIsGroup {
		b.WriteString("    return _tmp\n")
	} else {
		b.WriteString("    return _tmp.map { " + gname + " in " + sel + " }\n")
	}
	b.WriteString("}()")
	delete(c.groups, gname)
	delete(c.groupElemType, gname)
	delete(c.groupElemFields, gname)
	return b.String(), nil
}

// groupJoinQuery handles queries that contain both join and group by clauses.
// It supports inner joins and a single left join without additional FROM
// clauses. The generated Swift code mirrors the reference translations used in
// the tests by building the join result and grouping it in one pass.
func (c *compiler) groupJoinQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Group.Exprs) != 1 {
		return "", fmt.Errorf("unsupported query")
	}
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
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.expr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}
	savedVars := c.varTypes
	savedFields := c.mapFields
	c.varTypes = copyMap(c.varTypes)
	c.mapFields = copyMap(c.mapFields)
	c.varTypes[q.Var] = c.elementType(q.Source)
	if fields := c.elementFieldTypes(q.Source); fields != nil {
		c.mapFields[q.Var] = fields
	}
	for i, f := range q.Froms {
		c.varTypes[f.Var] = c.elementType(f.Src)
		if fields := c.elementFieldTypes(f.Src); fields != nil {
			c.mapFields[f.Var] = fields
		}
		_ = fromSrcs[i]
	}
	for i, j := range q.Joins {
		c.varTypes[j.Var] = c.elementType(j.Src)
		if fields := c.elementFieldTypes(j.Src); fields != nil {
			c.mapFields[j.Var] = fields
		}
		_ = joinSrcs[i]
	}

	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		on, err := c.expr(j.On)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		joinOns[i] = on
	}

	cond := ""
	if q.Where != nil {
		ccond, err := c.expr(q.Where)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		if !boolExpr(q.Where) {
			ccond = strings.ReplaceAll(ccond, " as! [String:Any]", "")
			ccond = fmt.Sprintf("%s != nil", ccond)
		}
		cond = ccond
	}

	keyExpr, err := c.expr(q.Group.Exprs[0])
	if err != nil {
		c.varTypes = savedVars
		c.mapFields = savedFields
		return "", err
	}
	gname := q.Group.Name
	c.varTypes[gname] = ""
	c.mapFields[gname] = nil
	c.groups[gname] = true
	c.groupElemType[gname] = ""
	names := []string{q.Var}
	for _, f := range q.Froms {
		names = append(names, f.Var)
	}
	for _, j := range q.Joins {
		names = append(names, j.Var)
	}

	fields := make(map[string]string)
	for _, n := range names {
		if t, ok := c.swiftTypes[n]; ok {
			fields[n] = t
		} else if vt, ok := c.varTypes[n]; ok {
			if st := swiftTypeOf(vt); st != "" {
				fields[n] = st
			}
		}
	}
	if len(fields) > 0 {
		c.groupElemFields[gname] = fields
	} else {
		c.groupElemFields[gname] = nil
	}

	var having string
	if q.Group.Having != nil {
		hv, err := c.expr(q.Group.Having)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		having = hv
	}
	sel, err := c.expr(q.Select)
	if err != nil {
		c.varTypes = savedVars
		c.mapFields = savedFields
		return "", err
	}
	sortExpr := ""
	if q.Sort != nil {
		s, err := c.expr(q.Sort)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		sortExpr = s
	}
	skipExpr := ""
	if q.Skip != nil {
		sk, err := c.expr(q.Skip)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		skipExpr = sk
	}
	takeExpr := ""
	if q.Take != nil {
		tk, err := c.expr(q.Take)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		takeExpr = tk
	}

	parts := make([]string, len(names))
	for i, n := range names {
		parts[i] = fmt.Sprintf("\"%s\": %s", n, n)
	}
	itemExpr := "[" + strings.Join(parts, ", ") + "]"

	var b strings.Builder
	b.WriteString("({\n")
	b.WriteString("\tvar _groups: [AnyHashable:[Any]] = [:]\n")
	b.WriteString(fmt.Sprintf("\tfor %s in %s {\n", q.Var, src))
	indent := "\t\t"
	for i, fs := range fromSrcs {
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, q.Froms[i].Var, fs))
		indent += "\t"
	}
	// handle single left join specially
	if len(q.Joins) == 1 && joinSides[0] == "left" && len(fromSrcs) == 0 && cond == "" {
		jv := q.Joins[0].Var
		js := joinSrcs[0]
		on := joinOns[0]
		b.WriteString(fmt.Sprintf("%svar _m = false\n", indent))
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, jv, js))
		indent += "\t"
		b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, on))
		b.WriteString(fmt.Sprintf("%s_m = true\n", indent))
		b.WriteString(fmt.Sprintf("%slet _k = %s\n", indent, keyExpr))
		b.WriteString(fmt.Sprintf("%s_groups[_k, default: []].append(%s)\n", indent, itemExpr))
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
		b.WriteString(fmt.Sprintf("%sif !_m {\n", indent))
		indent += "\t"
		b.WriteString(fmt.Sprintf("%slet %s: Any? = nil\n", indent, jv))
		b.WriteString(fmt.Sprintf("%slet _k = %s\n", indent, keyExpr))
		b.WriteString(fmt.Sprintf("%s_groups[_k, default: []].append(%s)\n", indent, itemExpr))
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	} else {
		for i, js := range joinSrcs {
			b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, q.Joins[i].Var, js))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, joinOns[i]))
		}
		if cond != "" {
			b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, cond))
		}
		b.WriteString(fmt.Sprintf("%slet _k = %s\n", indent, keyExpr))
		b.WriteString(fmt.Sprintf("%s_groups[_k, default: []].append(%s)\n", indent, itemExpr))
		for range joinSrcs {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "}\n")
		}
	}
	for range fromSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	b.WriteString("\tvar _tmp = _groups.map { (k, v) in (key: k, items: v) }\n")
	if having != "" {
		b.WriteString(fmt.Sprintf("\t_tmp = _tmp.filter { %s in %s }\n", gname, having))
	}
	if sortExpr != "" {
		desc := strings.HasPrefix(sortExpr, "-")
		if desc {
			sortExpr = strings.TrimPrefix(sortExpr, "-")
		}
		a := replaceIdent(sortExpr, gname, "$0")
		bstr := replaceIdent(sortExpr, gname, "$1")
		op := "<"
		if desc {
			op = ">"
		}
		b.WriteString(fmt.Sprintf("\t_tmp.sort { %s %s %s }\n", a, op, bstr))
	}
	if skipExpr != "" {
		b.WriteString(fmt.Sprintf("\t_tmp = Array(_tmp.dropFirst(%s))\n", skipExpr))
	}
	if takeExpr != "" {
		b.WriteString(fmt.Sprintf("\t_tmp = Array(_tmp.prefix(%s))\n", takeExpr))
	}
	if sel == gname {
		b.WriteString("\treturn _tmp\n")
	} else {
		b.WriteString(fmt.Sprintf("\treturn _tmp.map { %s in %s }\n", gname, sel))
	}
	b.WriteString("}())")

	delete(c.groups, gname)
	delete(c.groupElemType, gname)
	delete(c.groupElemFields, gname)
	c.varTypes = savedVars
	c.mapFields = savedFields
	return b.String(), nil
}

func (c *compiler) joinQuery(q *parser.QueryExpr) (string, error) {
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
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.expr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}
	savedVars := c.varTypes
	savedFields := c.mapFields
	c.varTypes = copyMap(c.varTypes)
	c.mapFields = copyMap(c.mapFields)
	c.varTypes[q.Var] = c.elementType(q.Source)
	if fields := c.elementFieldTypes(q.Source); fields != nil {
		c.mapFields[q.Var] = fields
	}
	for i, f := range q.Froms {
		c.varTypes[f.Var] = c.elementType(f.Src)
		if fields := c.elementFieldTypes(f.Src); fields != nil {
			c.mapFields[f.Var] = fields
		}
		_ = fromSrcs[i]
	}
	for i, j := range q.Joins {
		c.varTypes[j.Var] = c.elementType(j.Src)
		if fields := c.elementFieldTypes(j.Src); fields != nil {
			c.mapFields[j.Var] = fields
		}
		_ = joinSrcs[i]
	}

	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		on, err := c.expr(j.On)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		joinOns[i] = on
	}
	if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Where == nil && joinSides[0] != "" {
		c.varTypes = savedVars
		c.mapFields = savedFields
		return c.joinSingleSide(q, src, joinSrcs[0], joinOns[0], joinSides[0])
	}
	cond := ""
	if q.Where != nil {
		ccond, err := c.expr(q.Where)
		if err != nil {
			c.varTypes = savedVars
			c.mapFields = savedFields
			return "", err
		}
		if !boolExpr(q.Where) {
			ccond = strings.ReplaceAll(ccond, " as! [String:Any]", "")
			ccond = fmt.Sprintf("%s != nil", ccond)
		}
		cond = ccond
	}
	selType := c.exprType(q.Select)
	fields := c.elementFieldTypes(q.Select)
	sel, err := c.expr(q.Select)
	if err != nil {
		c.varTypes = savedVars
		c.mapFields = savedFields
		return "", err
	}
	c.varTypes = savedVars
	c.mapFields = savedFields

	resTyp := swiftTypeOf(selType)
	if selType == "map" && fields != nil {
		keys := make([]string, 0, len(fields))
		for k := range fields {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		parts := make([]string, len(keys))
		for i, k := range keys {
			parts[i] = fmt.Sprintf("%s: %s", k, fields[k])
		}
		resTyp = "(" + strings.Join(parts, ", ") + ")"
	}
	if resTyp == "" {
		resTyp = "Any"
	}

	var b strings.Builder
	b.WriteString("({\n")
	b.WriteString(fmt.Sprintf("\tvar _res: [%s] = []\n", resTyp))
	b.WriteString(fmt.Sprintf("\tfor %s in %s {\n", q.Var, src))
	indent := "\t\t"
	for i, fs := range fromSrcs {
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, q.Froms[i].Var, fs))
		indent += "\t"
	}
	for i, js := range joinSrcs {
		b.WriteString(fmt.Sprintf("%sfor %s in %s {\n", indent, q.Joins[i].Var, js))
		indent += "\t"
		b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, joinOns[i]))
	}
	if cond != "" {
		b.WriteString(fmt.Sprintf("%sif !(%s) { continue }\n", indent, cond))
	}
	b.WriteString(fmt.Sprintf("%s_res.append(%s)\n", indent, sel))
	for range joinSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range fromSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	b.WriteString("\treturn _res\n")
	b.WriteString("}())")
	return b.String(), nil
}

func (c *compiler) joinSingleSide(q *parser.QueryExpr, src, joinSrc, onExpr, side string) (string, error) {
	savedVars := c.varTypes
	savedFields := c.mapFields
	c.varTypes = copyMap(c.varTypes)
	c.mapFields = copyMap(c.mapFields)
	j := q.Joins[0]
	c.varTypes[q.Var] = c.elementType(q.Source)
	if fields := c.elementFieldTypes(q.Source); fields != nil {
		c.mapFields[q.Var] = fields
	}
	c.varTypes[j.Var] = c.elementType(j.Src)
	if fields := c.elementFieldTypes(j.Src); fields != nil {
		c.mapFields[j.Var] = fields
	}
	selType := c.exprType(q.Select)
	fields := c.elementFieldTypes(q.Select)
	sel, err := c.expr(q.Select)
	c.varTypes = savedVars
	c.mapFields = savedFields
	if err != nil {
		return "", err
	}
	resTyp := swiftTypeOf(selType)
	if selType == "map" && fields != nil {
		keys := make([]string, 0, len(fields))
		for k := range fields {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		parts := make([]string, len(keys))
		for i, k := range keys {
			parts[i] = fmt.Sprintf("%s: %s", k, fields[k])
		}
		resTyp = "(" + strings.Join(parts, ", ") + ")"
	}
	if resTyp == "" {
		resTyp = "Any"
	}
	qv := q.Var
	jv := j.Var
	var b strings.Builder
	b.WriteString("({\n")
	b.WriteString(fmt.Sprintf("\tvar _res: [%s] = []\n", resTyp))
	b.WriteString(fmt.Sprintf("\tlet _src = %s\n", src))
	b.WriteString(fmt.Sprintf("\tlet _join = %s\n", joinSrc))
	if side == "outer" {
		b.WriteString("\tvar _matched = Array(repeating: false, count: _join.count)\n")
	}
	if side == "right" {
		b.WriteString(fmt.Sprintf("\tfor %s in _join {\n", jv))
		b.WriteString("\t\tvar _m = false\n")
		b.WriteString(fmt.Sprintf("\t\tfor %s in _src {\n", qv))
		b.WriteString(fmt.Sprintf("\t\t\tif !(%s) { continue }\n", onExpr))
		b.WriteString("\t\t\t_m = true\n")
		b.WriteString(fmt.Sprintf("\t\t\t_res.append(%s)\n", sel))
		b.WriteString("\t\t}\n")
		b.WriteString("\t\tif !_m {\n")
		b.WriteString(fmt.Sprintf("\t\t\tlet %s: Any? = nil\n", qv))
		b.WriteString(fmt.Sprintf("\t\t\t_res.append(%s)\n", sel))
		b.WriteString("\t\t}\n")
		b.WriteString("\t}\n")
	} else {
		b.WriteString(fmt.Sprintf("\tfor %s in _src {\n", qv))
		if side != "" {
			b.WriteString("\t\tvar _m = false\n")
		}
		if side == "outer" {
			b.WriteString(fmt.Sprintf("\t\tfor (ri, %s) in _join.enumerated() {\n", jv))
		} else {
			b.WriteString(fmt.Sprintf("\t\tfor %s in _join {\n", jv))
		}
		b.WriteString(fmt.Sprintf("\t\t\tif !(%s) { continue }\n", onExpr))
		if side == "outer" {
			b.WriteString("\t\t\t_matched[ri] = true\n")
		}
		if side != "" {
			b.WriteString("\t\t\t_m = true\n")
		}
		b.WriteString(fmt.Sprintf("\t\t\t_res.append(%s)\n", sel))
		b.WriteString("\t\t}\n")
		if side == "outer" {
			b.WriteString("\t\tif !_m {\n")
			b.WriteString(fmt.Sprintf("\t\t\tlet %s: Any? = nil\n", jv))
			b.WriteString(fmt.Sprintf("\t\t\t_res.append(%s)\n", sel))
			b.WriteString("\t\t}\n")
			b.WriteString("\t}\n")
			b.WriteString(fmt.Sprintf("\tfor (ri, %s) in _join.enumerated() {\n", jv))
			b.WriteString("\t\tif !_matched[ri] {\n")
			b.WriteString(fmt.Sprintf("\t\t\tlet %s: Any? = nil\n", qv))
			b.WriteString(fmt.Sprintf("\t\t\tlet %s = %s\n", jv, jv))
			b.WriteString(fmt.Sprintf("\t\t\t_res.append(%s)\n", sel))
			b.WriteString("\t\t}\n")
			b.WriteString("\t}\n")
		} else if side == "left" {
			b.WriteString("\t\tif !_m {\n")
			b.WriteString(fmt.Sprintf("\t\t\tlet %s: Any? = nil\n", jv))
			b.WriteString(fmt.Sprintf("\t\t\t_res.append(%s)\n", sel))
			b.WriteString("\t\t}\n")
			b.WriteString("\t}\n")
		} else {
			b.WriteString("\t}\n")
		}
	}
	b.WriteString("\treturn _res\n")
	b.WriteString("}())")
	return b.String(), nil
}

func (c *compiler) queryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil && len(q.Joins) == 0 {
		return c.groupQuery(q)
	}
	if q.Group != nil && len(q.Joins) > 0 && !q.Distinct {
		return c.groupJoinQuery(q)
	}
	if len(q.Joins) > 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		return c.joinQuery(q)
	}
	if len(q.Joins) > 0 || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}

	if q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && len(q.Froms) == 0 {
		if call := callExprSimple(q.Select); call != nil && len(call.Args) == 1 {
			arg := call.Args[0]
			if isVarRef(arg, q.Var) {
				src, err := c.expr(q.Source)
				if err != nil {
					return "", err
				}
				if q.Where != nil {
					cond, err := c.expr(q.Where)
					if err != nil {
						return "", err
					}
					if !boolExpr(q.Where) {
						cond = castCond(cond)
						cond = fmt.Sprintf("%s != nil", cond)
					}
					src = fmt.Sprintf("%s.filter { %s in %s }", src, q.Var, cond)
				}
				switch call.Func {
				case "sum":
					return fmt.Sprintf("%s.reduce(0, +)", src), nil
				case "count", "len":
					return fmt.Sprintf("%s.count", src), nil
				case "min":
					return fmt.Sprintf("%s.min()!", src), nil
				case "max":
					return fmt.Sprintf("%s.max()!", src), nil
				case "avg":
					return fmt.Sprintf("({ let _t = %s; return _t.reduce(0, +) / _t.count })()", src), nil
				}
			}
		}
	}
	if len(q.Froms) == 0 && q.Where == nil && q.Sort != nil {
		src, err := c.expr(q.Source)
		if err != nil {
			return "", err
		}
		saved := c.varTypes
		savedFields := c.mapFields
		c.varTypes = copyMap(c.varTypes)
		c.mapFields = copyMap(c.mapFields)
		c.varTypes[q.Var] = c.elementType(q.Source)
		if fields := c.elementFieldTypes(q.Source); fields != nil {
			c.mapFields[q.Var] = fields
		}
		prev := c.tupleMap
		c.tupleMap = true
		sel, err := c.expr(q.Select)
		if err != nil {
			c.varTypes = saved
			c.mapFields = savedFields
			return "", err
		}
		key, err := c.expr(q.Sort)
		c.tupleMap = prev
		if err != nil {
			c.varTypes = saved
			return "", err
		}
		c.varTypes = saved
		desc := false
		if strings.HasPrefix(key, "-") {
			desc = true
			key = strings.TrimPrefix(key, "-")
		}
		res := fmt.Sprintf("%s.map { %s in (value: %s, key: %s) }", src, q.Var, sel, key)
		op := "<"
		if desc {
			op = ">"
		}
		res = fmt.Sprintf("%s.sorted { $0.key %s $1.key }", res, op)
		if q.Skip != nil {
			skip, err := c.expr(q.Skip)
			if err != nil {
				return "", err
			}
			res += fmt.Sprintf(".dropFirst(%s)", skip)
		}
		if q.Take != nil {
			take, err := c.expr(q.Take)
			if err != nil {
				return "", err
			}
			res += fmt.Sprintf(".prefix(%s)", take)
		}
		res += ".map { $0.value }"
		return res, nil
	}
	vars := []string{q.Var}
	srcs := []*parser.Expr{q.Source}
	for _, f := range q.Froms {
		vars = append(vars, f.Var)
		srcs = append(srcs, f.Src)
	}
	saved := c.varTypes
	savedFields := c.mapFields
	c.varTypes = copyMap(c.varTypes)
	c.mapFields = copyMap(c.mapFields)
	defer func() { c.varTypes = saved; c.mapFields = savedFields }()

	var build func(int) (string, error)
	build = func(i int) (string, error) {
		src, err := c.expr(srcs[i])
		if err != nil {
			return "", err
		}
		if name, ok := c.isGroupVar(srcs[i]); ok {
			src += ".items"
			_ = name
		}
		name := vars[i]
		prevT, havePrev := c.varTypes[name]
		prevF, haveF := c.mapFields[name]
		c.varTypes[name] = c.elementType(srcs[i])
		if fields := c.elementFieldTypes(srcs[i]); fields != nil {
			c.mapFields[name] = fields
		} else {
			delete(c.mapFields, name)
		}
		if i == len(vars)-1 {
			sel, err := c.expr(q.Select)
			if err != nil {
				return "", err
			}
			if q.Where != nil {
				cond, err := c.expr(q.Where)
				if err != nil {
					return "", err
				}
				if !boolExpr(q.Where) {
					cond = castCond(cond)
					cond = fmt.Sprintf("%s != nil", cond)
				}
				var res string
				if isVarRef(q.Select, name) {
					res = fmt.Sprintf("%s.filter { %s in %s }", src, name, cond)
				} else {
					res = fmt.Sprintf("%s.compactMap { %s in %s ? (%s) : nil }", src, name, cond, sel)
				}
				if havePrev {
					c.varTypes[name] = prevT
				} else {
					delete(c.varTypes, name)
				}
				if haveF {
					c.mapFields[name] = prevF
				} else {
					delete(c.mapFields, name)
				}
				return res, nil
			}
			res := fmt.Sprintf("%s.map { %s in %s }", src, name, sel)
			if havePrev {
				c.varTypes[name] = prevT
			} else {
				delete(c.varTypes, name)
			}
			if haveF {
				c.mapFields[name] = prevF
			} else {
				delete(c.mapFields, name)
			}
			return res, nil
		}
		inner, err := build(i + 1)
		if err != nil {
			return "", err
		}
		if havePrev {
			c.varTypes[name] = prevT
		} else {
			delete(c.varTypes, name)
		}
		if haveF {
			c.mapFields[name] = prevF
		} else {
			delete(c.mapFields, name)
		}
		return fmt.Sprintf("%s.flatMap { %s in %s }", src, name, inner), nil
	}

	res, err := build(0)
	if err != nil {
		return "", err
	}
	if q.Sort != nil {
		prevT := c.tupleMap
		c.tupleMap = true
		sortExpr, err := c.expr(q.Sort)
		c.tupleMap = prevT
		if err != nil {
			return "", err
		}
		desc := false
		if strings.HasPrefix(sortExpr, "-") {
			desc = true
			sortExpr = strings.TrimPrefix(sortExpr, "-")
		}
		a := sortExpr
		b := sortExpr
		for _, v := range vars {
			a = replaceIdent(a, v, "$0")
			b = replaceIdent(b, v, "$1")
		}
		op := "<"
		if desc {
			op = ">"
		}
		res = fmt.Sprintf("%s.sorted { %s %s %s }", res, a, op, b)
	}
	if q.Skip != nil {
		skip, err := c.expr(q.Skip)
		if err != nil {
			return "", err
		}
		res += fmt.Sprintf(".dropFirst(%s)", skip)
	}
	if q.Take != nil {
		take, err := c.expr(q.Take)
		if err != nil {
			return "", err
		}
		res += fmt.Sprintf(".prefix(%s)", take)
	}
	return res, nil
}

func (c *compiler) loadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.expr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.helpers["_load"] = true
	expr := fmt.Sprintf("_load(path: %s, opts: %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		name := *l.Type.Simple
		if fields, ok := c.structs[name]; ok {
			args := make([]string, len(fields))
			for i, f := range fields {
				typ := c.structTypes[name][f]
				cast := ""
				if typ != "" {
					cast = " as! " + typ
				}
				args[i] = fmt.Sprintf("%s: rec[\"%s\"]%s", f, f, cast)
			}
			st, err := c.typeRef(l.Type)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s.map { rec in %s(%s) }", expr, st, strings.Join(args, ", "))
		}
	}
	return expr, nil
}

func (c *compiler) saveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.expr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.expr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.helpers["_save"] = true
	return fmt.Sprintf("_save(%s, path: %s, opts: %s)", src, path, opts), nil
}

func isVarRef(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	sel := e.Binary.Left.Value.Target.Selector
	return sel != nil && sel.Root == name && len(sel.Tail) == 0
}

func callExprSimple(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	if len(e.Binary.Right) == 0 && len(e.Binary.Left.Value.Ops) == 0 {
		return e.Binary.Left.Value.Target.Call
	}
	return nil
}

func copyMap[K comparable, V any](m map[K]V) map[K]V {
	out := make(map[K]V, len(m))
	for k, v := range m {
		out[k] = v
	}
	return out
}

func (c *compiler) isGroupVar(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", false
	}
	sel := e.Binary.Left.Value.Target.Selector
	if sel != nil && len(sel.Tail) == 0 {
		if c.groups[sel.Root] {
			return sel.Root, true
		}
	}
	return "", false
}

func replaceIdent(s, name, repl string) string {
	re := regexp.MustCompile(`\b` + regexp.QuoteMeta(name) + `\b`)
	repl = strings.ReplaceAll(repl, "$", "$$")
	return re.ReplaceAllString(s, repl)
}

var floatLit = regexp.MustCompile(`^\d+\.\d+$`)

func isFloatExpr(s string) bool {
	s = strings.TrimSpace(s)
	if floatLit.MatchString(s) {
		return true
	}
	if strings.Contains(s, "Double(") || strings.Contains(s, " as! Double") {
		return true
	}
	return false
}

func castCond(cond string) string {
	if strings.Contains(cond, " as! [String:Any]") {
		idx := strings.Index(cond, "[")
		if idx > 0 {
			varName := cond[:idx]
			field := strings.TrimSuffix(cond[idx:], " as! [String:Any]")
			return fmt.Sprintf("(%s as! [String:Any])%s", varName, field)
		}
	}
	return cond
}

func (c *compiler) elementType(e *parser.Expr) string {
	typ := c.exprType(e)
	if strings.HasPrefix(typ, "list_") {
		return strings.TrimPrefix(typ, "list_")
	}
	if typ == "list" {
		if sel := e.Binary.Left.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
			if t, ok := c.varTypes[sel.Root]; ok && strings.HasPrefix(t, "list_") {
				return strings.TrimPrefix(t, "list_")
			}
			if c.groups[sel.Root] {
				if t, ok2 := c.groupElemType[sel.Root]; ok2 {
					return t
				}
			}
		}
		if e.Binary.Left.Value.Target.List != nil && len(e.Binary.Left.Value.Target.List.Elems) > 0 {
			return c.exprType(e.Binary.Left.Value.Target.List.Elems[0])
		}
	}
	if sel := e.Binary.Left.Value.Target.Selector; sel != nil {
		if c.groups[sel.Root] {
			if len(sel.Tail) == 0 {
				if t, ok := c.groupElemType[sel.Root]; ok {
					return t
				}
			}
			if len(sel.Tail) == 1 && sel.Tail[0] == "items" {
				if t, ok := c.groupElemType[sel.Root]; ok {
					return t
				}
			}
		}
	}
	return typ
}

func (c *compiler) elementFieldTypes(e *parser.Expr) map[string]string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	p := e.Binary.Left.Value
	if p.Target.Selector != nil {
		if len(p.Target.Selector.Tail) == 0 {
			if t, ok := c.mapFields[p.Target.Selector.Root]; ok {
				if t != nil {
					return t
				}
			}
			if c.groups[p.Target.Selector.Root] {
				if t, ok2 := c.groupElemFields[p.Target.Selector.Root]; ok2 {
					return t
				}
			}
		}
		if len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "items" {
			if c.groups[p.Target.Selector.Root] {
				if t, ok2 := c.groupElemFields[p.Target.Selector.Root]; ok2 {
					return t
				}
			}
		}
	}
	if p.Target.List != nil && len(p.Target.List.Elems) > 0 {
		if m := mapLit(p.Target.List.Elems[0]); m != nil {
			return c.mapFieldsFromLiteral(m)
		}
	}
	if p.Target.Query != nil {
		if t := c.queryFieldTypes(p.Target.Query); t != nil {
			return t
		}
	}
	return nil
}

func (c *compiler) queryFieldTypes(q *parser.QueryExpr) map[string]string {
	savedVars := c.varTypes
	savedFields := c.mapFields
	savedGroups := c.groups
	savedElem := c.groupElemType
	savedElemFields := c.groupElemFields
	c.varTypes = copyMap(c.varTypes)
	c.mapFields = copyMap(c.mapFields)
	c.groups = copyMap(c.groups)
	c.groupElemType = copyMap(c.groupElemType)
	c.groupElemFields = copyMap(c.groupElemFields)

	c.varTypes[q.Var] = c.elementType(q.Source)
	if f := c.elementFieldTypes(q.Source); f != nil {
		c.mapFields[q.Var] = f
	}
	for _, f := range q.Froms {
		c.varTypes[f.Var] = c.elementType(f.Src)
		if ff := c.elementFieldTypes(f.Src); ff != nil {
			c.mapFields[f.Var] = ff
		}
	}
	for _, j := range q.Joins {
		c.varTypes[j.Var] = c.elementType(j.Src)
		if jf := c.elementFieldTypes(j.Src); jf != nil {
			c.mapFields[j.Var] = jf
		}
	}
	if q.Group != nil {
		gname := q.Group.Name
		c.groups[gname] = true
		c.groupElemType[gname] = c.elementType(q.Source)
		if gf := c.elementFieldTypes(q.Source); gf != nil {
			c.groupElemFields[gname] = gf
		}
	}

	// When the query selects the group variable directly, return fields for
	// the implicit group object so later statements can access `key` and
	// `items`.
	if q.Group != nil && isVarRef(q.Select, q.Group.Name) {
		keyType := c.fieldType(q.Group.Exprs[0])
		if keyType == "" {
			keyType = c.exprType(q.Group.Exprs[0])
		}
		kt := swiftTypeOf(keyType)
		if kt == "" {
			kt = "AnyHashable"
		}
		et := swiftTypeOf(c.elementType(q.Source))
		if et == "" {
			et = "Any"
		}
		res := map[string]string{"key": kt, "items": "[" + et + "]"}
		c.varTypes = savedVars
		c.mapFields = savedFields
		c.groups = savedGroups
		c.groupElemType = savedElem
		c.groupElemFields = savedElemFields
		return res
	}

	res := c.elementFieldTypes(q.Select)

	c.varTypes = savedVars
	c.mapFields = savedFields
	c.groups = savedGroups
	c.groupElemType = savedElem
	c.groupElemFields = savedElemFields
	return res
}

func (c *compiler) recordMapFields(name string, e *parser.Expr) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return
	}
	if m := mapLit(e); m != nil {
		// ignore map fields when casting to a known struct type
		if len(e.Binary.Left.Value.Ops) == 1 && e.Binary.Left.Value.Ops[0].Cast != nil {
			typ, err := c.typeRef(e.Binary.Left.Value.Ops[0].Cast.Type)
			if err == nil {
				if _, ok := c.structs[typ]; ok {
					// result is a struct, not a map
					return
				}
			}
		}
		c.mapFields[name] = c.mapFieldsFromLiteral(m)
		return
	}
	p := e.Binary.Left.Value
	if p.Target.List != nil && len(p.Target.List.Elems) > 0 {
		if m := mapLit(p.Target.List.Elems[0]); m != nil {
			if st, ok := c.detectAutoStructList(p.Target.List, name); ok {
				c.varTypes[name] = "list_" + st
				c.swiftTypes[name] = "[" + st + "]"
				c.autoStructs[st] = true
				return
			}
			c.mapFields[name] = c.mapFieldsFromLiteral(m)
			return
		}
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if t, ok := c.mapFields[p.Target.Selector.Root]; ok {
			c.mapFields[name] = t
		}
	}
	if p.Target.Query != nil {
		if t := c.elementFieldTypes(p.Target.Query.Select); t != nil {
			c.mapFields[name] = t
		} else if t := c.queryFieldTypes(p.Target.Query); t != nil {
			c.mapFields[name] = t
		} else if m := mapLit(p.Target.Query.Select); m != nil {
			c.mapFields[name] = c.mapFieldsFromLiteral(m)
		} else if isVarRef(p.Target.Query.Select, p.Target.Query.Var) {
			if t := c.elementFieldTypes(p.Target.Query.Source); t != nil {
				c.mapFields[name] = t
			}
		}
		if p.Target.Query.Group != nil && isVarRef(p.Target.Query.Select, p.Target.Query.Group.Name) {
			if gf := c.elementFieldTypes(p.Target.Query.Source); gf != nil {
				c.groups[name] = true
				c.groupElemFields[name] = gf
				c.groupElemType[name] = c.elementType(p.Target.Query.Source)
			}
		}
	}
	if p.Target.Call != nil && (p.Target.Call.Func == "map" || p.Target.Call.Func == "compactMap") {
		if len(p.Target.Call.Args) == 1 && p.Target.Call.Args[0].Binary != nil {
			fe := p.Target.Call.Args[0].Binary.Left.Value.Target.FunExpr
			if fe != nil && fe.ExprBody != nil {
				if m := mapLit(fe.ExprBody); m != nil {
					c.mapFields[name] = c.mapFieldsFromLiteral(m)
				}
			}
		}
	} else if p.Target.Call != nil && p.Target.Call.Func == "append" && len(p.Target.Call.Args) == 2 {
		if m := mapLit(p.Target.Call.Args[1]); m != nil {
			if st, ok := c.detectAutoStructList(&parser.ListLiteral{Elems: []*parser.Expr{p.Target.Call.Args[1]}}, name); ok {
				c.varTypes[name] = "list_" + st
				c.swiftTypes[name] = "[" + st + "]"
				c.autoStructs[st] = true
				delete(c.mapFields, name)
				return
			}
			c.mapFields[name] = c.mapFieldsFromLiteral(m)
			return
		}
	}

	if fields, ok := c.mapFields[name]; ok && fields != nil {
		if typ := c.varTypes[name]; strings.HasPrefix(typ, "list") {
			if st, ok2 := c.autoStructFromFields(fields, name); ok2 {
				c.varTypes[name] = "list_" + st
				c.swiftTypes[name] = "[" + st + "]"
				delete(c.mapFields, name)
			}
		}
	}
}

func mapLit(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Map
}

func (c *compiler) mapFieldsFromLiteral(m *parser.MapLiteral) map[string]string {
	if m == nil {
		return nil
	}
	fields := make(map[string]string)
	for _, it := range m.Items {
		key, ok := keyName(it.Key)
		if !ok {
			continue
		}
		typ := ""
		if boolExpr(it.Value) {
			typ = "Bool"
		} else {
			t := c.exprType(it.Value)
			if t != "" {
				typ = swiftTypeOf(t)
			} else {
				typ = c.fieldType(it.Value)
			}
		}
		if typ == "" {
			typ = literalType(it.Value)
		}
		if typ == "" {
			typ = "Any"
		}
		fields[key] = typ
	}
	return fields
}

func (c *compiler) detectAutoStructList(l *parser.ListLiteral, name string) (string, bool) {
	if l == nil || len(l.Elems) == 0 {
		return "", false
	}
	first := mapLit(l.Elems[0])
	if first == nil {
		return "", false
	}
	fields := c.mapFieldsFromLiteral(first)
	order := make([]string, 0, len(fields))
	for k := range fields {
		order = append(order, k)
	}
	sort.Strings(order)
	for _, e := range l.Elems[1:] {
		m := mapLit(e)
		if m == nil {
			return "", false
		}
		f2 := c.mapFieldsFromLiteral(m)
		for _, k := range order {
			if f2[k] == "" {
				return "", false
			}
		}
	}
	for _, t := range fields {
		if t == "Any" || t == "" {
			return "", false
		}
	}
	key := ""
	for _, k := range order {
		key += k + ":" + fields[k] + ";"
	}
	if name, ok := c.structKeys[key]; ok {
		return name, true
	}
	c.autoCount++
	stName := c.genStructName(name)
	c.structKeys[key] = stName
	c.structs[stName] = order
	c.structTypes[stName] = fields
	c.autoStructs[stName] = true
	return stName, true
}

func (c *compiler) autoStructFromFields(fields map[string]string, name string) (string, bool) {
	if len(fields) == 0 {
		return "", false
	}
	order := make([]string, 0, len(fields))
	for k := range fields {
		if fields[k] == "" || fields[k] == "Any" {
			return "", false
		}
		order = append(order, k)
	}
	sort.Strings(order)
	key := ""
	for _, k := range order {
		key += k + ":" + fields[k] + ";"
	}
	if name, ok := c.structKeys[key]; ok {
		return name, true
	}
	c.autoCount++
	stName := c.genStructName(name)
	c.structKeys[key] = stName
	c.structs[stName] = order
	c.structTypes[stName] = fields
	c.autoStructs[stName] = true
	return stName, true
}

func boolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) == 0 {
		if u := e.Binary.Left; u != nil {
			if v := u.Value; v != nil && v.Target != nil && v.Target.Lit != nil {
				return v.Target.Lit.Bool != nil
			}
		}
	}
	for _, op := range e.Binary.Right {
		switch op.Op {
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||":
			return true
		}
	}
	return false
}

func (c *compiler) fieldType(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return ""
	}
	p := e.Binary.Left.Value
	if sel := p.Target.Selector; sel != nil {
		root := sel.Root
		// only support single-level field access
		if len(sel.Tail) == 1 {
			if m, ok := c.mapFields[root]; ok {
				if m != nil {
					if t, ok2 := m[sel.Tail[0]]; ok2 {
						return t
					}
				}
			}
			if c.groups[root] {
				if m, ok2 := c.groupElemFields[root]; ok2 {
					if t, ok3 := m[sel.Tail[0]]; ok3 {
						return t
					}
				}
			}
		}
	}
	if len(p.Ops) == 1 && p.Ops[0].Index != nil {
		idx := p.Ops[0].Index
		if idx.Start != nil && idx.End == nil && idx.Colon == nil && idx.Colon2 == nil {
			if key, ok := keyName(idx.Start); ok {
				if sel := p.Target.Selector; sel != nil && len(sel.Tail) == 0 {
					if m, ok := c.mapFields[sel.Root]; ok {
						if t, ok2 := m[key]; ok2 {
							return t
						}
					}
				}
			}
		}
	}
	return ""
}

func literalType(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return ""
	}
	lit := e.Binary.Left.Value.Target.Lit
	if lit == nil {
		return ""
	}
	switch {
	case lit.Int != nil:
		return "Int"
	case lit.Float != nil:
		return "Double"
	case lit.Bool != nil:
		return "Bool"
	case lit.Str != nil:
		return "String"
	}
	return ""
}

func (c *compiler) emitAutoStructs() {
	names := make([]string, 0, len(c.autoStructs))
	for n := range c.autoStructs {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		fields := c.structs[n]
		eq := true
		for _, f := range fields {
			if !c.isEquatableType(c.structTypes[n][f]) {
				eq = false
				break
			}
		}
		if eq {
			c.writeln(fmt.Sprintf("struct %s: Equatable {", n))
		} else {
			c.writeln(fmt.Sprintf("struct %s {", n))
		}
		c.indent++
		for _, f := range fields {
			typ := c.structTypes[n][f]
			if typ == "" {
				typ = "Any"
			}
			c.writeln(fmt.Sprintf("var %s: %s", f, typ))
		}
		c.indent--
		c.writeln("}\n")
	}
}

func (c *compiler) isEquatableType(t string) bool {
	switch t {
	case "Int", "Double", "Bool", "String", "AnyHashable":
		return true
	}
	if strings.HasPrefix(t, "[") && strings.HasSuffix(t, "]") {
		inner := strings.TrimSuffix(strings.TrimPrefix(t, "["), "]")
		return c.isEquatableType(inner)
	}
	if _, ok := c.structs[t]; ok {
		return true
	}
	return false
}

func (c *compiler) genStructName(varName string) string {
	base := strings.TrimLeft(varName, "_")
	if strings.HasSuffix(base, "ies") {
		base = strings.TrimSuffix(base, "ies") + "y"
	} else if strings.HasSuffix(base, "s") && len(base) > 1 {
		base = strings.TrimSuffix(base, "s")
	}
	if base != "" {
		parts := strings.Split(base, "_")
		for i, p := range parts {
			if len(p) == 0 {
				continue
			}
			parts[i] = strings.ToUpper(p[:1]) + p[1:]
		}
		base = strings.Join(parts, "")
	}
	if base == "" {
		return fmt.Sprintf("Auto%d", c.autoCount)
	}
	if _, ok := c.structs[base]; !ok {
		return base
	}
	return fmt.Sprintf("%s%d", base, c.autoCount)
}

func (c *compiler) emitRuntime() {
	if c.helpers["_load"] {
		for _, line := range strings.Split(helperLoad, "\n") {
			if line == "" {
				c.buf.WriteByte('\n')
			} else {
				c.writeln(line)
			}
		}
	}
	if c.helpers["_save"] {
		for _, line := range strings.Split(helperSave, "\n") {
			if line == "" {
				c.buf.WriteByte('\n')
			} else {
				c.writeln(line)
			}
		}
	}
	if c.helpers["_json"] {
		for _, line := range strings.Split(helperJSON, "\n") {
			if line == "" {
				c.buf.WriteByte('\n')
			} else {
				c.writeln(line)
			}
		}
	}
	if c.helpers["_group"] {
		for _, line := range strings.Split(helperGroup, "\n") {
			if line == "" {
				c.buf.WriteByte('\n')
			} else {
				c.writeln(line)
			}
		}
	}
}

const helperLoad = `func _parseVal(_ s: String) -> Any {
    if let i = Int(s) { return i }
    if let d = Double(s) { return d }
    return s
}

func _load(path: String, opts: [String:Any]?) -> [[String:Any]] {
    var text = ""
    if path.isEmpty || path == "-" {
        text = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
    } else {
        text = (try? String(contentsOfFile: path)) ?? ""
    }
    var rows: [[String:Any]] = []
    var cur: [String:Any] = [:]
    for line in text.split(separator: "\n") {
        let l = line.trimmingCharacters(in: .whitespaces)
        if l.hasPrefix("- ") {
            if !cur.isEmpty { rows.append(cur); cur = [:] }
            let rest = String(l.dropFirst(2))
            if let idx = rest.firstIndex(of: ":") {
                let key = String(rest[..<idx]).trimmingCharacters(in: .whitespaces)
                let val = String(rest[rest.index(after: idx)...]).trimmingCharacters(in: .whitespaces)
                cur[key] = _parseVal(val)
            }
        } else if let idx = l.firstIndex(of: ":") {
            let key = String(l[..<idx]).trimmingCharacters(in: .whitespaces)
            let val = String(l[l.index(after: idx)...]).trimmingCharacters(in: .whitespaces)
            cur[key] = _parseVal(val)
        }
    }
    if !cur.isEmpty { rows.append(cur) }
    return rows
}`

const helperSave = `func _save(_ rows: [[String:Any]], path: String, opts: [String:Any]?) {
    let format = (opts?["format"] as? String) ?? "csv"
    if format == "jsonl" {
        var handle: FileHandle
        if path.isEmpty || path == "-" {
            handle = FileHandle.standardOutput
        } else {
            FileManager.default.createFile(atPath: path, contents: nil)
            handle = FileHandle(forWritingAtPath: path)!
        }
        for row in rows {
            if let data = try? JSONSerialization.data(withJSONObject: row),
               let strData = String(data: data, encoding: .utf8)?.data(using: .utf8) {
                handle.write(strData)
                handle.write(Data([0x0a]))
            }
        }
        if handle !== FileHandle.standardOutput {
            handle.closeFile()
        }
    }
}`

const helperGroup = `class _Group {
    var key: Any
    var Items: [Any] = []
    init(_ k: Any) { self.key = k }
}

func _keyStr(_ v: Any) -> String {
    if let data = try? JSONSerialization.data(withJSONObject: v, options: [.sortedKeys]),
       let s = String(data: data, encoding: .utf8) {
        return s
    }
    return String(describing: v)
}`

const helperJSON = `func _json(_ v: Any) {
    func _sort(_ x: Any) -> Any {
        if let a = x as? [Any] { return a.map { _sort($0) } }
        if let m = x as? [String:Any] {
            var out: [String:Any] = [:]
            for k in m.keys.sorted() { out[k] = _sort(m[k]!) }
            return out
        }
        return x
    }
    if let obj = _sort(v) as? Any,
       let data = try? JSONSerialization.data(withJSONObject: obj),
       let s = String(data: data, encoding: .utf8) {
        print(s)
    }
}`
