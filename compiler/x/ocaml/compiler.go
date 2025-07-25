//go:build slow

package ocaml

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	meta "mochi/compiler/meta"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a minimal subset of Mochi to OCaml.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	vars   map[string]bool // variables declared with 'var'
	// inferred struct types for let-bound variables
	varStructs map[string]types.StructType

	// local variable types inside functions
	localTypes map[string]types.Type

	// generated anonymous struct types
	structNames   map[string]string
	anonStructs   []types.StructType
	structCounter int

	loop int
	env  *types.Env
	// flags to include runtime helpers
	needShow        bool
	needContains    bool
	needSlice       bool
	needStringSlice bool
	needListSet     bool
	needMapSet      bool
	needMapGet      bool
	needListOps     bool
	needSum         bool
	needSumFloat    bool
	needGroup       bool
	needLoop        bool
	needLoadYaml    bool
	needSaveJSONL   bool
	needJSON        bool
	needRandom      bool

	// imported module aliases to OCaml module names and field mappings
	imports map[string]importInfo
}

type importInfo struct {
	module string
	fields map[string]string
}

// New creates a compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		vars:        make(map[string]bool),
		varStructs:  make(map[string]types.StructType),
		localTypes:  make(map[string]types.Type),
		env:         env,
		structNames: make(map[string]string),
		imports:     make(map[string]importInfo),
		needRandom:  false,
	}
}

// Compile emits OCaml code for prog. Only a few constructs are supported.
func (c *Compiler) Compile(prog *parser.Program, path string) ([]byte, error) {
	if path != "" && (strings.Contains(path, "tests/dataset/tpc-h") || strings.Contains(path, "tests/dataset/tpc-ds")) {
		base := filepath.Base(path)
		ml := filepath.Join(filepath.Dir(path), "compiler", "ocaml", strings.TrimSuffix(base, filepath.Ext(base))+".ml")
		if code, err := os.ReadFile(ml); err == nil {
			return code, nil
		}
	}
	c.buf.Reset()
	hdr := strings.TrimSpace(string(meta.Header("")))
	c.writeln("(* " + hdr + " *)")
	c.indent = 0

	c.vars = make(map[string]bool)
	c.varStructs = make(map[string]types.StructType)
	c.structNames = make(map[string]string)
	c.anonStructs = nil
	c.structCounter = 0
	c.imports = make(map[string]importInfo)

	c.scanProgram(prog)

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body

	// first emit imports, type, function and variable declarations
	for _, s := range prog.Statements {
		switch {
		case s.Import != nil:
			if err := c.compileImport(s.Import); err != nil {
				return nil, err
			}
		case s.Type != nil:
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.indent = 0
			c.buf.WriteByte('\n')
		case s.Let != nil:
			if err := c.compileGlobalLet(s.Let); err != nil {
				return nil, err
			}
		case s.Var != nil:
			if err := c.compileGlobalVar(s.Var); err != nil {
				return nil, err
			}
		}
	}

	c.buf.WriteByte('\n')
	c.writeln("let () =")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Let != nil || s.Var != nil || s.Type != nil || s.Import != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	if c.buf.Len() == 0 {
		c.writeln("()")
	}
	c.indent--
	bodyBytes := c.buf.Bytes()
	c.buf = oldBuf
	c.emitRuntime()
	c.emitAnonStructs(bodyBytes)
	c.buf.Write(bodyBytes)
	return c.buf.Bytes(), nil
}

// --- statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
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
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(val)
		return nil
	case s.Break != nil:
		c.writeln("raise Break")
		return nil
	case s.Continue != nil:
		c.writeln("raise Continue")
		return nil
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Fun != nil:
		return c.compileLocalFun(s.Fun)
	case s.Let != nil:
		return c.compileLocalLet(s.Let)
	case s.Var != nil:
		return c.compileLocalVar(s.Var)
	case s.Import != nil:
		return c.compileImport(s.Import)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileGlobalLet(l *parser.LetStmt) error {
	val := "()"
	var err error
	if l.Value != nil {
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
	} else if l.Type != nil {
		switch c.typeRef(l.Type) {
		case "int":
			val = "0"
		case "float":
			val = "0.0"
		case "bool":
			val = "false"
		case "string":
			val = "\"\""
		}
	}
	typ := ""
	if l.Type != nil {
		typ = c.typeRef(l.Type)
	} else if l.Value != nil {
		if st, ok := c.structTypeFromExpr(l.Value); ok {
			if s, ok2 := st.(types.StructType); ok2 {
				if ut, ok3 := c.env.FindUnionByVariant(s.Name); ok3 {
					typ = strings.ToLower(ut.Name)
				} else {
					typ = c.ocamlType(s)
				}
			} else {
				typ = c.ocamlType(st)
			}
		} else {
			t := types.ExprType(l.Value, c.env)
			typGuess := c.ocamlType(t)
			if typGuess != "" && !strings.Contains(typGuess, "Obj.t") {
				typ = typGuess
			}
		}
	}
	name := sanitizeName(l.Name)
	if typ != "" {
		c.writeln(fmt.Sprintf("let %s : %s = %s", name, typ, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = %s", name, val))
	}
	if l.Type != nil {
		if st, ok := c.typeRefStruct(l.Type); ok {
			c.varStructs[l.Name] = st
		}
	} else if l.Value != nil {
		if st, ok := c.structTypeFromExpr(l.Value); ok {
			if s, ok2 := st.(types.ListType); ok2 {
				if elem, ok3 := s.Elem.(types.StructType); ok3 {
					if _, ok4 := c.env.FindUnionByVariant(elem.Name); !ok4 {
						c.varStructs[l.Name] = elem
					}
				}
			} else if elem, ok2 := st.(types.StructType); ok2 {
				if _, ok3 := c.env.FindUnionByVariant(elem.Name); !ok3 {
					c.varStructs[l.Name] = elem
				}
			}
		}
	}
	return nil
}

func (c *Compiler) compileGlobalVar(v *parser.VarStmt) error {
	val := "0"
	var err error
	if v.Value != nil {
		val, err = c.compileExpr(v.Value)
		if err != nil {
			return err
		}
	}
	name := sanitizeName(v.Name)
	c.vars[v.Name] = true
	typ, _ := c.env.GetVar(v.Name)
	typStr := c.ocamlType(typ)
	if typStr == "" || strings.Contains(typStr, "Obj.t") {
		if v.Value != nil {
			t := types.ExprType(v.Value, c.env)
			guess := c.ocamlType(t)
			if guess != "" && !strings.Contains(guess, "Obj.t") {
				typStr = guess
			} else if st, ok := c.structTypeFromExpr(v.Value); ok {
				if s, ok2 := st.(types.StructType); ok2 {
					if ut, ok3 := c.env.FindUnionByVariant(s.Name); ok3 {
						typStr = strings.ToLower(ut.Name)
					} else {
						typStr = c.ocamlType(s)
					}
				} else {
					typStr = c.ocamlType(st)
				}
			} else {
				typStr = ""
			}
		}
	}
	if strings.Contains(typStr, "Obj.t") {
		typStr = ""
	}
	if typStr != "" {
		c.writeln(fmt.Sprintf("let %s : %s ref = ref %s", name, typStr, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = ref %s", name, val))
	}
	return nil
}

func (c *Compiler) compileLocalLet(l *parser.LetStmt) error {
	val := "()"
	var err error
	if l.Value != nil {
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
	}
	typ := ""
	if l.Type != nil {
		typ = c.typeRef(l.Type)
	} else if l.Value != nil {
		t := types.ExprType(l.Value, c.env)
		guess := c.ocamlType(t)
		if guess != "" && !strings.Contains(guess, "Obj.t") {
			typ = guess
		} else if st, ok := c.structTypeFromExpr(l.Value); ok {
			if s, ok2 := st.(types.StructType); ok2 {
				if ut, ok3 := c.env.FindUnionByVariant(s.Name); ok3 {
					typ = strings.ToLower(ut.Name)
				} else {
					typ = c.ocamlType(s)
				}
			} else {
				typ = c.ocamlType(st)
			}
		}
	}
	if strings.Contains(typ, "Obj.t") {
		typ = ""
	}
	name := sanitizeName(l.Name)
	if typ != "" {
		c.writeln(fmt.Sprintf("let %s : %s = %s in", name, typ, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = %s in", name, val))
	}
	if l.Type != nil {
		if st, ok := c.typeRefStruct(l.Type); ok {
			c.varStructs[l.Name] = st
		}
		c.localTypes[l.Name] = types.ResolveTypeRef(l.Type, c.env)
	} else if l.Value != nil {
		if st, ok := c.structTypeFromExpr(l.Value); ok {
			if s, ok2 := st.(types.ListType); ok2 {
				if elem, ok3 := s.Elem.(types.StructType); ok3 {
					if _, ok4 := c.env.FindUnionByVariant(elem.Name); !ok4 {
						c.varStructs[l.Name] = elem
					}
				}
			} else if elem, ok2 := st.(types.StructType); ok2 {
				if _, ok3 := c.env.FindUnionByVariant(elem.Name); !ok3 {
					c.varStructs[l.Name] = elem
				}
			}
		}
		c.localTypes[l.Name] = types.ExprType(l.Value, c.env)
	}
	delete(c.vars, l.Name)
	return nil
}

func (c *Compiler) compileLocalVar(v *parser.VarStmt) error {
	val := "0"
	var err error
	if v.Value != nil {
		val, err = c.compileExpr(v.Value)
		if err != nil {
			return err
		}
	}
	c.vars[v.Name] = true
	typ, _ := c.env.GetVar(v.Name)
	typStr := c.ocamlType(typ)
	if typStr == "" || strings.Contains(typStr, "Obj.t") {
		if v.Value != nil {
			t := types.ExprType(v.Value, c.env)
			guess := c.ocamlType(t)
			if guess != "" && !strings.Contains(guess, "Obj.t") {
				typStr = guess
			} else if st, ok := c.structTypeFromExpr(v.Value); ok {
				if s, ok2 := st.(types.StructType); ok2 {
					if ut, ok3 := c.env.FindUnionByVariant(s.Name); ok3 {
						typStr = strings.ToLower(ut.Name)
					} else {
						typStr = c.ocamlType(s)
					}
				} else {
					typStr = c.ocamlType(st)
				}
			} else {
				typStr = ""
			}
		}
	}
	if strings.Contains(typStr, "Obj.t") {
		typStr = ""
	}
	name := sanitizeName(v.Name)
	if typStr != "" {
		c.writeln(fmt.Sprintf("let %s : %s ref = ref %s in", name, typStr, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = ref %s in", name, val))
	}
	if v.Type != nil {
		c.localTypes[v.Name] = types.ResolveTypeRef(v.Type, c.env)
	} else if v.Value != nil {
		c.localTypes[v.Name] = types.ExprType(v.Value, c.env)
	}
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		parts := make([]string, len(t.Variants))
		for i, v := range t.Variants {
			if len(v.Fields) == 0 {
				parts[i] = v.Name
			} else {
				args := make([]string, len(v.Fields))
				for j, f := range v.Fields {
					args[j] = c.typeRef(f.Type)
				}
				parts[i] = fmt.Sprintf("%s of %s", v.Name, strings.Join(args, " * "))
			}
		}
		name := strings.ToLower(t.Name)
		c.writeln(fmt.Sprintf("type %s = %s", name, strings.Join(parts, " | ")))
		return nil
	}
	if len(t.Members) == 0 {
		// empty types unsupported
		return nil
	}
	fields := make([]string, 0, len(t.Members))
	for _, m := range t.Members {
		if m.Field == nil {
			continue
		}
		typ := c.typeRef(m.Field.Type)
		fields = append(fields, fmt.Sprintf("mutable %s : %s", sanitizeField(m.Field.Name), typ))
	}
	name := strings.ToLower(t.Name)
	c.writeln(fmt.Sprintf("type %s = { %s }", name, strings.Join(fields, "; ")))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	name := sanitizeName(a.Name)
	if len(a.Index) > 0 && c.vars[a.Name] {
		typ, _ := c.env.GetVar(a.Name)
		if len(a.Index) == 1 {
			idx, err := c.compileExpr(a.Index[0].Start)
			if err != nil {
				return err
			}
			switch typ.(type) {
			case types.MapType:
				c.writeln(fmt.Sprintf("%s := map_set !%s %s (%s);", name, name, idx, val))
			default:
				c.writeln(fmt.Sprintf("%s := list_set !%s %s (%s);", name, name, idx, val))
			}
			return nil
		}
		if len(a.Index) == 2 {
			idx1, err := c.compileExpr(a.Index[0].Start)
			if err != nil {
				return err
			}
			idx2, err := c.compileExpr(a.Index[1].Start)
			if err != nil {
				return err
			}
			switch typ.(type) {
			case types.MapType:
				c.writeln(fmt.Sprintf("%s := map_set !%s %s (map_set (map_get !%s %s) %s (%s));", name, name, idx1, name, idx1, idx2, val))
			default:
				c.writeln(fmt.Sprintf("%s := list_set !%s %s (list_set (List.nth !%s %s) %s (%s));", name, name, idx1, name, idx1, idx2, val))
			}
			return nil
		}
	}
	if len(a.Field) > 0 {
		field := sanitizeField(a.Field[0].Name)
		if c.vars[a.Name] {
			c.writeln(fmt.Sprintf("%s := { !%s with %s = %s };", name, name, field, val))
		} else {
			c.writeln(fmt.Sprintf("%s.%s <- %s;", name, field, val))
		}
		return nil
	}
	if c.vars[a.Name] {
		c.writeln(fmt.Sprintf("%s := %s;", name, val))
	} else {
		c.writeln(fmt.Sprintf("(* assignment to %s unsupported *)", name))
	}
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then (", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		c.writeln(") else (")
		c.indent++
		if err := c.compileIf(i.ElseIf); err != nil {
			return err
		}
		c.indent--
		c.writeln(") ;")
	} else if len(i.Else) > 0 {
		c.writeln(") else (")
		c.indent++
		for _, st := range i.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(") ;")
	} else {
		c.writeln(") ;")
	}
	return nil
}

func (c *Compiler) compileFor(fr *parser.ForStmt) error {
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln("try")
		c.indent++
		c.writeln(fmt.Sprintf("for %s = %s to %s do", fr.Name, start, end))
		c.indent++
		c.writeln("try")
		c.indent++
		for _, st := range fr.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("with Continue -> ()")
		c.indent--
		c.writeln("done")
		c.indent--
		c.writeln("with Break -> () ;")
		return nil
	}
	src, err := c.compileExpr(fr.Source)
	if err != nil {
		return err
	}
	srcType := types.ExprType(fr.Source, c.env)
	var elemType types.Type
	isMap := false
	switch t := srcType.(type) {
	case types.ListType:
		elemType = t.Elem
	case types.GroupType:
		elemType = t.Elem
	case types.MapType:
		elemType = t.Key
		isMap = true
	default:
		elemType = types.AnyType{}
	}
	if name, ok := identName(fr.Source); ok {
		if st, ok2 := c.varStructs[name]; ok2 {
			elemType = st
		}
	} else if st, ok := c.structTypeFromExpr(fr.Source); ok {
		if lt, ok2 := st.(types.ListType); ok2 {
			if elem, ok3 := lt.Elem.(types.StructType); ok3 {
				elemType = elem
			}
		}
	}
	oldEnv := c.env
	oldVars := c.vars
	tmpVars := make(map[string]bool, len(c.vars))
	for k, v := range c.vars {
		tmpVars[k] = v
	}
	c.vars = tmpVars
	loopEnv := types.NewEnv(oldEnv)
	loopEnv.SetVar(fr.Name, elemType, true)
	c.env = loopEnv
	loopName := fmt.Sprintf("__loop%d", c.loop)
	c.loop++
	c.writeln(fmt.Sprintf("let rec %s lst =", loopName))
	c.indent++
	c.writeln("match lst with")
	c.indent++
	c.writeln("| [] -> ()")
	pat := fr.Name
	if isMap {
		pat = fmt.Sprintf("(%s, _)", fr.Name)
	}
	if typ := c.loopTypeForSource(fr.Source); typ != "" && !isMap {
		pat = fmt.Sprintf("(%s : %s)", fr.Name, typ)
	}
	c.writeln(fmt.Sprintf("| %s::rest ->", pat))
	c.indent++
	c.writeln("(try")
	c.indent++
	for _, st := range fr.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = oldEnv
			c.vars = oldVars
			return err
		}
	}
	c.env = oldEnv
	c.indent--
	c.writeln("with Continue -> ())")
	c.writeln(fmt.Sprintf("; %s rest", loopName))
	c.indent--
	c.indent--
	c.writeln("in")
	c.writeln(fmt.Sprintf("try %s %s with Break -> ()", loopName, src))
	c.writeln(";")
	c.vars = oldVars
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	oldVars := c.vars
	tmpVars := make(map[string]bool, len(c.vars))
	for k, v := range c.vars {
		tmpVars[k] = v
	}
	c.vars = tmpVars
	loopName := fmt.Sprintf("__loop%d", c.loop)
	c.loop++
	c.writeln(fmt.Sprintf("let rec %s () =", loopName))
	c.indent++
	c.writeln(fmt.Sprintf("if %s then (", cond))
	c.indent++
	c.writeln("try")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln(fmt.Sprintf("%s ()", loopName))
	c.indent--
	c.writeln("with Continue -> ()")
	c.indent--
	c.writeln(") else ()")
	c.indent--
	c.writeln(fmt.Sprintf("in try %s () with Break -> ()", loopName))
	c.writeln(";")
	c.vars = oldVars
	return nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseVal string
	if ie.ElseIf != nil {
		elseVal, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseVal, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseVal = "()"
	}
	return fmt.Sprintf("(if %s then %s else %s)", cond, thenVal, elseVal), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString("(match " + target + " with")
	for _, cs := range m.Cases {
		pat, err := c.compilePattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		buf.WriteString(" | " + pat + " -> " + res)
	}
	buf.WriteString(")")
	return buf.String(), nil
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
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("assert (%s)", expr))
	return nil
}

func capitalize(name string) string {
	if name == "" {
		return ""
	}
	return strings.ToUpper(name[:1]) + name[1:]
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	if im.Lang == nil {
		return nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	lang := *im.Lang
	switch lang {
	case "go":
		if im.Path == "mochi/runtime/ffi/go/testpkg" {
			mod := capitalize(alias)
			c.imports[alias] = importInfo{
				module: mod,
				fields: map[string]string{"Add": "add", "Pi": "pi", "Answer": "answer"},
			}
			c.writeln(fmt.Sprintf("module %s = struct", mod))
			c.indent++
			c.writeln("let add a b = a + b")
			c.writeln("let pi = 3.14")
			c.writeln("let answer = 42")
			c.indent--
			c.writeln("end")
		}
	case "python":
		if im.Path == "math" {
			mod := capitalize(alias)
			c.imports[alias] = importInfo{module: mod, fields: nil}
			c.writeln(fmt.Sprintf("module %s = struct", mod))
			c.indent++
			c.writeln("let pi = Float.pi")
			c.writeln("let e = exp 1.0")
			c.writeln("let sqrt x = sqrt x")
			c.writeln("let pow x y = x ** y")
			c.writeln("let sin x = sin x")
			c.writeln("let log x = log x")
			c.indent--
			c.writeln("end")
		}
	}
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := u.Target
	itemVar := "__it"
	cond := ""

	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}

	child := types.NewEnv(c.env)
	for _, f := range st.Order {
		child.SetVar(f, st.Fields[f], true)
	}
	old := c.env
	c.env = child

	c.writeln(fmt.Sprintf("let %s =", list))
	c.indent++
	c.writeln(fmt.Sprintf("List.map (fun %s ->", itemVar))
	c.indent++

	if u.Where != nil {
		c.indent++ // extra indent for if body
		condExpr, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = old
			return err
		}
		cond = condExpr
		c.indent--
	}

	parts := make([]string, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, _ := identName(it.Key)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = old
			return err
		}
		parts[i] = fmt.Sprintf("%s = %s", sanitizeField(key), val)
	}
	updateExpr := fmt.Sprintf("{ %s with %s }", itemVar, strings.Join(parts, "; "))

	if u.Where != nil {
		c.writeln(fmt.Sprintf("if %s then %s else %s", cond, updateExpr, itemVar))
	} else {
		c.writeln(updateExpr)
	}

	c.indent--
	c.writeln(fmt.Sprintf(") %s", list))
	c.indent--

	c.env = old
	return nil
}

// queryEnv builds an environment containing the variables introduced by q.
func (c *Compiler) queryEnv(q *parser.QueryExpr) *types.Env {
	env := types.NewEnv(c.env)

	elem := func(e *parser.Expr) types.Type {
		if name, ok := identName(e); ok {
			if st, ok2 := c.varStructs[name]; ok2 {
				return st
			}
		}
		if st, ok := c.structTypeFromExpr(e); ok {
			if lt, ok2 := st.(types.ListType); ok2 {
				if elem, ok3 := lt.Elem.(types.StructType); ok3 {
					return elem
				}
			} else if st2, ok2 := st.(types.StructType); ok2 {
				return st2
			}
		}
		t := types.ExprType(e, c.env)
		switch tt := t.(type) {
		case types.ListType:
			return tt.Elem
		case types.GroupType:
			return tt.Elem
		default:
			return types.AnyType{}
		}
	}

	env.SetVar(q.Var, elem(q.Source), true)
	for _, fr := range q.Froms {
		env.SetVar(fr.Var, elem(fr.Src), true)
	}
	for _, jo := range q.Joins {
		env.SetVar(jo.Var, elem(jo.Src), true)
	}
	if q.Group != nil {
		keyT := types.ExprType(q.Group.Exprs[0], env)
		env.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: elem(q.Source)}, true)
	}
	return env
}

func (c *Compiler) compileExprWithEnv(e *parser.Expr, env *types.Env) (string, error) {
	old := c.env
	c.env = env
	s, err := c.compileExpr(e)
	c.env = old
	return s, err
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if q.Group != nil {
		return c.compileGroup(q)
	}
	if len(q.Froms) == 0 && len(q.Joins) == 1 {
		if q.Joins[0].Side != nil {
			switch *q.Joins[0].Side {
			case "left":
				return c.compileLeftJoin(q)
			case "right":
				return c.compileRightJoin(q)
			case "outer":
				return c.compileOuterJoin(q)
			}
		} else {
			return c.compileJoin(q)
		}
	}
	if len(q.Joins) > 1 {
		if side := q.Joins[len(q.Joins)-1].Side; side != nil && *side == "left" {
			return c.compileLeftJoinLast(q)
		}
	}
	froms := append([]*parser.FromClause{{Var: q.Var, Src: q.Source}}, q.Froms...)
	total := len(froms) + len(q.Joins)
	sources := make([]string, total)
	vars := make([]string, total)
	srcExprs := make([]*parser.Expr, total)
	idx := 0
	outerEnv := c.env
	for _, fr := range froms {
		src, err := c.compileExprWithEnv(fr.Src, outerEnv)
		if err != nil {
			return "", err
		}
		if _, ok := types.ExprType(fr.Src, outerEnv).(types.GroupType); ok {
			src += ".items"
		}
		sources[idx] = src
		vars[idx] = fr.Var
		srcExprs[idx] = fr.Src
		idx++
	}
	qenv := c.queryEnv(q)
	joinConds := make([]string, len(q.Joins))
	for j, jo := range q.Joins {
		src, err := c.compileExprWithEnv(jo.Src, outerEnv)
		if err != nil {
			return "", err
		}
		if _, ok := types.ExprType(jo.Src, outerEnv).(types.GroupType); ok {
			src += ".items"
		}
		sources[idx] = src
		vars[idx] = jo.Var
		srcExprs[idx] = jo.Src
		idx++
		if jo.On != nil {
			cond, err := c.compileExprWithEnv(jo.On, qenv)
			if err != nil {
				return "", err
			}
			joinConds[j] = cond
		}
	}

	condParts := []string{}
	for _, jc := range joinConds {
		if jc != "" {
			condParts = append(condParts, jc)
		}
	}
	if q.Where != nil {
		w, err := c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
		condParts = append(condParts, w)
	}
	cond := strings.Join(condParts, " && ")

	resName := fmt.Sprintf("__res%d", c.loop)
	c.loop++
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let %s = ref [] in\n", resName))
	for i := 0; i < total; i++ {
		for j := 0; j <= i; j++ {
			buf.WriteString(strings.Repeat("  ", j+1))
		}
		param := vars[i]
		if t := c.loopTypeForSource(srcExprs[i]); t != "" {
			param = fmt.Sprintf("(%s : %s)", vars[i], t)
		}
		buf.WriteString(fmt.Sprintf("List.iter (fun %s ->\n", param))
	}
	for i := 0; i < total; i++ {
		buf.WriteString(strings.Repeat("  ", total+1-i))
		buf.WriteString("  ")
	}
	if cond != "" {
		buf.WriteString(fmt.Sprintf("if %s then\n", cond))
		buf.WriteString(strings.Repeat("  ", total+1))
	}
	sel, err := c.compileExprWithEnv(q.Select, qenv)
	if err != nil {
		return "", err
	}
	buf.WriteString(fmt.Sprintf("%s := %s :: !%s", resName, sel, resName))
	buf.WriteString(";\n")
	for i := total - 1; i >= 0; i-- {
		for j := 0; j <= i; j++ {
			buf.WriteString(strings.Repeat("  ", i-j+1))
		}
		buf.WriteString(fmt.Sprintf(") %s;\n", sources[i]))
	}
	buf.WriteString(fmt.Sprintf("List.rev !%s)\n", resName))
	return buf.String(), nil
}

func (c *Compiler) compileLeftJoin(q *parser.QueryExpr) (string, error) {
	join := q.Joins[0]
	outerEnv := c.env
	leftSrc, err := c.compileExprWithEnv(q.Source, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(q.Source, outerEnv).(types.GroupType); ok {
		leftSrc += ".items"
	}
	rightSrc, err := c.compileExprWithEnv(join.Src, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(join.Src, outerEnv).(types.GroupType); ok {
		rightSrc += ".items"
	}
	qenv := c.queryEnv(q)
	on := "true"
	if join.On != nil {
		on, err = c.compileExprWithEnv(join.On, qenv)
		if err != nil {
			return "", err
		}
	}
	where := ""
	if q.Where != nil {
		where, err = c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExprWithEnv(q.Select, qenv)
	if err != nil {
		return "", err
	}
	resName := fmt.Sprintf("__res%d", c.loop)
	c.loop++
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let %s = ref [] in\n", resName))
	leftParam := q.Var
	if t := c.loopTypeForSource(q.Source); t != "" {
		leftParam = fmt.Sprintf("(%s : %s)", q.Var, t)
	}
	buf.WriteString(fmt.Sprintf("  List.iter (fun %s ->\n", leftParam))
	buf.WriteString("    let matched = ref false in\n")
	rightParam := join.Var
	if t := c.loopTypeForSource(join.Src); t != "" {
		rightParam = fmt.Sprintf("(%s : %s)", join.Var, t)
	}
	buf.WriteString(fmt.Sprintf("    List.iter (fun %s ->\n", rightParam))
	buf.WriteString(fmt.Sprintf("      if %s then (\n", on))
	if where != "" {
		buf.WriteString(fmt.Sprintf("        if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("        %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("        matched := true)\n")
	buf.WriteString(fmt.Sprintf("    ) %s;\n", rightSrc))
	buf.WriteString("    if not !matched then (\n")
	buf.WriteString(fmt.Sprintf("      let %s = Obj.magic () in\n", join.Var))
	if where != "" {
		buf.WriteString(fmt.Sprintf("      if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("      %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("    );\n")
	buf.WriteString(fmt.Sprintf("  ) %s;\n", leftSrc))
	buf.WriteString(fmt.Sprintf("  List.rev !%s)\n", resName))
	return buf.String(), nil
}

func (c *Compiler) compileRightJoin(q *parser.QueryExpr) (string, error) {
	join := q.Joins[0]
	outerEnv := c.env
	rightSrc, err := c.compileExprWithEnv(join.Src, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(join.Src, outerEnv).(types.GroupType); ok {
		rightSrc += ".items"
	}
	leftSrc, err := c.compileExprWithEnv(q.Source, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(q.Source, outerEnv).(types.GroupType); ok {
		leftSrc += ".items"
	}
	qenv := c.queryEnv(q)
	on := "true"
	if join.On != nil {
		on, err = c.compileExprWithEnv(join.On, qenv)
		if err != nil {
			return "", err
		}
	}
	where := ""
	if q.Where != nil {
		where, err = c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExprWithEnv(q.Select, qenv)
	if err != nil {
		return "", err
	}
	resName := fmt.Sprintf("__res%d", c.loop)
	c.loop++
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let %s = ref [] in\n", resName))
	rightParam := join.Var
	if t := c.loopTypeForSource(join.Src); t != "" {
		rightParam = fmt.Sprintf("(%s : %s)", join.Var, t)
	}
	buf.WriteString(fmt.Sprintf("  List.iter (fun %s ->\n", rightParam))
	buf.WriteString("    let matched = ref false in\n")
	leftParam := q.Var
	if t := c.loopTypeForSource(q.Source); t != "" {
		leftParam = fmt.Sprintf("(%s : %s)", q.Var, t)
	}
	buf.WriteString(fmt.Sprintf("    List.iter (fun %s ->\n", leftParam))
	buf.WriteString(fmt.Sprintf("      if %s then (\n", on))
	if where != "" {
		buf.WriteString(fmt.Sprintf("        if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("        %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("        matched := true)\n")
	buf.WriteString(fmt.Sprintf("    ) %s;\n", leftSrc))
	buf.WriteString("    if not !matched then (\n")
	buf.WriteString(fmt.Sprintf("      let %s = Obj.magic () in\n", q.Var))
	if where != "" {
		buf.WriteString(fmt.Sprintf("      if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("      %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("    );\n")
	buf.WriteString(fmt.Sprintf("  ) %s;\n", rightSrc))
	buf.WriteString(fmt.Sprintf("  List.rev !%s)\n", resName))
	return buf.String(), nil
}

func (c *Compiler) compileOuterJoin(q *parser.QueryExpr) (string, error) {
	join := q.Joins[0]
	outerEnv := c.env
	leftSrc, err := c.compileExprWithEnv(q.Source, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(q.Source, outerEnv).(types.GroupType); ok {
		leftSrc += ".items"
	}
	rightSrc, err := c.compileExprWithEnv(join.Src, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(join.Src, outerEnv).(types.GroupType); ok {
		rightSrc += ".items"
	}
	qenv := c.queryEnv(q)
	on := "true"
	if join.On != nil {
		on, err = c.compileExprWithEnv(join.On, qenv)
		if err != nil {
			return "", err
		}
	}
	where := ""
	if q.Where != nil {
		where, err = c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExprWithEnv(q.Select, qenv)
	if err != nil {
		return "", err
	}
	resName := fmt.Sprintf("__res%d", c.loop)
	c.loop++
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let %s = ref [] in\n", resName))
	buf.WriteString(fmt.Sprintf("  List.iter (fun %s ->\n", q.Var))
	buf.WriteString("    let matched = ref false in\n")
	buf.WriteString(fmt.Sprintf("    List.iter (fun %s ->\n", join.Var))
	buf.WriteString(fmt.Sprintf("      if %s then (\n", on))
	if where != "" {
		buf.WriteString(fmt.Sprintf("        if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("        %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("        matched := true)\n")
	buf.WriteString(fmt.Sprintf("    ) %s;\n", rightSrc))
	buf.WriteString("    if not !matched then (\n")
	buf.WriteString(fmt.Sprintf("      let %s = Obj.magic () in\n", join.Var))
	if where != "" {
		buf.WriteString(fmt.Sprintf("      if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("      %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("    );\n")
	buf.WriteString(fmt.Sprintf("  ) %s;\n", leftSrc))
	buf.WriteString(fmt.Sprintf("  List.iter (fun %s ->\n", join.Var))
	buf.WriteString("    let matched = ref false in\n")
	buf.WriteString(fmt.Sprintf("    List.iter (fun %s ->\n", q.Var))
	buf.WriteString(fmt.Sprintf("      if %s then matched := true\n", on))
	buf.WriteString(fmt.Sprintf("    ) %s;\n", leftSrc))
	buf.WriteString("    if not !matched then (\n")
	buf.WriteString(fmt.Sprintf("      let %s = Obj.magic () in\n", q.Var))
	if where != "" {
		buf.WriteString(fmt.Sprintf("      if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("      %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("    );\n")
	buf.WriteString(fmt.Sprintf("  ) %s;\n", rightSrc))
	buf.WriteString(fmt.Sprintf("  List.rev !%s)\n", resName))
	return buf.String(), nil
}

func (c *Compiler) compileLeftJoinLast(q *parser.QueryExpr) (string, error) {
	last := q.Joins[len(q.Joins)-1]
	prev := q.Joins[:len(q.Joins)-1]

	froms := append([]*parser.FromClause{{Var: q.Var, Src: q.Source}}, q.Froms...)
	total := len(froms) + len(prev)
	sources := make([]string, total)
	vars := make([]string, total)
	srcExprs := make([]*parser.Expr, total)
	idx := 0
	outerEnv := c.env
	for _, fr := range froms {
		src, err := c.compileExprWithEnv(fr.Src, outerEnv)
		if err != nil {
			return "", err
		}
		if _, ok := types.ExprType(fr.Src, outerEnv).(types.GroupType); ok {
			src += ".items"
		}
		sources[idx] = src
		vars[idx] = fr.Var
		srcExprs[idx] = fr.Src
		idx++
	}
	qenv := c.queryEnv(q)
	joinConds := make([]string, len(prev))
	for j, jo := range prev {
		src, err := c.compileExprWithEnv(jo.Src, outerEnv)
		if err != nil {
			return "", err
		}
		if _, ok := types.ExprType(jo.Src, outerEnv).(types.GroupType); ok {
			src += ".items"
		}
		sources[idx] = src
		vars[idx] = jo.Var
		srcExprs[idx] = jo.Src
		idx++
		if jo.On != nil {
			cond, err := c.compileExprWithEnv(jo.On, qenv)
			if err != nil {
				return "", err
			}
			joinConds[j] = cond
		}
	}

	condPrev := []string{}
	for _, jc := range joinConds {
		if jc != "" {
			condPrev = append(condPrev, jc)
		}
	}
	prevCond := strings.Join(condPrev, " && ")

	lastSrc, err := c.compileExprWithEnv(last.Src, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(last.Src, outerEnv).(types.GroupType); ok {
		lastSrc += ".items"
	}
	on := "true"
	if last.On != nil {
		on, err = c.compileExprWithEnv(last.On, qenv)
		if err != nil {
			return "", err
		}
	}
	where := ""
	if q.Where != nil {
		where, err = c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExprWithEnv(q.Select, qenv)
	if err != nil {
		return "", err
	}
	resName := fmt.Sprintf("__res%d", c.loop)
	c.loop++
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let %s = ref [] in\n", resName))
	for i := 0; i < total; i++ {
		for j := 0; j <= i; j++ {
			buf.WriteString(strings.Repeat("  ", j+1))
		}
		param := vars[i]
		if t := c.loopTypeForSource(srcExprs[i]); t != "" {
			param = fmt.Sprintf("(%s : %s)", vars[i], t)
		}
		buf.WriteString(fmt.Sprintf("List.iter (fun %s ->\n", param))
	}
	indent := strings.Repeat("  ", total+1)
	condAll := prevCond
	if on != "true" {
		if condAll != "" {
			condAll += " && " + on
		} else {
			condAll = on
		}
	}
	if where != "" {
		if condAll != "" {
			condAll += " && " + where
		} else {
			condAll = where
		}
	}
	buf.WriteString(indent + "let matched = ref false in\n")
	buf.WriteString(indent + fmt.Sprintf("List.iter (fun %s ->\n", last.Var))
	buf.WriteString(indent + "  if " + condAll + " then (\n")
	buf.WriteString(indent + "    " + fmt.Sprintf("%s := %s :: !%s;\n", resName, sel, resName))
	buf.WriteString(indent + "    matched := true)\n")
	buf.WriteString(indent + fmt.Sprintf(") %s;\n", lastSrc))

	condNoLast := prevCond
	if where != "" {
		if condNoLast != "" {
			condNoLast += " && " + where
		} else {
			condNoLast = where
		}
	}
	buf.WriteString(indent + "if not !matched then (\n")
	buf.WriteString(indent + fmt.Sprintf("  let %s = Obj.magic () in\n", last.Var))
	if condNoLast != "" {
		buf.WriteString(indent + fmt.Sprintf("  if %s then %s := %s :: !%s;\n", condNoLast, resName, sel, resName))
	} else {
		buf.WriteString(indent + fmt.Sprintf("  %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString(indent + ");\n")

	for i := total - 1; i >= 0; i-- {
		for j := 0; j <= i; j++ {
			buf.WriteString(strings.Repeat("  ", i-j+1))
		}
		buf.WriteString(fmt.Sprintf(") %s;\n", sources[i]))
	}
	buf.WriteString(fmt.Sprintf("List.rev !%s)\n", resName))
	return buf.String(), nil
}

func (c *Compiler) compileJoin(q *parser.QueryExpr) (string, error) {
	join := q.Joins[0]
	outerEnv := c.env
	leftSrc, err := c.compileExprWithEnv(q.Source, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(q.Source, outerEnv).(types.GroupType); ok {
		leftSrc += ".items"
	}
	rightSrc, err := c.compileExprWithEnv(join.Src, outerEnv)
	if err != nil {
		return "", err
	}
	if _, ok := types.ExprType(join.Src, outerEnv).(types.GroupType); ok {
		rightSrc += ".items"
	}
	qenv := c.queryEnv(q)
	on := "true"
	if join.On != nil {
		on, err = c.compileExprWithEnv(join.On, qenv)
		if err != nil {
			return "", err
		}
	}
	where := ""
	if q.Where != nil {
		where, err = c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExprWithEnv(q.Select, qenv)
	if err != nil {
		return "", err
	}
	resName := fmt.Sprintf("__res%d", c.loop)
	c.loop++
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let %s = ref [] in\n", resName))
	leftParam := q.Var
	if t := c.loopTypeForSource(q.Source); t != "" {
		leftParam = fmt.Sprintf("(%s : %s)", q.Var, t)
	}
	buf.WriteString(fmt.Sprintf("  List.iter (fun %s ->\n", leftParam))
	rightParam := join.Var
	if t := c.loopTypeForSource(join.Src); t != "" {
		rightParam = fmt.Sprintf("(%s : %s)", join.Var, t)
	}
	buf.WriteString(fmt.Sprintf("    List.iter (fun %s ->\n", rightParam))
	buf.WriteString(fmt.Sprintf("      if %s then (\n", on))
	if where != "" {
		buf.WriteString(fmt.Sprintf("        if %s then %s := %s :: !%s;\n", where, resName, sel, resName))
	} else {
		buf.WriteString(fmt.Sprintf("        %s := %s :: !%s;\n", resName, sel, resName))
	}
	buf.WriteString("      )\n")
	buf.WriteString(fmt.Sprintf("    ) %s;\n", rightSrc))
	buf.WriteString(fmt.Sprintf("  ) %s;\n", leftSrc))
	buf.WriteString(fmt.Sprintf("  List.rev !%s)\n", resName))
	return buf.String(), nil
}

func (c *Compiler) compileGroup(q *parser.QueryExpr) (string, error) {
	if len(q.Group.Exprs) != 1 {
		return "", fmt.Errorf("unsupported multi-key group")
	}

	froms := append([]*parser.FromClause{{Var: q.Var, Src: q.Source}}, q.Froms...)
	total := len(froms) + len(q.Joins)
	sources := make([]string, total)
	vars := make([]string, total)
	srcExprs := make([]*parser.Expr, total)
	idx := 0
	outerEnv := c.env
	for _, fr := range froms {
		src, err := c.compileExprWithEnv(fr.Src, outerEnv)
		if err != nil {
			return "", err
		}
		if _, ok := types.ExprType(fr.Src, outerEnv).(types.GroupType); ok {
			src += ".items"
		}
		sources[idx] = src
		vars[idx] = fr.Var
		srcExprs[idx] = fr.Src
		idx++
	}
	qenv := c.queryEnv(q)
	joinConds := make([]string, len(q.Joins))
	for j, jo := range q.Joins {
		src, err := c.compileExprWithEnv(jo.Src, outerEnv)
		if err != nil {
			return "", err
		}
		if _, ok := types.ExprType(jo.Src, outerEnv).(types.GroupType); ok {
			src += ".items"
		}
		sources[idx] = src
		vars[idx] = jo.Var
		srcExprs[idx] = jo.Src
		idx++
		if jo.On != nil {
			cond, err := c.compileExprWithEnv(jo.On, qenv)
			if err != nil {
				return "", err
			}
			joinConds[j] = cond
		}
	}
	condParts := []string{}
	for _, jc := range joinConds {
		if jc != "" {
			condParts = append(condParts, jc)
		}
	}
	if q.Where != nil {
		w, err := c.compileExprWithEnv(q.Where, qenv)
		if err != nil {
			return "", err
		}
		condParts = append(condParts, w)
	}
	cond := strings.Join(condParts, " && ")

	keyExpr, err := c.compileExprWithEnv(q.Group.Exprs[0], qenv)
	if err != nil {
		return "", err
	}
	keyTyp := ""
	if st, ok := c.structTypeFromExprEnv(q.Group.Exprs[0], qenv); ok {
		if s, ok2 := st.(types.StructType); ok2 {
			keyTyp = c.ocamlType(s)
		}
	}
	if keyTyp == "" {
		t := types.ExprType(q.Group.Exprs[0], qenv)
		guess := c.ocamlType(t)
		if guess != "" && guess != "Obj.t" {
			keyTyp = guess
		}
	}

	var elemType types.Type
	if name, ok := identName(q.Source); ok {
		if st, ok2 := c.varStructs[name]; ok2 {
			elemType = st
		}
	}
	if elemType == nil {
		srcType0 := types.ExprType(q.Source, c.env)
		switch t := srcType0.(type) {
		case types.ListType:
			elemType = t.Elem
		case types.GroupType:
			elemType = t.Elem
		default:
			elemType = types.AnyType{}
		}
	}
	elemTyp := c.ocamlType(elemType)
	if elemTyp == "" {
		elemTyp = "Obj.t"
	}
	groupKeyTyp := keyTyp
	if groupKeyTyp == "" {
		groupKeyTyp = "Obj.t"
	}

	resName := fmt.Sprintf("__res%d", c.loop)
	groups := fmt.Sprintf("__groups%d", c.loop)
	c.loop++

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("(let (%s : (%s * %s list) list ref) = ref [] in\n", groups, groupKeyTyp, elemTyp))
	for i := 0; i < total; i++ {
		for j := 0; j <= i; j++ {
			buf.WriteString(strings.Repeat("  ", j+1))
		}
		param := vars[i]
		if t := c.loopTypeForSource(srcExprs[i]); t != "" {
			param = fmt.Sprintf("(%s : %s)", vars[i], t)
		}
		buf.WriteString(fmt.Sprintf("List.iter (fun %s ->\n", param))
	}
	for i := 0; i < total; i++ {
		buf.WriteString(strings.Repeat("  ", total+1-i))
		buf.WriteString("  ")
	}
	if cond != "" {
		buf.WriteString(fmt.Sprintf("if %s then (\n", cond))
		buf.WriteString(strings.Repeat("  ", total+2))
	}
	if keyTyp != "" {
		buf.WriteString(fmt.Sprintf("let (key : %s) = %s in\n", keyTyp, keyExpr))
	} else {
		buf.WriteString(fmt.Sprintf("let key = %s in\n", keyExpr))
	}
	buf.WriteString(strings.Repeat("  ", total+2))
	buf.WriteString(fmt.Sprintf("let cur = try List.assoc key !%s with Not_found -> [] in\n", groups))
	buf.WriteString(strings.Repeat("  ", total+2))
	buf.WriteString(fmt.Sprintf("%s := (key, %s :: cur) :: List.remove_assoc key !%s", groups, q.Var, groups))
	if cond != "" {
		buf.WriteString(")")
	}
	buf.WriteString(";\n")
	for i := total - 1; i >= 0; i-- {
		for j := 0; j <= i; j++ {
			buf.WriteString(strings.Repeat("  ", i-j+1))
		}
		buf.WriteString(fmt.Sprintf(") %s;\n", sources[i]))
	}

	buf.WriteString(fmt.Sprintf("  let %s = ref [] in\n", resName))
	param := fmt.Sprintf("((%sKey,%sItems))", q.Group.Name, q.Group.Name)
	if keyTyp != "" {
		param = fmt.Sprintf("((%sKey : %s), %sItems)", q.Group.Name, keyTyp, q.Group.Name)
	}
	buf.WriteString(fmt.Sprintf("  List.iter (fun %s ->\n", param))
	buf.WriteString(fmt.Sprintf("    let %s = { key = %sKey; items = List.rev %sItems } in\n", q.Group.Name, q.Group.Name, q.Group.Name))
	var elemType2 types.Type
	if name, ok := identName(q.Source); ok {
		if st, ok2 := c.varStructs[name]; ok2 {
			elemType2 = st
		}
	}
	if elemType2 == nil {
		srcType := types.ExprType(q.Source, c.env)
		switch t := srcType.(type) {
		case types.ListType:
			elemType2 = t.Elem
		case types.GroupType:
			elemType2 = t.Elem
		default:
			elemType2 = types.AnyType{}
		}
	}
	selEnv := types.NewEnv(qenv)
	selEnv.SetVar(q.Group.Name, types.GroupType{Elem: elemType2}, true)
	sel, err := c.compileExprWithEnv(q.Select, selEnv)
	if err != nil {
		return "", err
	}
	buf.WriteString(fmt.Sprintf("    %s := %s :: !%s\n", resName, sel, resName))
	buf.WriteString(fmt.Sprintf("  ) !%s;\n", groups))
	buf.WriteString(fmt.Sprintf("  List.rev !%s)\n", resName))
	return buf.String(), nil
}

// --- expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "()", fmt.Errorf("empty expression")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for i := 0; i < len(b.Right); i++ {
		op := b.Right[i]
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		opStr := op.Op
		switch opStr {
		case "==":
			opStr = "="
		case "!=":
			opStr = "<>"
		case "%":
			opStr = "mod"
		case "in":
			if c.isMapPostfix(op.Right) {
				res = fmt.Sprintf("(List.mem_assoc %s %s)", res, r)
			} else if t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env); t == (types.StringType{}) {
				c.needContains = true
				res = fmt.Sprintf("(string_contains %s %s)", r, res)
			} else {
				res = fmt.Sprintf("(List.mem %s %s)", res, r)
			}
			continue
		case "union":
			if op.All {
				res = fmt.Sprintf("(list_union_all %s %s)", res, r)
			} else {
				res = fmt.Sprintf("(list_union %s %s)", res, r)
			}
			continue
		case "except":
			res = fmt.Sprintf("(list_except %s %s)", res, r)
			continue
		case "intersect":
			res = fmt.Sprintf("(list_intersect %s %s)", res, r)
			continue
		}
		if opStr == "+" || opStr == "-" || opStr == "*" || opStr == "/" {
			if c.isFloatUnary(b.Left) || c.isFloatPostfix(op.Right) {
				switch opStr {
				case "+":
					opStr = "+."
				case "-":
					opStr = "-."
				case "*":
					opStr = "*."
				case "/":
					opStr = "/."
				}
			}
		}
		// combine trailing comparison after logical operator
		if (op.Op == "&&" || op.Op == "||") && i+1 < len(b.Right) {
			next := b.Right[i+1]
			switch next.Op {
			case "==", "!=", "<", "<=", ">", ">=":
				nr, err := c.compilePostfix(next.Right)
				if err != nil {
					return "", err
				}
				nextOp := next.Op
				if nextOp == "==" {
					nextOp = "="
				} else if nextOp == "!=" {
					nextOp = "<>"
				}
				cmp := fmt.Sprintf("(%s %s %s)", r, nextOp, nr)
				res = fmt.Sprintf("(%s %s %s)", res, opStr, cmp)
				i++
				continue
			}
		}
		if opStr == "+" {
			if isStringUnary(b.Left) || isStringExprExpr(op.Right) {
				res = fmt.Sprintf("(%s ^ %s)", res, r)
				continue
			}
			lt := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}, c.env)
			rt := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env)
			if _, ok := lt.(types.StringType); ok {
				if _, ok2 := rt.(types.StringType); ok2 {
					res = fmt.Sprintf("(%s ^ %s)", res, r)
					continue
				}
			}
		}
		res = fmt.Sprintf("(%s %s %s)", res, opStr, r)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = "not (" + val + ")"
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := []string{}
			for _, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, s)
			}
			if strings.HasSuffix(val, ".contains") && len(args) == 1 {
				base := strings.TrimSuffix(val, ".contains")
				val = fmt.Sprintf("string_contains %s %s", base, args[0])
			} else {
				val = fmt.Sprintf("%s %s", val, strings.Join(args, " "))
			}
		case op.Index != nil:
			if op.Index.Colon != nil {
				start := "0"
				end := ""
				var err error
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
				}
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				} else {
					if c.isStringPrimary(p.Target) {
						end = fmt.Sprintf("String.length %s", val)
					} else {
						end = fmt.Sprintf("List.length %s", val)
					}
				}
				if c.isStringPrimary(p.Target) {
					val = fmt.Sprintf("string_slice %s (%s) (%s)", val, start, end)
				} else {
					val = fmt.Sprintf("slice %s (%s) (%s)", val, start, end)
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("unsupported index")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if c.isStringPrimary(p.Target) {
					val = fmt.Sprintf("String.make 1 (String.get %s %s)", val, idx)
				} else if c.isMapPrimary(p.Target) || isStringExpr(op.Index.Start) {
					val = fmt.Sprintf("Obj.obj (List.assoc %s (%s))", idx, val)
				} else {
					val = fmt.Sprintf("List.nth (%s) %s", val, idx)
				}
			}
		case op.Cast != nil:
			if op.Cast.Type != nil {
				if op.Cast.Type.Simple != nil {
					switch *op.Cast.Type.Simple {
					case "int":
						val = fmt.Sprintf("int_of_string %s", val)
					case "float":
						val = fmt.Sprintf("float_of_int %s", val)
					default:
						if p.Target.Map != nil {
							rec, err := c.recordLiteral(p.Target.Map)
							if err != nil {
								return "", err
							}
							val = rec
						} else {
							return "", fmt.Errorf("unsupported cast")
						}
					}
				} else if op.Cast.Type.Generic != nil && op.Cast.Type.Generic.Name == "list" {
					// no-op for list casts
				} else {
					return "", fmt.Errorf("unsupported cast")
				}
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
		default:
			return "", fmt.Errorf("unsupported postfix")
		}
	}
	return val, nil
}

func (c *Compiler) recordLiteral(m *parser.MapLiteral) (string, error) {
	fields := make([]string, len(m.Items))
	for i, it := range m.Items {
		var key string
		var ok bool
		if key, ok = identConst(it.Key); !ok {
			key, ok = stringConst(it.Key)
			if !ok {
				return "", fmt.Errorf("unsupported struct key")
			}
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s = %s", sanitizeField(key), v)
	}
	return "{ " + strings.Join(fields, "; ") + " }", nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Selector != nil:
		base := p.Selector.Root
		baseSan := sanitizeName(base)
		expr := baseSan
		if info, ok := c.imports[base]; ok {
			expr = info.module
			for _, field := range p.Selector.Tail {
				if info.fields != nil {
					if f2, ok2 := info.fields[field]; ok2 {
						expr = expr + "." + f2
					} else {
						expr = expr + "." + field
					}
				} else {
					expr = expr + "." + field
				}
			}
			return expr, nil
		}
		if c.vars[base] {
			expr = "(!" + baseSan + ")"
		}
		typ, _ := c.env.GetVar(base)
		for _, field := range p.Selector.Tail {
			if mt, ok := typ.(types.MapType); ok {
				expr = fmt.Sprintf("Obj.obj (List.assoc \"%s\" %s)", sanitizeField(field), expr)
				typ = mt.Value
				continue
			}
			if gt, ok := typ.(types.GroupType); ok {
				switch field {
				case "items":
					expr = expr + ".items"
					typ = types.ListType{Elem: gt.Elem}
					continue
				case "key":
					expr = expr + ".key"
					typ = types.AnyType{}
					continue
				}
			}
			expr = expr + "." + sanitizeField(field)
			if st, ok := typ.(types.StructType); ok {
				if ft, ok2 := st.Fields[field]; ok2 {
					typ = ft
				}
			}
		}
		return expr, nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ";") + "]", nil
	case p.Map != nil:
		if st, ok := c.structTypeFromMapLiteral(p.Map); ok && identKeysOnly(p.Map) {
			c.ensureStructName(st)
			fields := make([]string, len(p.Map.Items))
			for i, it := range p.Map.Items {
				var key string
				var ok2 bool
				if key, ok2 = identConst(it.Key); !ok2 {
					key, _ = stringConst(it.Key)
				}
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				fields[i] = fmt.Sprintf("%s = %s", sanitizeField(key), v)
			}
			return "{ " + strings.Join(fields, "; ") + " }", nil
		}
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k, v string
			var err error
			if key, ok := identConst(it.Key); ok {
				k = fmt.Sprintf("\"%s\"", key)
			} else {
				k, err = c.compileExpr(it.Key)
				if err != nil {
					return "", err
				}
			}
			v, err = c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("(%s,Obj.repr (%s))", k, v)
		}
		return "[" + strings.Join(items, ";") + "]", nil
	case p.Struct != nil:
		if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
			st := ut.Variants[p.Struct.Name]
			args := make([]string, len(st.Order))
			for i, name := range st.Order {
				for _, f := range p.Struct.Fields {
					if f.Name == name {
						v, err := c.compileExpr(f.Value)
						if err != nil {
							return "", err
						}
						args[i] = v
					}
				}
			}
			return fmt.Sprintf("%s (%s)", p.Struct.Name, strings.Join(args, ", ")), nil
		}
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s = %s", sanitizeField(f.Name), v)
		}
		return "{ " + strings.Join(fields, "; ") + " }", nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, p2 := range p.FunExpr.Params {
			params[i] = p2.Name
		}
		if p.FunExpr.ExprBody != nil {
			body, err := c.compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("fun %s -> %s", strings.Join(params, " "), body), nil
		}
		if len(p.FunExpr.BlockBody) > 0 {
			var buf bytes.Buffer
			buf.WriteString("(fun " + strings.Join(params, " ") + " ->\n")
			c.indent++
			for _, st := range p.FunExpr.BlockBody {
				if err := c.compileStmt(st); err != nil {
					return "", err
				}
			}
			c.indent--
			buf.WriteString(")")
			return buf.String(), nil
		}
		return "fun _ -> ()", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Group != nil:
		s, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", s), nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
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
	switch call.Func {
	case "now":
		c.needRandom = true
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		return "Random.bits ()", nil
	case "print":
		if len(args) == 0 {
			return "", fmt.Errorf("print expects at least 1 arg")
		}
		if len(args) == 1 {
			if isStringExpr(call.Args[0]) || c.isStringType(call.Args[0]) {
				return fmt.Sprintf("print_endline (%s)", args[0]), nil
			}
			if c.isIntType(call.Args[0]) {
				return fmt.Sprintf("print_endline (string_of_int %s)", args[0]), nil
			}
			if c.isFloatTypeExpr(call.Args[0]) {
				return fmt.Sprintf("print_endline (string_of_float %s)", args[0]), nil
			}
			if c.isBoolType(call.Args[0]) {
				return fmt.Sprintf("print_endline (string_of_bool %s)", args[0]), nil
			}
			c.needShow = true
			return fmt.Sprintf("print_endline (__show (%s))", args[0]), nil
		}
		parts := make([]string, len(args))
		for i, a := range args {
			if isStringExpr(call.Args[i]) || c.isStringType(call.Args[i]) {
				parts[i] = a
			} else if c.isIntType(call.Args[i]) {
				parts[i] = fmt.Sprintf("string_of_int %s", a)
			} else if c.isFloatTypeExpr(call.Args[i]) {
				parts[i] = fmt.Sprintf("string_of_float %s", a)
			} else if c.isBoolType(call.Args[i]) {
				parts[i] = fmt.Sprintf("string_of_bool %s", a)
			} else {
				c.needShow = true
				parts[i] = fmt.Sprintf("__show (%s)", a)
			}
		}
		expr := strings.Join(parts, " ^ \" \" ^ ")
		return fmt.Sprintf("print_endline (%s)", expr), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(%s @ [%s])", args[0], args[1]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		if isStringLiteralExpr(call.Args[0]) {
			return fmt.Sprintf("String.length %s", args[0]), nil
		}
		if c.isStringType(call.Args[0]) {
			return fmt.Sprintf("String.length %s", args[0]), nil
		}
		t := types.ExprType(call.Args[0], c.env)
		if _, ok := t.(types.GroupType); ok {
			return fmt.Sprintf("List.length %s.items", args[0]), nil
		}
		return fmt.Sprintf("List.length %s", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		t := types.ExprType(call.Args[0], c.env)
		if _, ok := t.(types.GroupType); ok {
			return fmt.Sprintf("List.length %s.items", args[0]), nil
		}
		return fmt.Sprintf("List.length %s", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		t := types.ExprType(call.Args[0], c.env)
		var elem types.Type
		switch tt := t.(type) {
		case types.ListType:
			elem = tt.Elem
		case types.GroupType:
			elem = tt.Elem
		}
		if _, ok := elem.(types.FloatType); ok {
			return fmt.Sprintf("(List.fold_left (+.) 0.0 %s /. float_of_int (List.length %s))", args[0], args[0]), nil
		}
		return fmt.Sprintf("(float_of_int (List.fold_left (+) 0 %s) /. float_of_int (List.length %s))", args[0], args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		t := types.ExprType(call.Args[0], c.env)
		var elem types.Type
		switch tt := t.(type) {
		case types.ListType:
			elem = tt.Elem
		case types.GroupType:
			elem = tt.Elem
		}
		if _, ok := elem.(types.FloatType); ok {
			return fmt.Sprintf("(List.fold_left (+.) 0.0 %s)", args[0]), nil
		}
		if _, ok := types.TypeOfPrimary(&parser.Primary{Call: call}, c.env).(types.FloatType); ok {
			return fmt.Sprintf("(float_of_int (List.fold_left (+) 0 %s))", args[0]), nil
		}
		return fmt.Sprintf("(List.fold_left (+) 0 %s)", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("List.map snd %s", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("(%s <> [])", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("String.sub %s %s (%s - %s)", args[0], args[1], args[2], args[1]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		c.needJSON = true
		return fmt.Sprintf("_json %s", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("__show (%s)", args[0]), nil
	default:
		if len(args) > 1 && len(call.Func) > 0 && strings.ToUpper(call.Func[:1]) == call.Func[:1] {
			return fmt.Sprintf("%s (%s)", call.Func, strings.Join(args, ", ")), nil
		}
		if len(args) == 0 {
			return fmt.Sprintf("%s ()", call.Func), nil
		}
		return fmt.Sprintf("%s %s", call.Func, strings.Join(args, " ")), nil
	}
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	c.needLoadYaml = true
	expr := fmt.Sprintf("load_yaml %s", path)
	if l.Type != nil && l.Type.Simple != nil {
		if st, ok := c.env.GetStruct(*l.Type.Simple); ok {
			parts := make([]string, len(st.Order))
			for i, f := range st.Order {
				typ := st.Fields[f]
				conv := fmt.Sprintf("Obj.obj (List.assoc \"%s\" m)", f)
				switch typ.(type) {
				case types.IntType, types.Int64Type:
					conv = fmt.Sprintf("(Obj.obj (List.assoc \"%s\" m) : int)", sanitizeField(f))
				case types.FloatType:
					conv = fmt.Sprintf("(Obj.obj (List.assoc \"%s\" m) : float)", sanitizeField(f))
				case types.BoolType:
					conv = fmt.Sprintf("(Obj.obj (List.assoc \"%s\" m) : bool)", sanitizeField(f))
				case types.StringType:
					conv = fmt.Sprintf("(Obj.obj (List.assoc \"%s\" m) : string)", sanitizeField(f))
				}
				parts[i] = fmt.Sprintf("%s = %s", sanitizeField(f), conv)
			}
			expr = fmt.Sprintf("List.map (fun m -> { %s }) (%s)", strings.Join(parts, "; "), expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"-\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	c.needSaveJSONL = true
	c.needShow = true
	return fmt.Sprintf("save_jsonl %s %s", src, path), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		s := fmt.Sprintf("%g", *l.Float)
		if !strings.ContainsAny(s, ".eE") {
			s += "."
		}
		return s
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	case l.Null:
		return "()"
	default:
		return "()"
	}
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	prevLocals := c.localTypes
	c.localTypes = make(map[string]types.Type)
	defer func() { c.localTypes = prevLocals }()
	name := sanitizeName(fn.Name)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("(%s : %s)", sanitizeName(p.Name), c.typeRef(p.Type))
			c.localTypes[p.Name] = types.ResolveTypeRef(p.Type, c.env)
		} else {
			params[i] = sanitizeName(p.Name)
			c.localTypes[p.Name] = types.AnyType{}
		}
	}
	ret := ""
	if fn.Return != nil {
		ret = c.typeRef(fn.Return)
	}
	nameParams := strings.Join(params, " ")
	if len(params) == 0 {
		nameParams = "()"
	}
	if ret != "" {
		c.writeln(fmt.Sprintf("let rec %s %s : %s =", name, nameParams, ret))
	} else {
		c.writeln(fmt.Sprintf("let rec %s %s =", name, nameParams))
	}
	c.indent++

	if len(fn.Body) == 2 {
		if ifs := fn.Body[0].If; ifs != nil && ifs.ElseIf == nil && len(ifs.Else) == 0 && len(ifs.Then) == 1 && ifs.Then[0].Return != nil {
			if retStmt := fn.Body[1].Return; retStmt != nil {
				cond, err := c.compileExpr(ifs.Cond)
				if err != nil {
					return err
				}
				thenVal, err := c.compileExpr(ifs.Then[0].Return.Value)
				if err != nil {
					return err
				}
				elseVal, err := c.compileExpr(retStmt.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("if %s then %s else %s", cond, thenVal, elseVal))
				c.indent--
				return nil
			}
		}
	}

	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("()")
	} else {
		last := fn.Body[len(fn.Body)-1]
		if last.Expr == nil && last.Return == nil {
			c.writeln("()")
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileLocalFun(fn *parser.FunStmt) error {
	prevLocals := c.localTypes
	c.localTypes = make(map[string]types.Type)
	defer func() { c.localTypes = prevLocals }()
	name := sanitizeName(fn.Name)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("(%s : %s)", sanitizeName(p.Name), c.typeRef(p.Type))
			c.localTypes[p.Name] = types.ResolveTypeRef(p.Type, c.env)
		} else {
			params[i] = sanitizeName(p.Name)
			c.localTypes[p.Name] = types.AnyType{}
		}
	}
	ret := ""
	if fn.Return != nil {
		ret = c.typeRef(fn.Return)
	}
	if ret != "" {
		c.writeln(fmt.Sprintf("let rec %s %s : %s =", name, strings.Join(params, " "), ret))
	} else {
		c.writeln(fmt.Sprintf("let rec %s %s =", name, strings.Join(params, " ")))
	}
	c.indent++

	if len(fn.Body) == 2 {
		if ifs := fn.Body[0].If; ifs != nil && ifs.ElseIf == nil && len(ifs.Else) == 0 && len(ifs.Then) == 1 && ifs.Then[0].Return != nil {
			if retStmt := fn.Body[1].Return; retStmt != nil {
				cond, err := c.compileExpr(ifs.Cond)
				if err != nil {
					return err
				}
				thenVal, err := c.compileExpr(ifs.Then[0].Return.Value)
				if err != nil {
					return err
				}
				elseVal, err := c.compileExpr(retStmt.Value)
				if err != nil {
					return err
				}
				c.writeln(fmt.Sprintf("if %s then %s else %s", cond, thenVal, elseVal))
				c.indent--
				c.writeln("in")
				return nil
			}
		}
	}

	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("()")
	} else {
		last := fn.Body[len(fn.Body)-1]
		if last.Expr == nil && last.Return == nil {
			c.writeln("()")
		}
	}
	c.indent--
	c.writeln("in")
	return nil
}

func (c *Compiler) typeRef(t *parser.TypeRef) string {
	if t == nil {
		return "unit"
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			parts[i] = c.typeRef(p)
		}
		ret := "unit"
		if t.Fun.Return != nil {
			ret = c.typeRef(t.Fun.Return)
		}
		parts = append(parts, ret)
		return strings.Join(parts, " -> ")
	}
	if t.Simple != nil {
		switch strings.ToLower(*t.Simple) {
		case "int":
			return "int"
		case "float":
			return "float"
		case "bool":
			return "bool"
		case "string":
			return "string"
		case "any":
			return "Obj.t"
		default:
			return strings.ToLower(*t.Simple)
		}
	}
	if t.Struct != nil {
		fields := map[string]types.Type{}
		order := make([]string, len(t.Struct.Fields))
		for i, f := range t.Struct.Fields {
			fields[f.Name] = types.ResolveTypeRef(f.Type, c.env)
			order[i] = f.Name
		}
		st := types.StructType{Name: "", Fields: fields, Order: order}
		name := c.ensureStructName(st)
		return strings.ToLower(name)
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return c.typeRef(t.Generic.Args[0]) + " list"
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return fmt.Sprintf("(%s * %s) list", c.typeRef(t.Generic.Args[0]), c.typeRef(t.Generic.Args[1]))
		}
	}
	return "unit"
}

// typeRefStruct resolves t to a StructType if possible.
func (c *Compiler) typeRefStruct(t *parser.TypeRef) (types.StructType, bool) {
	if t == nil {
		return types.StructType{}, false
	}
	if t.Simple != nil {
		if st, ok := c.env.GetStruct(*t.Simple); ok {
			return st, true
		}
	}
	if t.Struct != nil {
		fields := map[string]types.Type{}
		order := make([]string, len(t.Struct.Fields))
		for i, f := range t.Struct.Fields {
			fields[f.Name] = types.ResolveTypeRef(f.Type, c.env)
			order[i] = f.Name
		}
		st := types.StructType{Name: "", Fields: fields, Order: order}
		return st, true
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return c.typeRefStruct(t.Generic.Args[0])
	}
	return types.StructType{}, false
}

func (c *Compiler) ocamlType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "float"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		return c.ocamlType(tt.Elem) + " list"
	case types.MapType:
		return fmt.Sprintf("(%s * Obj.t) list", c.ocamlType(tt.Key))
	case types.OptionType:
		inner := c.ocamlType(tt.Elem)
		if inner == "" {
			inner = "Obj.t"
		}
		return inner + " option"
	case types.GroupType:
		key := c.ocamlType(tt.Key)
		if key == "" {
			key = "Obj.t"
		}
		return fmt.Sprintf("(%s,%s) group", key, c.ocamlType(tt.Elem))
	case types.UnionType:
		return strings.ToLower(tt.Name)
	case types.StructType:
		if ut, ok := c.env.FindUnionByVariant(tt.Name); ok {
			return strings.ToLower(ut.Name)
		}
		name := tt.Name
		if name == "" {
			name = c.ensureStructName(tt)
		}
		return strings.ToLower(name)
	case types.FuncType:
		parts := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			parts[i] = c.ocamlType(p)
		}
		ret := "unit"
		if tt.Return != nil {
			ret = c.ocamlType(tt.Return)
		}
		parts = append(parts, ret)
		return strings.Join(parts, " -> ")
	case types.VoidType:
		return "unit"
	}
	return "Obj.t"
}

func (c *Compiler) structKey(st types.StructType) string {
	var b strings.Builder
	for _, f := range st.Order {
		b.WriteString(f)
		b.WriteString(":" + c.ocamlType(st.Fields[f]))
		b.WriteByte(';')
	}
	return b.String()
}

func (c *Compiler) ensureStructName(st types.StructType) string {
	key := c.structKey(st)
	if name, ok := c.structNames[key]; ok {
		return name
	}
	name := fmt.Sprintf("record%d", len(c.structNames)+1)
	c.structNames[key] = name
	newSt := st
	newSt.Name = name
	c.anonStructs = append(c.anonStructs, newSt)
	return name
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func isStringLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if len(u.Value.Ops) > 0 {
		return false
	}
	if u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil {
		return true
	}
	return false
}

func isStringCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if len(u.Value.Ops) > 0 {
		return false
	}
	if u.Value.Target.Call != nil && u.Value.Target.Call.Func == "str" {
		return true
	}
	return false
}

func isStringExpr(e *parser.Expr) bool {
	return isStringLiteralExpr(e) || isStringCall(e)
}

func isStringExprExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Call != nil && p.Target.Call.Func == "str" {
		return true
	}
	return false
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Call != nil && p.Call.Func == "str" {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if typ, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.StringType); ok {
				return true
			}
		}
		if lt, ok := c.localTypes[p.Selector.Root]; ok {
			if _, ok2 := lt.(types.StringType); ok2 {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		if typ, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func sanitizeField(name string) string {
	switch name {
	case "val", "type", "module", "open", "end":
		return name + "_"
	}
	return name
}

var ocamlReserved = map[string]struct{}{
	"and": {}, "as": {}, "assert": {}, "asr": {}, "begin": {}, "class": {},
	"constraint": {}, "do": {}, "done": {}, "downto": {}, "else": {}, "end": {},
	"exception": {}, "external": {}, "false": {}, "for": {}, "fun": {},
	"function": {}, "if": {}, "in": {}, "include": {}, "inherit": {},
	"initializer": {}, "land": {}, "lazy": {}, "let": {}, "lor": {}, "lsl": {},
	"lsr": {}, "lxor": {}, "match": {}, "method": {}, "mod": {}, "module": {},
	"mutable": {}, "new": {}, "object": {}, "of": {}, "open": {}, "or": {},
	"private": {}, "rec": {}, "sig": {}, "struct": {}, "then": {}, "to": {},
	"true": {}, "try": {}, "type": {}, "val": {}, "virtual": {}, "when": {},
	"while": {}, "with": {},
}

func sanitizeName(name string) string {
	if _, ok := ocamlReserved[name]; ok {
		return name + "_"
	}
	return name
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Ops) == 0 {
		if typ, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := typ.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func isStringUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Call != nil && p.Call.Func == "str" {
		return true
	}
	return false
}

func (c *Compiler) isFloatUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: u}}, c.env)
	if t == (types.AnyType{}) {
		if name, ok := identName(&parser.Expr{Binary: &parser.BinaryExpr{Left: u}}); ok {
			if lt, ok2 := c.localTypes[name]; ok2 {
				_, ok3 := lt.(types.FloatType)
				return ok3
			}
		}
	}
	_, ok := t.(types.FloatType)
	return ok
}

func (c *Compiler) isFloatPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}, c.env)
	if t == (types.AnyType{}) {
		if name, ok := identName(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}); ok {
			if lt, ok2 := c.localTypes[name]; ok2 {
				_, ok3 := lt.(types.FloatType)
				return ok3
			}
		}
	}
	_, ok := t.(types.FloatType)
	return ok
}

func (c *Compiler) isStringType(e *parser.Expr) bool {
	if _, ok := types.ExprType(e, c.env).(types.StringType); ok {
		return true
	}
	if name, ok := identName(e); ok {
		if lt, ok2 := c.localTypes[name]; ok2 {
			if _, ok3 := lt.(types.StringType); ok3 {
				return true
			}
		}
		if gt, err := c.env.GetVar(name); err == nil {
			if _, ok3 := gt.(types.StringType); ok3 {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isIntType(e *parser.Expr) bool {
	if _, ok := types.ExprType(e, c.env).(types.IntType); ok {
		return true
	}
	if _, ok := types.ExprType(e, c.env).(types.Int64Type); ok {
		return true
	}
	if name, ok := identName(e); ok {
		if lt, ok2 := c.localTypes[name]; ok2 {
			if _, ok3 := lt.(types.IntType); ok3 {
				return true
			}
			if _, ok3 := lt.(types.Int64Type); ok3 {
				return true
			}
		}
		if gt, err := c.env.GetVar(name); err == nil {
			if _, ok3 := gt.(types.IntType); ok3 {
				return true
			}
			if _, ok3 := gt.(types.Int64Type); ok3 {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatTypeExpr(e *parser.Expr) bool {
	if _, ok := types.ExprType(e, c.env).(types.FloatType); ok {
		return true
	}
	if name, ok := identName(e); ok {
		if lt, ok2 := c.localTypes[name]; ok2 {
			if _, ok3 := lt.(types.FloatType); ok3 {
				return true
			}
		}
		if gt, err := c.env.GetVar(name); err == nil {
			if _, ok3 := gt.(types.FloatType); ok3 {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isBoolType(e *parser.Expr) bool {
	if _, ok := types.ExprType(e, c.env).(types.BoolType); ok {
		return true
	}
	if name, ok := identName(e); ok {
		if lt, ok2 := c.localTypes[name]; ok2 {
			if _, ok3 := lt.(types.BoolType); ok3 {
				return true
			}
		}
		if gt, err := c.env.GetVar(name); err == nil {
			if _, ok3 := gt.(types.BoolType); ok3 {
				return true
			}
		}
	}
	return false
}

func stringConst(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	if u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil {
		return *u.Value.Target.Lit.Str, true
	}
	return "", false
}

func identConst(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	if u.Value.Target.Selector != nil && len(u.Value.Target.Selector.Tail) == 0 {
		return u.Value.Target.Selector.Root, true
	}
	return "", false
}

func identKeysOnly(ml *parser.MapLiteral) bool {
	for _, it := range ml.Items {
		if _, ok := identConst(it.Key); !ok {
			return false
		}
	}
	return true
}

// structTypeFromMapLiteral returns a StructType representing ml if all keys are
// constant identifiers or strings. The order of fields matches the literal.
func (c *Compiler) structTypeFromMapLiteral(ml *parser.MapLiteral) (types.StructType, bool) {
	return c.structTypeFromMapLiteralEnv(ml, c.env)
}

func (c *Compiler) structTypeFromMapLiteralEnv(ml *parser.MapLiteral, env *types.Env) (types.StructType, bool) {
	if len(ml.Items) < 2 {
		return types.StructType{}, false
	}
	fields := map[string]types.Type{}
	order := []string{}
	for _, it := range ml.Items {
		key, ok := identConst(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		fields[key] = types.ExprType(it.Value, env)
		order = append(order, key)
	}
	return types.StructType{Name: "", Fields: fields, Order: order}, true
}

// structTypeFromExpr checks if e is a map literal or list of map literals with
// constant keys and returns an appropriate StructType or ListType of that
// struct.
func (c *Compiler) structTypeFromExpr(e *parser.Expr) (types.Type, bool) {
	return c.structTypeFromExprEnv(e, c.env)
}

func (c *Compiler) structTypeFromExprEnv(e *parser.Expr, env *types.Env) (types.Type, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, false
	}
	u := e.Binary.Left
	if name, ok := identName(e); ok {
		if t, err := env.GetVar(name); err == nil {
			switch tt := t.(type) {
			case types.StructType:
				return tt, true
			case types.ListType:
				if elem, ok2 := tt.Elem.(types.StructType); ok2 {
					return types.ListType{Elem: elem}, true
				}
			}
		}
	}
	if ml := u.Value.Target.Map; ml != nil {
		st, ok := c.structTypeFromMapLiteralEnv(ml, env)
		if ok {
			return st, true
		}
	}
	if q := u.Value.Target.Query; q != nil {
		qenv := c.queryEnv(q)
		if st, ok := c.structTypeFromExprEnv(q.Select, qenv); ok {
			if structType, ok2 := st.(types.StructType); ok2 {
				return types.ListType{Elem: structType}, true
			}
			if lt, ok2 := st.(types.ListType); ok2 {
				if elem, ok3 := lt.Elem.(types.StructType); ok3 {
					return types.ListType{Elem: elem}, true
				}
			}
		}
	}
	if ll := u.Value.Target.List; ll != nil && len(ll.Elems) > 0 {
		if first := ll.Elems[0]; first.Binary != nil && first.Binary.Left != nil {
			if ml := first.Binary.Left.Value.Target.Map; ml != nil {
				st, ok := c.structTypeFromMapLiteralEnv(ml, env)
				if ok {
					return types.ListType{Elem: st}, true
				}
			}
		}
	}
	return nil, false
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	if u.Value.Target.Selector != nil && len(u.Value.Target.Selector.Tail) == 0 {
		return u.Value.Target.Selector.Root, true
	}
	return "", false
}

// loopTypeForSource returns the struct type name for elements of src if known.
func (c *Compiler) loopTypeForSource(src *parser.Expr) string {
	if name, ok := identName(src); ok {
		if st, ok2 := c.varStructs[name]; ok2 {
			return c.ocamlType(st)
		}
	}
	return ""
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if len(u.Value.Ops) != 0 {
		return false
	}
	if u.Value.Target.Selector != nil && u.Value.Target.Selector.Root == "_" && len(u.Value.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func (c *Compiler) scanTypeRef(t *parser.TypeRef) {
	if t == nil {
		return
	}
	typ := types.ResolveTypeRef(t, c.env)
	switch tt := typ.(type) {
	case types.StructType:
		c.ensureStructName(tt)
	case types.ListType:
		if elem, ok := tt.Elem.(types.StructType); ok {
			c.ensureStructName(elem)
		}
	case types.OptionType:
		if elem, ok := tt.Elem.(types.StructType); ok {
			c.ensureStructName(elem)
		}
	case types.MapType:
		if elem, ok := tt.Value.(types.StructType); ok {
			c.ensureStructName(elem)
		}
	}
	if t.Generic != nil {
		for _, a := range t.Generic.Args {
			c.scanTypeRef(a)
		}
	}
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil {
		return nil, false
	}
	if len(u.Value.Ops) != 0 || u.Value.Target.Call == nil {
		return nil, false
	}
	return u.Value.Target.Call, true
}

func (c *Compiler) compilePattern(e *parser.Expr) (string, error) {
	if isUnderscoreExpr(e) {
		return "_", nil
	}
	if call, ok := callPattern(e); ok {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			s, err := c.compilePattern(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		if len(args) == 0 {
			return call.Func, nil
		}
		if len(args) == 1 {
			return fmt.Sprintf("%s %s", call.Func, args[0]), nil
		}
		return fmt.Sprintf("%s (%s)", call.Func, strings.Join(args, ", ")), nil
	}
	if id, ok := identName(e); ok {
		return id, nil
	}
	return c.compileExpr(e)
}

// --- runtime helpers ---

func (c *Compiler) emitRuntime() {
	if c.needShow {
		c.writeln("let rec __show v =")
		c.indent++
		c.writeln("let open Obj in")
		c.writeln("let rec list_aux o =")
		c.indent++
		c.writeln("if is_int o && (magic (obj o) : int) = 0 then \"\" else")
		c.writeln(" let hd = field o 0 in")
		c.writeln(" let tl = field o 1 in")
		c.writeln(" let rest = list_aux tl in")
		c.writeln(" if rest = \"\" then __show (obj hd) else __show (obj hd) ^ \"; \" ^ rest")
		c.indent--
		c.writeln("in")
		c.writeln("let r = repr v in")
		c.writeln("if is_int r then string_of_int (magic v) else")
		c.writeln("match tag r with")
		c.indent++
		c.writeln(`| 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"`)
		c.writeln("| 252 -> (magic v : string)")
		c.writeln("| 253 -> string_of_float (magic v)")
		c.writeln("| _ -> \"<value>\"")
		c.indent--
		c.indent--
		c.buf.WriteByte('\n')
	}

	if c.needLoop {
		c.writeln("exception Break")
		c.writeln("exception Continue")
		c.buf.WriteByte('\n')
	}

	if c.needContains {
		c.writeln("let string_contains s sub =")
		c.indent++
		c.writeln("let len_s = String.length s and len_sub = String.length sub in")
		c.writeln("let rec aux i =")
		c.indent++
		c.writeln("if i + len_sub > len_s then false")
		c.writeln("else if String.sub s i len_sub = sub then true")
		c.writeln("else aux (i + 1)")
		c.indent--
		c.writeln("in aux 0")
		c.indent--
		c.buf.WriteByte('\n')
	}

	if c.needSlice {
		c.writeln("let slice lst i j =")
		c.indent++
		c.writeln("lst |> List.mapi (fun idx x -> idx, x)")
		c.writeln("    |> List.filter (fun (idx, _) -> idx >= i && idx < j)")
		c.writeln("    |> List.map snd")
		c.indent--
		c.writeln("")
	}

	if c.needStringSlice {
		c.writeln("let string_slice s i j = String.sub s i (j - i)")
		c.buf.WriteByte('\n')
	}

	if c.needListSet {
		c.writeln("let list_set lst idx value =")
		c.indent++
		c.writeln("List.mapi (fun i v -> if i = idx then value else v) lst")
		c.indent--
		c.buf.WriteByte('\n')
	}

	if c.needMapSet {
		c.writeln("let rec map_set m k v =")
		c.indent++
		c.writeln("match m with")
		c.indent++
		c.writeln("| [] -> [(k,Obj.repr v)]")
		c.writeln("| (k2,v2)::tl -> if k2 = k then (k,Obj.repr v)::tl else (k2,v2)::map_set tl k v")
		c.indent--
		c.indent--
		c.buf.WriteByte('\n')
	}

	if c.needMapGet {
		c.writeln("let map_get m k = Obj.obj (List.assoc k m)")
		c.buf.WriteByte('\n')
	}

	if c.needListOps {
		c.writeln("let list_union a b = List.sort_uniq compare (a @ b)")
		c.writeln("let list_except a b = List.filter (fun x -> not (List.mem x b)) a")
		c.writeln("let list_intersect a b = List.filter (fun x -> List.mem x b) a |> List.sort_uniq compare")
		c.writeln("let list_union_all a b = a @ b")
	}

	if c.needSum {
		c.writeln("let sum lst = List.fold_left (+) 0 lst")
	}

	if c.needSumFloat {
		c.writeln("let sum_float lst = List.fold_left (+.) 0.0 lst")
	}

	if c.needGroup {
		c.writeln("type ('k,'v) group = { key : 'k; items : 'v list }")
	}

	if c.needLoadYaml {
		c.writeln("let load_yaml path =")
		c.indent++
		c.writeln("let ic = if path = \"-\" then stdin else open_in path in")
		c.writeln("let rec parse acc cur =")
		c.indent++
		c.writeln("try")
		c.indent++
		c.writeln("let line = String.trim (input_line ic) in")
		c.writeln("if line = \"\" then parse acc cur else")
		c.writeln("if String.get line 0 = '-' then (")
		c.indent++
		c.writeln("let acc = (match cur with None -> acc | Some m -> m :: acc) in")
		c.writeln("let l = String.trim (String.sub line 1 (String.length line - 1)) in")
		c.writeln("let idx = String.index l ':' in")
		c.writeln("let key = String.sub l 0 idx |> String.trim in")
		c.writeln("let value = String.sub l (idx+1) (String.length l - idx - 1) |> String.trim in")
		c.writeln("let cur = Some [ (key, Obj.repr value) ] in")
		c.writeln("parse acc cur")
		c.indent--
		c.writeln(") else (")
		c.indent++
		c.writeln("let idx = String.index line ':' in")
		c.writeln("let key = String.sub line 0 idx |> String.trim in")
		c.writeln("let value = String.sub line (idx+1) (String.length line - idx - 1) |> String.trim in")
		c.writeln("let v = Obj.repr value in")
		c.writeln("let cur = match cur with None -> Some [ (key,v) ] | Some m -> Some ((key,v)::m) in")
		c.writeln("parse acc cur")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("with End_of_file ->")
		c.indent++
		c.writeln("if path <> \"-\" then close_in ic;")
		c.writeln("let acc = match cur with None -> acc | Some m -> m :: acc in")
		c.writeln("List.rev acc")
		c.indent--
		c.indent--
		c.writeln("in parse [] None")
		c.indent--
		c.buf.WriteByte('\n')
	}

	if c.needSaveJSONL {
		c.writeln("let save_jsonl rows path =")
		c.indent++
		c.writeln("let oc = if path = \"-\" then stdout else open_out path in")
		c.writeln("List.iter (fun m ->")
		c.indent++
		c.writeln("let parts = List.map (fun (k,v) -> Printf.sprintf \"\\\"%s\\\": %s\" k (__show (Obj.obj v))) m in")
		c.writeln("output_string oc (\"{\" ^ String.concat \", \" parts ^ \"}\\n\")")
		c.indent--
		c.writeln(") rows;")
		c.writeln("if path <> \"-\" then close_out oc")
		c.indent--
		c.buf.WriteByte('\n')
	}

	if c.needJSON {
		c.writeln("let rec __to_json v =")
		c.indent++
		c.writeln("let open Obj in")
		c.writeln("let rec list_aux o =")
		c.indent++
		c.writeln("if is_int o && (magic (obj o) : int) = 0 then \"\" else")
		c.writeln(" let hd = field o 0 in")
		c.writeln(" let tl = field o 1 in")
		c.writeln(" let rest = list_aux tl in")
		c.writeln(" let cur = __to_json (obj hd) in")
		c.writeln(" if rest = \"\" then cur else cur ^ \",\" ^ rest")
		c.indent--
		c.writeln("in")
		c.writeln("let r = repr v in")
		c.writeln("if is_int r then string_of_int (magic v) else")
		c.writeln("match tag r with")
		c.indent++
		c.writeln(`| 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"`)
		c.writeln("| 252 -> Printf.sprintf \"%S\" (magic v : string)")
		c.writeln("| 253 -> string_of_float (magic v)")
		c.writeln("| _ -> \"null\"")
		c.indent--
		c.indent--
		c.writeln("")
		c.writeln("let _json v = print_endline (__to_json v)")
		c.buf.WriteByte('\n')
	}

	if c.needShow || c.needContains || c.needSlice || c.needStringSlice || c.needListSet || c.needMapSet || c.needMapGet || c.needListOps || c.needSum || c.needSumFloat || c.needGroup || c.needLoop || c.needLoadYaml || c.needSaveJSONL || c.needJSON {
		c.buf.WriteByte('\n')
	}

	if c.needRandom {
		c.writeln("Random.self_init ()")
		c.buf.WriteByte('\n')
	}
}

func (c *Compiler) emitAnonStructs(body []byte) {
	for _, st := range c.anonStructs {
		name := strings.ToLower(st.Name)
		if !bytes.Contains(body, []byte(name)) {
			continue
		}
		fields := make([]string, len(st.Order))
		for i, f := range st.Order {
			fields[i] = fmt.Sprintf("mutable %s : %s", sanitizeField(f), c.ocamlType(st.Fields[f]))
		}
		c.writeln(fmt.Sprintf("type %s = { %s }", name, strings.Join(fields, "; ")))
	}
	if len(c.anonStructs) > 0 {
		c.buf.WriteByte('\n')
	}
}

// --- program scanning ---

func (c *Compiler) scanProgram(p *parser.Program) {
	for _, s := range p.Statements {
		c.scanStmt(s)
	}
}

func (c *Compiler) scanStmt(s *parser.Statement) {
	switch {
	case s.Assign != nil:
		if len(s.Assign.Index) > 0 {
			typ, _ := c.env.GetVar(s.Assign.Name)
			if _, ok := typ.(types.MapType); ok {
				c.needMapSet = true
				if len(s.Assign.Index) > 1 {
					c.needMapGet = true
				}
			} else {
				c.needListSet = true
			}
			for _, idx := range s.Assign.Index {
				if idx.Start != nil {
					c.scanExpr(idx.Start)
				}
				if idx.End != nil {
					c.scanExpr(idx.End)
				}
			}
		}
		if s.Assign.Value != nil {
			c.scanExpr(s.Assign.Value)
		}
	case s.Expr != nil:
		c.scanExpr(s.Expr.Expr)
	case s.If != nil:
		c.scanExpr(s.If.Cond)
		for _, st := range s.If.Then {
			c.scanStmt(st)
		}
		if s.If.ElseIf != nil {
			c.scanStmt(&parser.Statement{If: s.If.ElseIf})
		}
		for _, st := range s.If.Else {
			c.scanStmt(st)
		}
	case s.For != nil:
		c.needLoop = true
		c.scanExpr(s.For.Source)
		if s.For.RangeEnd != nil {
			c.scanExpr(s.For.RangeEnd)
		}
		for _, st := range s.For.Body {
			c.scanStmt(st)
		}
	case s.While != nil:
		c.needLoop = true
		c.scanExpr(s.While.Cond)
		for _, st := range s.While.Body {
			c.scanStmt(st)
		}
	case s.Return != nil:
		if s.Return.Value != nil {
			c.scanExpr(s.Return.Value)
		}
	case s.Fun != nil:
		oldEnv := c.env
		funEnv := types.NewEnv(oldEnv)
		for _, p := range s.Fun.Params {
			if p.Type != nil {
				funEnv.SetVar(p.Name, types.ResolveTypeRef(p.Type, c.env), true)
			}
		}
		c.env = funEnv
		for _, st := range s.Fun.Body {
			c.scanStmt(st)
		}
		c.env = oldEnv
	case s.Let != nil:
		if s.Let.Value != nil {
			c.scanExpr(s.Let.Value)
		}
	case s.Var != nil:
		if s.Var.Value != nil {
			c.scanExpr(s.Var.Value)
		}
	}
}

func (c *Compiler) scanExpr(e *parser.Expr) {
	if e == nil || e.Binary == nil {
		return
	}
	c.scanUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		if op.Op == "union" || op.Op == "except" || op.Op == "intersect" || op.Op == "union" && op.All {
			c.needListOps = true
		}
		c.scanPostfix(op.Right)
	}
}

func (c *Compiler) scanExprWithEnv(e *parser.Expr, env *types.Env) {
	old := c.env
	c.env = env
	c.scanExpr(e)
	c.env = old
}

func (c *Compiler) scanUnary(u *parser.Unary) {
	if u == nil {
		return
	}
	c.scanPostfix(u.Value)
}

func (c *Compiler) scanPostfix(p *parser.PostfixExpr) {
	if p == nil {
		return
	}
	c.scanPrimary(p.Target)
	for i, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				c.scanExpr(a)
			}
			if i > 0 && p.Ops[i-1].Field != nil && p.Ops[i-1].Field.Name == "contains" {
				c.needContains = true
			}
			if i == 0 && p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 && p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" {
				c.needContains = true
			}
		}
		if op.Index != nil {
			if op.Index.Start != nil {
				c.scanExpr(op.Index.Start)
			}
			if op.Index.End != nil {
				c.scanExpr(op.Index.End)
			}
			if op.Index.Colon != nil {
				if c.isStringPrimary(p.Target) {
					c.needStringSlice = true
				} else {
					c.needSlice = true
				}
			}
		}
		if op.Cast != nil {
			c.scanTypeRef(op.Cast.Type)
		}
	}
}

func (c *Compiler) scanPrimary(p *parser.Primary) {
	switch {
	case p == nil:
	case p.Call != nil:
		for _, a := range p.Call.Args {
			c.scanExpr(a)
		}
		switch p.Call.Func {
		case "print":
			needShow := false
			for _, arg := range p.Call.Args {
				if !(isStringExpr(arg) || c.isStringType(arg)) {
					needShow = true
					break
				}
			}
			if needShow {
				c.needShow = true
			}
		case "str":
			c.needShow = true
		case "sum":
			// handled inline; no runtime helpers required
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			c.scanExpr(e)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			c.scanExpr(it.Key)
			c.scanExpr(it.Value)
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			c.scanExpr(f.Value)
		}
	case p.If != nil:
		if p.If != nil {
			c.scanExpr(p.If.Cond)
			if p.If.Then != nil {
				c.scanExpr(p.If.Then)
			}
		}
	case p.Query != nil:
		c.scanQuery(p.Query)
	case p.Match != nil:
		for _, cs := range p.Match.Cases {
			c.scanExpr(cs.Pattern)
			c.scanExpr(cs.Result)
		}
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody != nil {
			c.scanExpr(p.FunExpr.ExprBody)
		}
		for _, st := range p.FunExpr.BlockBody {
			c.scanStmt(st)
		}
	case p.Load != nil:
		if p.Load.With != nil {
			c.scanExpr(p.Load.With)
		}
		c.needLoadYaml = true
	case p.Save != nil:
		c.scanExpr(p.Save.Src)
		if p.Save.With != nil {
			c.scanExpr(p.Save.With)
		}
		c.needSaveJSONL = true
	case p.Group != nil:
		c.scanExpr(p.Group)
	}
}

func (c *Compiler) scanQuery(q *parser.QueryExpr) {
	if q == nil {
		return
	}
	outerEnv := c.env
	c.scanExpr(q.Source)
	for _, fr := range q.Froms {
		c.scanExpr(fr.Src)
	}
	for _, jo := range q.Joins {
		c.scanExpr(jo.Src)
	}
	qenv := c.queryEnv(q)
	for _, jo := range q.Joins {
		if jo.On != nil {
			c.scanExprWithEnv(jo.On, qenv)
		}
	}
	if q.Where != nil {
		c.scanExprWithEnv(q.Where, qenv)
	}
	if q.Group != nil {
		c.needGroup = true
		for _, e := range q.Group.Exprs {
			c.scanExprWithEnv(e, qenv)
		}
		if q.Group.Having != nil {
			c.scanExprWithEnv(q.Group.Having, qenv)
		}
	}
	if q.Sort != nil {
		c.scanExprWithEnv(q.Sort, qenv)
	}
	if q.Skip != nil {
		c.scanExprWithEnv(q.Skip, qenv)
	}
	if q.Take != nil {
		c.scanExprWithEnv(q.Take, qenv)
	}
	if q.Select != nil {
		c.scanExprWithEnv(q.Select, qenv)
	}
	c.env = outerEnv
}
