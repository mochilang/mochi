package dartcode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Dart source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	tempVarCount int
	imports      map[string]bool
	dartImports  map[string]string
	packages     map[string]bool
	structs      map[string]bool
	agents       map[string]bool
	handlerCount int
	helpers      map[string]bool
	models       bool
	structRegs   []string
}

func containsStreamCode(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if stmtHasStream(s) {
			return true
		}
	}
	return false
}

var fetchExprType = reflect.TypeOf(&parser.FetchExpr{})

func containsFetchExpr(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if hasFetchExpr(reflect.ValueOf(s)) {
			return true
		}
	}
	return false
}

func hasFetchExpr(v reflect.Value) bool {
	if !v.IsValid() {
		return false
	}
	if v.Type() == fetchExprType {
		return !v.IsNil()
	}
	switch v.Kind() {
	case reflect.Ptr, reflect.Interface:
		if v.IsNil() {
			return false
		}
		return hasFetchExpr(v.Elem())
	case reflect.Slice, reflect.Array:
		for i := 0; i < v.Len(); i++ {
			if hasFetchExpr(v.Index(i)) {
				return true
			}
		}
	case reflect.Struct:
		for i := 0; i < v.NumField(); i++ {
			if hasFetchExpr(v.Field(i)) {
				return true
			}
		}
	}
	return false
}

func stmtHasStream(s *parser.Statement) bool {
	switch {
	case s.Stream != nil, s.Emit != nil, s.On != nil, s.Agent != nil:
		return true
	case s.Fun != nil:
		return containsStreamCode(s.Fun.Body)
	case s.Test != nil:
		return containsStreamCode(s.Test.Body)
	case s.If != nil:
		if containsStreamCode(s.If.Then) {
			return true
		}
		if s.If.ElseIf != nil {
			if stmtHasStream(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		return containsStreamCode(s.If.Else)
	case s.While != nil:
		return containsStreamCode(s.While.Body)
	case s.For != nil:
		return containsStreamCode(s.For.Body)
	case s.On != nil:
		return containsStreamCode(s.On.Body)
	}
	return false
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, imports: make(map[string]bool), dartImports: make(map[string]string), packages: make(map[string]bool),
		structs: make(map[string]bool), agents: make(map[string]bool), helpers: make(map[string]bool), models: false, structRegs: []string{}}
}

// initState resets compiler state before a compilation run.
func (c *Compiler) initState() {
	c.buf.Reset()
	c.imports = make(map[string]bool)
	c.dartImports = make(map[string]string)
	c.packages = make(map[string]bool)
	c.structs = make(map[string]bool)
	c.agents = make(map[string]bool)
	c.helpers = make(map[string]bool)
	c.handlerCount = 0
	c.models = false
	c.structRegs = nil
}

// Compile returns Dart source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	needsAsync := containsStreamCode(prog.Statements) || containsFetchExpr(prog.Statements)
	c.initState()

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body

	if err := c.compileTypeDecls(prog.Statements); err != nil {
		return nil, err
	}
	if err := c.compileFunDecls(prog.Statements); err != nil {
		return nil, err
	}
	if err := c.compileGlobals(prog.Statements); err != nil {
		return nil, err
	}
	if err := c.compileTestBlocks(prog.Statements); err != nil {
		return nil, err
	}
	if err := c.compileMain(prog.Statements, needsAsync); err != nil {
		return nil, err
	}
	bodyBytes := c.buf.Bytes()

	c.buf = oldBuf
	if prog.Package != "" {
		c.writeln("library " + sanitizeName(prog.Package) + ";")
		c.writeln("")
	}
	if len(c.imports) > 0 || len(c.dartImports) > 0 {
		for imp := range c.imports {
			c.writeln(fmt.Sprintf("import '%s';", imp))
		}
		for alias, path := range c.dartImports {
			if alias == path {
				c.writeln(fmt.Sprintf("import '%s';", path))
			} else {
				c.writeln(fmt.Sprintf("import '%s' as %s;", path, alias))
			}
		}
		c.writeln("")
	}
	if c.models {
		c.writeln("Map<String, Map<String, dynamic>> _models = {};")
		c.writeln("")
	}
	if len(c.structs) > 0 {
		c.writeln("Map<String, Function> _structParsers = {};")
		c.writeln("")
	}
	c.buf.Write(bodyBytes)
	c.emitRuntime()
	c.buf.WriteByte('\n')
	out := c.buf.Bytes()
	return FormatDart(out), nil
}

func (c *Compiler) compileTypeDecls(stmts []*parser.Statement) error {
	for _, s := range stmts {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileFunDecls(stmts []*parser.Statement) error {
	for _, s := range stmts {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileGlobals(stmts []*parser.Statement) error {
	for _, s := range stmts {
		if s.Let != nil || s.Var != nil {
			if err := c.compileStmt(s); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileTestBlocks(stmts []*parser.Statement) error {
	for _, s := range stmts {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return err
			}
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileMain(stmts []*parser.Statement, needsAsync bool) error {
	hasTests := hasTest(stmts)
	if hasTests {
		c.imports["dart:io"] = true
	}
	if needsAsync {
		c.writeln("Future<void> main() async {")
	} else {
		c.writeln("void main() {")
	}
	c.indent++
	if hasTests {
		c.writeln("int failures = 0;")
	}
	for _, r := range c.structRegs {
		c.writeln(r)
	}
	if len(c.structRegs) > 0 {
		c.writeln("")
	}
	for _, s := range stmts {
		if s.Fun != nil || s.Type != nil || s.Test != nil || s.Let != nil || s.Var != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	for _, s := range stmts {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			if hasTests {
				c.use("_runTest")
				c.use("_formatDuration")
				c.writeln(fmt.Sprintf("if (!_runTest(%q, %s)) failures++;", s.Test.Name, name))
			} else {
				c.writeln(fmt.Sprintf("%s();", name))
			}
		}
	}
	if needsAsync {
		c.use("_waitAll")
		c.writeln("await _waitAll();")
	}
	if hasTests {
		c.writeln("if (failures > 0) {")
		c.indent++
		c.writeln("print(\"\\n[FAIL] $failures test(s) failed.\");")
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	return nil
}

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Type != nil:
		if err := c.compileTypeDecl(s.Type); err != nil {
			return err
		}
		c.writeln("")
		return nil
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Agent != nil:
		return c.compileAgentDecl(s.Agent)
	case s.Model != nil:
		return c.compileModelDecl(s.Model)
	case s.Import != nil:
		if s.Import.Lang == nil {
			return c.compilePackageImport(s.Import)
		}
		if *s.Import.Lang == "dart" {
			return c.compileDartImport(s.Import)
		}
		return fmt.Errorf("unsupported import language: %v", s.Import.Lang)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		// extern declarations have no runtime effect in Dart
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	var typ types.Type
	if s.Type != nil {
		typ = c.resolveTypeRef(s.Type)
	}
	if c.env != nil && typ == nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if (typ == nil || isAny(typ)) && s.Value != nil {
		typ = c.exprType(s.Value)
	}
	var val string
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeVarDecl(name, typ, val)
	if c.env != nil && typ != nil && !isAny(typ) {
		c.env.SetVar(s.Name, typ, false)
	}
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if s.ElseIf != nil {
		c.writeln("} else ")
		return c.compileIf(s.ElseIf)
	}
	if len(s.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
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

func (c *Compiler) compileFor(s *parser.ForStmt) error {
	name := sanitizeName(s.Name)
	loopVar := name
	if s.Name == "_" {
		loopVar = c.newVar()
	}
	if s.RangeEnd != nil {
		start, err := c.compileExpr(s.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (var %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
	} else {
		src, err := c.compileExpr(s.Source)
		if err != nil {
			return err
		}
		iterVar := src
		needTemp := isMapExpr(c, s.Source) || isStringExpr(c, s.Source)
		if needTemp {
			tmp := c.newVar()
			c.writeln(fmt.Sprintf("var %s = %s;", tmp, src))
			iterVar = tmp
		}
		if isMapExpr(c, s.Source) {
			c.writeln(fmt.Sprintf("for (var %s in %s.values) {", loopVar, iterVar))
		} else if isStringExpr(c, s.Source) {
			tmp := c.newVar()
			c.writeln(fmt.Sprintf("for (var %s in %s.runes) {", tmp, iterVar))
			if s.Name != "_" {
				c.indent++
				c.writeln(fmt.Sprintf("var %s = String.fromCharCode(%s);", loopVar, tmp))
				c.indent--
			}
		} else {
			c.writeln(fmt.Sprintf("for (var %s in %s) {", loopVar, iterVar))
		}
	}
	c.indent++
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	var typ types.Type
	if s.Type != nil {
		typ = c.resolveTypeRef(s.Type)
	}
	if c.env != nil && typ == nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if (typ == nil || isAny(typ)) && s.Value != nil {
		typ = c.exprType(s.Value)
	}
	var val string
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeVarDecl(name, typ, val)
	if c.env != nil && typ != nil && !isAny(typ) {
		c.env.SetVar(s.Name, typ, true)
	}
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	for _, idx := range s.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			if isInt(t) || isInt64(t) {
				if !isIntExpr(c, s.Value) {
					rhs = fmt.Sprintf("(%s).toInt()", rhs)
				}
			}
		}
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	c.compileStructType(st)
	varName := "_" + sanitizeName(s.Name) + "Stream"
	c.writeln(fmt.Sprintf("var %s = _Stream<%s>('%s');", varName, sanitizeName(st.Name), s.Name))
	c.use("_Stream")
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	c.compileStructType(st)
	streamVar := "_" + sanitizeName(h.Stream) + "Stream"
	handlerName := fmt.Sprintf("_handler_%d", c.handlerCount)
	c.handlerCount++
	c.writeln(fmt.Sprintf("void %s(%s ev) {", handlerName, sanitizeName(st.Name)))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("var %s = ev;", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.indent--
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s.register(%s);", streamVar, handlerName))
	c.use("_Stream")
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	c.compileStructType(st)
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
	}
	lit := fmt.Sprintf("%s({%s})", sanitizeName(st.Name), strings.Join(parts, ", "))
	streamVar := "_" + sanitizeName(e.Stream) + "Stream"
	c.writeln(fmt.Sprintf("%s.append(%s);", streamVar, lit))
	c.use("_Stream")
	return nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	listVar := sanitizeName(u.Target)
	idxVar := c.newVar()
	itemVar := c.newVar()
	c.writeln(fmt.Sprintf("for (var %s = 0; %s < %s.length; %s++) {", idxVar, idxVar, listVar, idxVar))
	c.indent++
	c.writeln(fmt.Sprintf("var %s = %s[%s];", itemVar, listVar, idxVar))

	origEnv := c.env
	child := types.NewEnv(c.env)
	var fields []string
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					for _, fn := range st.Order {
						fields = append(fields, fn)
						child.SetVar(fn, st.Fields[fn], true)
						c.writeln(fmt.Sprintf("var %s = %s.%s;", sanitizeName(fn), itemVar, sanitizeName(fn)))
					}
				}
			}
		}
	}
	c.env = child

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln("if (" + cond + ") {")
		c.indent++
	}

	for _, it := range u.Set.Items {
		key, ok := identName(it.Key)
		if !ok {
			c.env = origEnv
			return fmt.Errorf("unsupported update key")
		}
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("%s.%s = %s;", itemVar, sanitizeName(key), val))
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.indent--
	c.writeln("}")
	c.env = origEnv
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", nil
	}

	operands := []string{}
	operators := []string{}
	posts := []*parser.PostfixExpr{}
	floats := []bool{}
	strings := []bool{}
	typesList := []types.Type{}

	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, left)
	floats = append(floats, isFloatUnary(c, b.Left))
	strings = append(strings, isStringUnary(c, b.Left))
	typesList = append(typesList, c.unaryType(b.Left))

	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, right)
		operators = append(operators, op.Op)
		posts = append(posts, op.Right)
		floats = append(floats, isFloatPostfix(c, op.Right))
		strings = append(strings, isStringPostfix(c, op.Right))
		typesList = append(typesList, c.postfixType(op.Right))
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	for _, lvl := range levels {
		for i := 0; i < len(operators); {
			if containsOp(lvl, operators[i]) {
				l := operands[i]
				r := operands[i+1]
				op := operators[i]
				var expr string
				lf := floats[i]
				rf := floats[i+1]
				if op == "/" {
					if lf || rf {
						op = "/"
						floats[i] = true
					} else {
						op = "~/"
						floats[i] = false
					}
				} else if op == "*" || op == "+" || op == "-" {
					if lf || rf {
						floats[i] = true
					} else {
						floats[i] = false
					}
				} else {
					floats[i] = false
				}
				lt := typesList[i]
				rt := typesList[i+1]
				if op == "in" {
					if isMapPostfix(c, posts[i]) {
						expr = fmt.Sprintf("(%s.containsKey(%s))", r, l)
					} else {
						expr = fmt.Sprintf("(%s.contains(%s))", r, l)
					}
				} else if op == "==" || op == "!=" {
					if isList(lt) || isList(rt) || isMap(lt) || isMap(rt) || isStruct(lt) || isStruct(rt) || isAny(lt) || isAny(rt) {
						c.use("_equal")
						if op == "==" {
							expr = fmt.Sprintf("_equal(%s, %s)", l, r)
						} else {
							expr = fmt.Sprintf("!_equal(%s, %s)", l, r)
						}
					} else {
						expr = fmt.Sprintf("(%s %s %s)", l, op, r)
					}
					typesList[i] = types.BoolType{}
				} else if (op == "<" || op == "<=" || op == ">" || op == ">=") && (strings[i] || strings[i+1]) {
					cmp := fmt.Sprintf("%s.compareTo(%s)", l, r)
					switch op {
					case "<":
						expr = fmt.Sprintf("(%s < 0)", cmp)
					case "<=":
						expr = fmt.Sprintf("(%s <= 0)", cmp)
					case ">":
						expr = fmt.Sprintf("(%s > 0)", cmp)
					case ">=":
						expr = fmt.Sprintf("(%s >= 0)", cmp)
					}
				} else if op == "union" {
					c.use("_union")
					expr = fmt.Sprintf("_union(%s, %s)", l, r)
				} else if op == "union_all" {
					c.use("_unionAll")
					expr = fmt.Sprintf("_unionAll(%s, %s)", l, r)
				} else if op == "except" {
					c.use("_except")
					expr = fmt.Sprintf("_except(%s, %s)", l, r)
				} else if op == "intersect" {
					c.use("_intersect")
					expr = fmt.Sprintf("_intersect(%s, %s)", l, r)
				} else {
					expr = fmt.Sprintf("(%s %s %s)", l, op, r)
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
				posts = append(posts[:i], posts[i+1:]...)
				floats = append(floats[:i+1], floats[i+2:]...)
				strings = append(strings[:i+1], strings[i+2:]...)
				typesList = append(typesList[:i+1], typesList[i+2:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected binary expression")
	}
	return operands[0], nil
}

func containsOp(list []string, op string) bool {
	for _, o := range list {
		if o == op {
			return true
		}
	}
	return false
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if p == nil {
		return "", nil
	}
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := "0"
				end := fmt.Sprintf("%s.length", expr)
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if isStringPrimary(c, p.Target) {
					expr = fmt.Sprintf("%s.substring(%s, %s)", expr, start, end)
				} else {
					expr = fmt.Sprintf("%s.sublist(%s, %s)", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isStringPrimary(c, p.Target) {
					c.use("_indexString")
					if !isIntExpr(c, op.Index.Start) {
						idx = fmt.Sprintf("(%s).toInt()", idx)
					}
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			}
		} else if op.Call != nil {
			call, err := c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
			expr = call
		} else if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			dt := dartType(t)
			switch tt := t.(type) {
			case types.StructType:
				expr = fmt.Sprintf("%s.fromJson(%s as Map<String,dynamic>)", sanitizeName(tt.Name), expr)
			case types.ListType:
				if st, ok := tt.Elem.(types.StructType); ok {
					tmp := c.newVar()
					expr = fmt.Sprintf("(%s as List).map((%s) => %s.fromJson(%s as Map<String,dynamic>)).toList()", expr, tmp, sanitizeName(st.Name), tmp)
				} else {
					if dt == "double" {
						expr = fmt.Sprintf("(%s).toDouble()", expr)
					} else if dt == "int" {
						expr = fmt.Sprintf("(%s).toInt()", expr)
					} else {
						expr = fmt.Sprintf("(%s as %s)", expr, dt)
					}
				}
			default:
				switch dt {
				case "double":
					expr = fmt.Sprintf("(%s).toDouble()", expr)
				case "int":
					expr = fmt.Sprintf("(%s).toInt()", expr)
				default:
					expr = fmt.Sprintf("(%s as %s)", expr, dt)
				}
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compileCallOp(receiver string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", receiver, strings.Join(args, ", ")), nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			if c.env != nil {
				if t, err := c.env.GetVar(p.Selector.Root); err == nil {
					if _, ok := t.(types.MapType); ok {
						key := sanitizeName(p.Selector.Tail[0])
						name += fmt.Sprintf("['%s']", key)
						if len(p.Selector.Tail) > 1 {
							name += "." + strings.Join(p.Selector.Tail[1:], ".")
						}
						return name, nil
					}
				}
			}
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k string
			if id, ok := identName(it.Key); ok {
				k = strconv.Quote(sanitizeName(id))
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
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "", fmt.Errorf("unsupported primary expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	// handle len()
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.length", arg), nil
	}
	// handle str()
	if name == "str" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.toString()", arg), nil
	}
	// handle count()
	if name == "count" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.use("_count")
		return fmt.Sprintf("_count(%s)", arg), nil
	}
	// handle exists()
	if name == "exists" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.use("_exists")
		return fmt.Sprintf("_exists(%s)", arg), nil
	}
	// handle avg()
	if name == "avg" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.use("_avg")
		return fmt.Sprintf("_avg(%s)", arg), nil
	}
	// handle sum()
	if name == "sum" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.use("_sum")
		return fmt.Sprintf("_sum(%s)", arg), nil
	}
	// handle min()
	if name == "min" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.use("_min")
		return fmt.Sprintf("_min(%s)", arg), nil
	}
	// handle max()
	if name == "max" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.use("_max")
		return fmt.Sprintf("_max(%s)", arg), nil
	}
	// handle abs()
	if name == "abs" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("_abs(%s)", arg), nil
	}
	// handle input()
	if name == "input" && len(call.Args) == 0 {
		c.imports["dart:io"] = true
		return "stdin.readLineSync() ?? ''", nil
	}
	// handle now()
	if name == "now" && len(call.Args) == 0 {
		return "DateTime.now().microsecondsSinceEpoch * 1000", nil
	}
	// handle json()
	if name == "json" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.imports["dart:convert"] = true
		c.use("_json")
		return fmt.Sprintf("_json(%s)", arg), nil
	}
	// handle print with multiple arguments
	if name == "print" && len(call.Args) > 1 {
		parts := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s.toString()", v)
		}
		return fmt.Sprintf("print([%s].join(' '))", strings.Join(parts, ", ")), nil
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLiteral(lit *parser.Literal) (string, error) {
	switch {
	case lit.Int != nil:
		return strconv.Itoa(*lit.Int), nil
	case lit.Float != nil:
		return strconv.FormatFloat(*lit.Float, 'f', -1, 64), nil
	case lit.Str != nil:
		// Use Go's quoting to handle escape sequences then protect
		// `$` from Dart interpolation.
		s := strings.ReplaceAll(*lit.Str, "$", "\\$")
		return strconv.Quote(s), nil
	case lit.Bool != nil:
		if *lit.Bool {
			return "true", nil
		}
		return "false", nil
	case lit.Null:
		return "null", nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)

	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = c.resolveTypeRef(p.Type)
			} else {
				ft.Params[i] = types.AnyType{}
			}
		}
	}
	if ft.Return == nil {
		if fun.Return != nil {
			ft.Return = c.resolveTypeRef(fun.Return)
		} else {
			ft.Return = c.funcReturnType(fun.Body)
		}
	}

	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		typStr := dartType(ft.Params[i])
		if typStr != "dynamic" && typStr != "" {
			params[i] = fmt.Sprintf("%s %s", typStr, sanitizeName(p.Name))
		} else {
			params[i] = sanitizeName(p.Name)
		}
	}

	ret := "dynamic"
	if rt := dartType(ft.Return); rt != "dynamic" && rt != "" {
		ret = rt
	}

	c.writeln(fmt.Sprintf("%s %s(%s) {", ret, name, strings.Join(params, ", ")))
	c.indent++
	origEnv := c.env
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		child.SetVar(p.Name, ft.Params[i], true)
	}
	c.env = child
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.env = origEnv
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)

	var ft types.FuncType
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			if m, ok := st.Methods[fun.Name]; ok {
				ft = m.Type
			}
		}
	}
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = c.resolveTypeRef(p.Type)
			} else {
				ft.Params[i] = types.AnyType{}
			}
		}
	}
	if ft.Return == nil {
		if fun.Return != nil {
			ft.Return = c.resolveTypeRef(fun.Return)
		} else {
			ft.Return = c.funcReturnType(fun.Body)
		}
	}

	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		typStr := dartType(ft.Params[i])
		if typStr != "dynamic" && typStr != "" {
			params[i] = fmt.Sprintf("%s %s", typStr, sanitizeName(p.Name))
		} else {
			params[i] = sanitizeName(p.Name)
		}
	}

	ret := "dynamic"
	if rt := dartType(ft.Return); rt != "dynamic" && rt != "" {
		ret = rt
	}

	c.writeln(fmt.Sprintf("%s %s(%s) {", ret, name, strings.Join(params, ", ")))
	c.indent++
	origEnv := c.env
	child := types.NewEnv(c.env)
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			for fname, t := range st.Fields {
				child.SetVar(fname, t, true)
			}
		}
	}
	for i, p := range fun.Params {
		child.SetVar(p.Name, ft.Params[i], true)
	}
	c.env = child
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.env = origEnv
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	fields := []string{}
	for _, fn := range st.Order {
		typ := dartType(st.Fields[fn])
		if typ == "" {
			typ = "dynamic"
		}
		fname := sanitizeName(fn)
		c.writeln(fmt.Sprintf("%s %s;", typ, fname))
		param := "this." + fname
		if typ != "dynamic" {
			param = "required " + param
		}
		fields = append(fields, param)
	}
	var ctor string
	if len(fields) == 0 {
		ctor = fmt.Sprintf("%s();", name)
	} else {
		ctor = fmt.Sprintf("%s({%s});", name, strings.Join(fields, ", "))
	}
	c.writeln(ctor)
	// fromJson factory
	c.writeln(fmt.Sprintf("factory %s.fromJson(Map<String,dynamic> m) {", name))
	c.indent++
	assigns := make([]string, len(st.Order))
	for i, fn := range st.Order {
		typ := dartType(st.Fields[fn])
		if typ == "" {
			typ = "dynamic"
		}
		assigns[i] = fmt.Sprintf("%s: m['%s'] as %s", sanitizeName(fn), sanitizeName(fn), typ)
	}
	c.writeln(fmt.Sprintf("return %s(%s);", name, strings.Join(assigns, ", ")))
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	c.structRegs = append(c.structRegs, fmt.Sprintf("_structParsers['%s'] = (m) => %s.fromJson(m);", name, name))
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("void %s() {", name))
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

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (!(%s)) { throw Exception('expect failed'); }", expr))
	return nil
}

func (c *Compiler) compileModelDecl(m *parser.ModelDecl) error {
	c.models = true
	parts := make([]string, len(m.Fields))
	for i, f := range m.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%q: %s", f.Name, v)
	}
	c.writeln(fmt.Sprintf("_models[%q] = {%s};", m.Name, strings.Join(parts, ", ")))
	return nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), expr), nil
	}
	sub := &Compiler{env: types.NewEnv(c.env), imports: c.imports, dartImports: c.dartImports}
	for _, p := range fn.Params {
		sub.env.SetVar(p.Name, types.AnyType{}, true)
	}
	for _, s := range fn.BlockBody {
		if err := sub.compileStmt(s); err != nil {
			return "", err
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return fmt.Sprintf("(%s) {\n%s}", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileAgentDecl(a *parser.AgentDecl) error {
	st, ok := c.env.GetStruct(a.Name)
	if !ok {
		return fmt.Errorf("unknown agent: %s", a.Name)
	}
	name := sanitizeName(a.Name)
	if c.agents[name] {
		return nil
	}
	c.agents[name] = true

	baseEnv := types.NewEnv(c.env)
	for _, fn := range st.Order {
		baseEnv.SetVar(fn, st.Fields[fn], true)
	}

	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	c.writeln("_Agent Agent;")
	for _, fn := range st.Order {
		typ := dartType(st.Fields[fn])
		if typ == "" {
			typ = "dynamic"
		}
		c.writeln(fmt.Sprintf("%s %s;", typ, sanitizeName(fn)))
	}
	c.writeln(fmt.Sprintf("%s() {", name))
	c.indent++
	c.use("_Agent")
	c.writeln(fmt.Sprintf("Agent = _Agent('%s');", a.Name))
	orig := c.env
	c.env = baseEnv
	for _, blk := range a.Body {
		switch {
		case blk.Let != nil:
			val := "null"
			if blk.Let.Value != nil {
				v, err := c.compileExpr(blk.Let.Value)
				if err != nil {
					c.env = orig
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(blk.Let.Name), val))
		case blk.Var != nil:
			val := "null"
			if blk.Var.Value != nil {
				v, err := c.compileExpr(blk.Var.Value)
				if err != nil {
					c.env = orig
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(blk.Var.Name), val))
		}
	}
	c.env = orig
	handlerID := 0
	for _, blk := range a.Body {
		if blk.On != nil {
			streamVar := "_" + sanitizeName(blk.On.Stream) + "Stream"
			c.writeln(fmt.Sprintf("Agent.on(%s, _on%d);", streamVar, handlerID))
			handlerID++
		}
	}
	for _, blk := range a.Body {
		if blk.Intent != nil {
			mname := sanitizeName(blk.Intent.Name)
			c.writeln(fmt.Sprintf("Agent.registerIntent('%s', %s);", blk.Intent.Name, mname))
		}
	}
	c.writeln("Agent.start();")
	c.indent--
	c.writeln("}")

	handlerID = 0
	for _, blk := range a.Body {
		switch {
		case blk.Intent != nil:
			if err := c.compileAgentIntent(name, baseEnv, blk.Intent); err != nil {
				return err
			}
		case blk.On != nil:
			if _, err := c.compileAgentOn(name, baseEnv, blk.On, handlerID); err != nil {
				return err
			}
			handlerID++
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.writeln(fmt.Sprintf("%s New%s() {", name, name))
	c.indent++
	c.writeln(fmt.Sprintf("return %s();", name))
	c.indent--
	c.writeln("}")
	c.writeln("")
	return nil
}

func (c *Compiler) compileAgentIntent(agentName string, env *types.Env, in *parser.IntentDecl) error {
	name := sanitizeName(in.Name)
	c.writeln(fmt.Sprintf("dynamic %s(%s) {", name, strings.Join(paramNames(in.Params), ", ")))
	child := types.NewEnv(env)
	orig := c.env
	c.env = child
	c.indent++
	for _, s := range in.Body {
		if err := c.compileStmt(s); err != nil {
			c.env = orig
			return err
		}
	}
	c.indent--
	c.env = orig
	c.writeln("}")
	c.writeln("")
	return nil
}

func paramNames(params []*parser.Param) []string {
	names := make([]string, len(params))
	for i, p := range params {
		names[i] = sanitizeName(p.Name)
	}
	return names
}

func (c *Compiler) compileAgentOn(agentName string, env *types.Env, h *parser.OnHandler, id int) (string, error) {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return "", fmt.Errorf("unknown stream: %s", h.Stream)
	}
	fname := fmt.Sprintf("_on%d", id)
	c.writeln(fmt.Sprintf("void %s(%s ev) {", fname, sanitizeName(st.Name)))
	alias := sanitizeName(h.Alias)
	child := types.NewEnv(env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	c.indent++
	c.writeln(fmt.Sprintf("var %s = ev;", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return "", err
		}
	}
	c.indent--
	c.env = orig
	c.writeln("}")
	c.writeln("")
	return fname, nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if c.structs == nil {
		c.structs = map[string]bool{}
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("abstract class %s {}", name))
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			c.writeln(fmt.Sprintf("class %s extends %s {", vname, name))
			c.indent++
			fields := []string{}
			for _, f := range v.Fields {
				fname := sanitizeName(f.Name)
				typ := dartType(c.resolveTypeRef(f.Type))
				if typ == "" {
					typ = "dynamic"
				}
				c.writeln(fmt.Sprintf("%s %s;", typ, fname))
				param := "this." + fname
				if typ != "dynamic" {
					param = "required " + param
				}
				fields = append(fields, param)
			}
			var ctor string
			if len(fields) == 0 {
				ctor = fmt.Sprintf("%s();", vname)
			} else {
				ctor = fmt.Sprintf("%s({%s});", vname, strings.Join(fields, ", "))
			}
			c.writeln(ctor)
			c.indent--
			c.writeln("}")
		}
		return nil
	}
	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	fields := []string{}
	names := []string{}
	ftypes := []string{}
	var methods []*parser.FunStmt
	for _, m := range t.Members {
		if m.Field != nil {
			fname := sanitizeName(m.Field.Name)
			typ := dartType(c.resolveTypeRef(m.Field.Type))
			if typ == "" {
				typ = "dynamic"
			}
			c.writeln(fmt.Sprintf("%s %s;", typ, fname))
			param := "this." + fname
			if typ != "dynamic" {
				param = "required " + param
			}
			fields = append(fields, param)
			names = append(names, fname)
			ftypes = append(ftypes, typ)
		} else if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	var ctor string
	if len(fields) == 0 {
		ctor = fmt.Sprintf("%s();", name)
	} else {
		ctor = fmt.Sprintf("%s({%s});", name, strings.Join(fields, ", "))
	}
	c.writeln(ctor)
	c.writeln(fmt.Sprintf("factory %s.fromJson(Map<String,dynamic> m) {", name))
	c.indent++
	assigns := make([]string, len(names))
	for i, fn := range names {
		typ := ftypes[i]
		assigns[i] = fmt.Sprintf("%s: m['%s'] as %s", fn, fn, typ)
	}
	c.writeln(fmt.Sprintf("return %s(%s);", name, strings.Join(assigns, ", ")))
	c.indent--
	c.writeln("}")
	for _, m := range methods {
		if err := c.compileMethod(name, m); err != nil {
			return err
		}
		c.writeln("")
	}
	c.indent--
	c.writeln("}")
	c.structRegs = append(c.structRegs, fmt.Sprintf("_structParsers['%s'] = (m) => %s.fromJson(m);", name, name))
	return nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	// Setup a child environment so field access inside the query can be
	// type-aware. Without this, map fields are accessed using dot syntax
	// which fails to compile.
	origEnv := c.env
	child := types.NewEnv(c.env)
	var elemType types.Type = types.AnyType{}
	if lt, ok := c.exprType(q.Source).(types.ListType); ok {
		elemType = lt.Elem
	}
	child.SetVar(q.Var, elemType, true)
	c.env = child
	defer func() { c.env = origEnv }()

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		fromSrcs[i] = fs
		var fe types.Type = types.AnyType{}
		if lt, ok := c.exprType(f.Src).(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}

	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	needsHelper := false
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		joinSrcs[i] = js
		var je types.Type = types.AnyType{}
		if lt, ok := c.exprType(j.Src).(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
	}
	c.env = child
	for i, j := range q.Joins {
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
			needsHelper = true
		}
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var where string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	pushdown := q.Where != nil && exprUsesOnlyVar(q.Where, q.Var)
	var sortExpr, skipExpr, takeExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	distinct := q.Distinct
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}

	v := sanitizeName(q.Var)
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		c.use("_Group")
		c.use("_group_by")
		expr := fmt.Sprintf("_group_by(%s, (%s) => %s).map((%s) => %s).toList()", src, v, keyExpr, sanitizeName(q.Group.Name), valExpr)
		if distinct {
			c.use("_distinct")
			expr = fmt.Sprintf("(() { var _t = %s; return _distinct(_t); })()", expr)
		}
		return expr, nil
	}

	if q.Group != nil && needsHelper {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		oldEnv := c.env
		genv := types.NewEnv(c.env)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		c.env = oldEnv
		if err != nil {
			return "", err
		}
		varNames := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			varNames = append(varNames, sanitizeName(f.Var))
		}
		params := append([]string(nil), varNames...)
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{'items': %s}", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onFn := fmt.Sprintf("(%s) => %s", strings.Join(onParams, ", "), joinOns[i])
			spec := fmt.Sprintf("{'items': %s, 'on': %s", js, onFn)
			if joinSides[i] == "left" || joinSides[i] == "outer" {
				spec += ", 'left': true"
			}
			if joinSides[i] == "right" || joinSides[i] == "outer" {
				spec += ", 'right': true"
			}
			spec += "}"
			joins = append(joins, spec)
			params = append(params, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(params, ", ")
		selectFn := fmt.Sprintf("(%s) => [%s]", allParams, allParams)
		var whereFn string
		if where != "" && !pushdown {
			whereFn = fmt.Sprintf("(%s) => %s", allParams, where)
		}
		c.use("_query")
		c.use("_Group")
		var buf strings.Builder
		buf.WriteString("(() {\n")
		if pushdown {
			buf.WriteString(fmt.Sprintf("\tvar src = (%s).where((%s) => %s).toList();\n", src, v, where))
			whereFn = ""
		} else {
			buf.WriteString(fmt.Sprintf("\tvar src = %s;\n", src))
		}
		buf.WriteString("\tvar items = _query(src, [\n")
		for _, j := range joins {
			buf.WriteString("\t\t" + j + ",\n")
		}
		buf.WriteString("\t], { 'select': " + selectFn)
		if whereFn != "" {
			buf.WriteString(", 'where': " + whereFn)
		}
		if distinct {
			buf.WriteString(", 'distinct': true")
			c.use("_distinct")
		}
		buf.WriteString(" });\n")
		buf.WriteString("\tvar groups = <String,_Group>{};\n")
		buf.WriteString("\tvar order = <String>[];\n")
		buf.WriteString("\tfor (var _r in items) {\n")
		for i, p := range params {
			buf.WriteString(fmt.Sprintf("\t\tvar %s = _r[%d];\n", p, i))
		}
		buf.WriteString(fmt.Sprintf("\t\tvar key = %s;\n", keyExpr))
		buf.WriteString("\t\tvar ks = key.toString();\n")
		buf.WriteString("\t\tvar g = groups[ks];\n")
		buf.WriteString("\t\tif (g == null) {\n")
		buf.WriteString("\t\t\tg = _Group(key);\n")
		buf.WriteString("\t\t\tgroups[ks] = g;\n")
		buf.WriteString("\t\t\torder.add(ks);\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString(fmt.Sprintf("\t\tg.Items.add(%s);\n", v))
		buf.WriteString("\t}\n")
		buf.WriteString("\tvar itemsG = [for (var k in order) groups[k]!];\n")
		if sortExpr != "" {
			name := sanitizeName(q.Group.Name)
			buf.WriteString(fmt.Sprintf("\titemsG.sort((%sA, %sB) {\n", name, name))
			buf.WriteString(fmt.Sprintf("\t\tvar %s = %sA;\n", name, name))
			buf.WriteString(fmt.Sprintf("\t\tvar keyA = %s;\n", sortExpr))
			buf.WriteString(fmt.Sprintf("\t\t%s = %sB;\n", name, name))
			buf.WriteString(fmt.Sprintf("\t\tvar keyB = %s;\n", sortExpr))
			buf.WriteString("\t\treturn Comparable.compare(keyA, keyB);\n")
			buf.WriteString("\t});\n")
		}
		if skipExpr != "" {
			buf.WriteString(fmt.Sprintf("\tvar skip = %s;\n", skipExpr))
			buf.WriteString("\tif (skip < itemsG.length) {\n")
			buf.WriteString("\t\titemsG = itemsG.sublist(skip);\n")
			buf.WriteString("\t} else {\n")
			buf.WriteString("\t\titemsG = [];\n")
			buf.WriteString("\t}\n")
		}
		if takeExpr != "" {
			buf.WriteString(fmt.Sprintf("\tvar take = %s;\n", takeExpr))
			buf.WriteString("\tif (take < itemsG.length) {\n")
			buf.WriteString("\t\titemsG = itemsG.sublist(0, take);\n")
			buf.WriteString("\t}\n")
		}
		buf.WriteString("\tvar _res = [];\n")
		buf.WriteString(fmt.Sprintf("\tfor (var %s in itemsG) {\n", sanitizeName(q.Group.Name)))
		buf.WriteString(fmt.Sprintf("\t\t_res.add(%s);\n", valExpr))
		buf.WriteString("\t}\n")
		if distinct {
			c.use("_distinct")
			buf.WriteString("\t_res = _distinct(_res);\n")
		}
		buf.WriteString("\treturn _res;\n")
		buf.WriteString("})()")
		return buf.String(), nil
	}

	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		oldEnv := c.env
		genv := types.NewEnv(c.env)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		c.env = oldEnv
		if err != nil {
			return "", err
		}
		c.use("_Group")
		var buf strings.Builder
		buf.WriteString("(() {\n")
		buf.WriteString("\tvar groups = <String,_Group>{};\n")
		buf.WriteString("\tvar order = <String>[];\n")
		buf.WriteString(fmt.Sprintf("\tfor (var %s in %s) {\n", v, src))
		indent := "\t\t"
		for i, fs := range fromSrcs {
			buf.WriteString(fmt.Sprintf(indent+"for (var %s in %s) {\n", sanitizeName(q.Froms[i].Var), fs))
			indent += "\t"
		}
		for i, js := range joinSrcs {
			buf.WriteString(fmt.Sprintf(indent+"for (var %s in %s) {\n", sanitizeName(q.Joins[i].Var), js))
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"if (!(%s)) {\n", joinOns[i]))
			buf.WriteString(indent + "\tcontinue;\n")
			buf.WriteString(indent + "}\n")
		}
		if where != "" && !pushdown {
			buf.WriteString(indent + "if (!(" + where + ")) {\n")
			buf.WriteString(indent + "\tcontinue;\n")
			buf.WriteString(indent + "}\n")
		}
		buf.WriteString(fmt.Sprintf(indent+"var key = %s;\n", keyExpr))
		buf.WriteString(indent + "var ks = key.toString();\n")
		buf.WriteString(indent + "var g = groups[ks];\n")
		buf.WriteString(indent + "if (g == null) {\n")
		buf.WriteString(indent + "\tg = _Group(key);\n")
		buf.WriteString(indent + "\tgroups[ks] = g;\n")
		buf.WriteString(indent + "\torder.add(ks);\n")
		buf.WriteString(indent + "}\n")
		buf.WriteString(fmt.Sprintf(indent+"g.Items.add(%s);\n", v))
		for i := len(joinSrcs) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
		for i := len(fromSrcs) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
		buf.WriteString("\t}\n")
		buf.WriteString("\tvar items = [for (var k in order) groups[k]!];\n")
		if sortExpr != "" {
			name := sanitizeName(q.Group.Name)
			buf.WriteString(fmt.Sprintf("\titems.sort((%sA, %sB) {\n", name, name))
			buf.WriteString(fmt.Sprintf("\t\tvar %s = %sA;\n", name, name))
			buf.WriteString(fmt.Sprintf("\t\tvar keyA = %s;\n", sortExpr))
			buf.WriteString(fmt.Sprintf("\t\t%s = %sB;\n", name, name))
			buf.WriteString(fmt.Sprintf("\t\tvar keyB = %s;\n", sortExpr))
			buf.WriteString("\t\treturn Comparable.compare(keyA, keyB);\n")
			buf.WriteString("\t});\n")
		}
		if skipExpr != "" {
			buf.WriteString(fmt.Sprintf("\tvar skip = %s;\n", skipExpr))
			buf.WriteString("\tif (skip < items.length) {\n")
			buf.WriteString("\t\titems = items.sublist(skip);\n")
			buf.WriteString("\t} else {\n")
			buf.WriteString("\t\titems = [];\n")
			buf.WriteString("\t}\n")
		}
		if takeExpr != "" {
			buf.WriteString(fmt.Sprintf("\tvar take = %s;\n", takeExpr))
			buf.WriteString("\tif (take < items.length) {\n")
			buf.WriteString("\t\titems = items.sublist(0, take);\n")
			buf.WriteString("\t}\n")
		}
		buf.WriteString("\tvar _res = [];\n")
		buf.WriteString(fmt.Sprintf("\tfor (var %s in items) {\n", sanitizeName(q.Group.Name)))
		buf.WriteString(fmt.Sprintf("\t\t_res.add(%s);\n", valExpr))
		buf.WriteString("\t}\n")
		if distinct {
			c.use("_distinct")
			buf.WriteString("\t_res = _distinct(_res);\n")
		}
		buf.WriteString("\treturn _res;\n")
		buf.WriteString("})()")
		return buf.String(), nil
	}

	if needsHelper {
		varNames := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			varNames = append(varNames, sanitizeName(f.Var))
		}
		params := append([]string(nil), varNames...)
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{'items': %s}", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onFn := fmt.Sprintf("(%s) => %s", strings.Join(onParams, ", "), joinOns[i])
			spec := fmt.Sprintf("{'items': %s, 'on': %s", js, onFn)
			if joinSides[i] == "left" || joinSides[i] == "outer" {
				spec += ", 'left': true"
			}
			if joinSides[i] == "right" || joinSides[i] == "outer" {
				spec += ", 'right': true"
			}
			spec += "}"
			joins = append(joins, spec)
			params = append(params, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(params, ", ")
		selectFn := fmt.Sprintf("(%s) => %s", allParams, sel)
		var whereFn, sortFn string
		if where != "" && !pushdown {
			whereFn = fmt.Sprintf("(%s) => %s", allParams, where)
		}
		if sortExpr != "" {
			sortFn = fmt.Sprintf("(%s) => %s", allParams, sortExpr)
		}
		c.use("_query")
		var buf strings.Builder
		buf.WriteString("(() {\n")
		if pushdown {
			buf.WriteString(fmt.Sprintf("\tvar src = (%s).where((%s) => %s).toList();\n", src, v, where))
			whereFn = ""
		} else {
			buf.WriteString(fmt.Sprintf("\tvar src = %s;\n", src))
		}
		buf.WriteString("\tvar res = _query(src, [\n")
		for _, j := range joins {
			buf.WriteString("\t\t" + j + ",\n")
		}
		buf.WriteString("\t], { 'select': " + selectFn)
		if whereFn != "" {
			buf.WriteString(", 'where': " + whereFn)
		}
		if sortFn != "" {
			buf.WriteString(", 'sortKey': " + sortFn)
		}
		if skipExpr != "" {
			buf.WriteString(", 'skip': " + skipExpr)
		}
		if takeExpr != "" {
			buf.WriteString(", 'take': " + takeExpr)
		}
		if distinct {
			buf.WriteString(", 'distinct': true")
			c.use("_distinct")
		}
		buf.WriteString(" });\n")
		buf.WriteString("\treturn res;\n")
		buf.WriteString("})()")
		return buf.String(), nil
	}

	var b strings.Builder
	b.WriteString("(() {\n")
	b.WriteString("\tvar _res = [];\n")
	if pushdown {
		b.WriteString(fmt.Sprintf("\tvar _src = (%s).where((%s) => %s).toList();\n", src, sanitizeName(q.Var), where))
		b.WriteString(fmt.Sprintf("\tfor (var %s in _src) {\n", sanitizeName(q.Var)))
	} else {
		b.WriteString(fmt.Sprintf("\tfor (var %s in %s) {\n", sanitizeName(q.Var), src))
	}
	indent := "\t\t"
	for i, fs := range fromSrcs {
		b.WriteString(fmt.Sprintf(indent+"for (var %s in %s) {\n", sanitizeName(q.Froms[i].Var), fs))
		indent += "\t"
	}
	for i, js := range joinSrcs {
		b.WriteString(fmt.Sprintf(indent+"for (var %s in %s) {\n", sanitizeName(q.Joins[i].Var), js))
		indent += "\t"
		b.WriteString(fmt.Sprintf(indent+"if (!(%s)) {\n", joinOns[i]))
		b.WriteString(indent + "\tcontinue;\n")
		b.WriteString(indent + "}\n")
	}
	if where != "" && !pushdown {
		b.WriteString(indent + "if (!(" + where + ")) {\n")
		b.WriteString(indent + "\tcontinue;\n")
		b.WriteString(indent + "}\n")
	}
	b.WriteString(fmt.Sprintf(indent+"_res.add(%s);\n", sel))
	for range joinSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range fromSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	if sortExpr != "" || skipExpr != "" || takeExpr != "" {
		b.WriteString("\tvar items = List.from(_res);\n")
		if sortExpr != "" {
			v := sanitizeName(q.Var)
			b.WriteString(fmt.Sprintf("\titems.sort((%sA, %sB) {\n", v, v))
			b.WriteString(fmt.Sprintf("\t\tvar %s = %sA;\n", v, v))
			b.WriteString(fmt.Sprintf("\t\tvar keyA = %s;\n", sortExpr))
			b.WriteString(fmt.Sprintf("\t\t%s = %sB;\n", v, v))
			b.WriteString(fmt.Sprintf("\t\tvar keyB = %s;\n", sortExpr))
			b.WriteString("\t\treturn Comparable.compare(keyA, keyB);\n")
			b.WriteString("\t});\n")
		}
		if skipExpr != "" {
			b.WriteString(fmt.Sprintf("\tvar skip = %s;\n", skipExpr))
			b.WriteString("\tif (skip < items.length) {\n")
			b.WriteString("\t\titems = items.sublist(skip);\n")
			b.WriteString("\t} else {\n")
			b.WriteString("\t\titems = [];\n")
			b.WriteString("\t}\n")
		}
		if takeExpr != "" {
			b.WriteString(fmt.Sprintf("\tvar take = %s;\n", takeExpr))
			b.WriteString("\tif (take < items.length) {\n")
			b.WriteString("\t\titems = items.sublist(0, take);\n")
			b.WriteString("\t}\n")
		}
		b.WriteString("\t_res = items;\n")
	}
	if distinct {
		c.use("_distinct")
		b.WriteString("\t_res = _distinct(_res);\n")
	}
	b.WriteString("\treturn _res;\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(() {\n")
	b.WriteString("\tvar _t = " + target + ";\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + ";\n")
			b.WriteString("})()")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("_t is %s", sanitizeName(call.Func))
				names := []string{}
				vals := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						vals = append(vals, fmt.Sprintf("(_t as %s).%s", sanitizeName(call.Func), field))
					}
				}
				if len(names) > 0 {
					res = fmt.Sprintf("((%s) { return %s; })(%s)", strings.Join(names, ", "), res, strings.Join(vals, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("_t is %s", sanitizeName(ident))
			}
		}
		if cond == "" {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("_t == %s", pat)
		}
		b.WriteString(fmt.Sprintf("\tif (%s) { return %s; }\n", cond, res))
	}
	b.WriteString("\treturn null;\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var (
		prompt string
		text   string
		model  string
		params []string
	)
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("%q: %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "null"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("{ %s }", strings.Join(params, ", "))
	}
	if model == "" {
		model = "\"\""
	}
	c.imports["dart:convert"] = true
	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramStr), nil
	}
	if _, ok := c.env.GetStruct(g.Target); ok {
		c.use("_genStruct")
		return fmt.Sprintf("_genStruct<%s>(%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
	}
	c.use("_genText")
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "null"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = w
	}
	c.imports["dart:io"] = true
	c.imports["dart:convert"] = true
	c.use("_fetch")
	return fmt.Sprintf("await _fetch(%s, %s)", urlStr, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "null"
	if l.With != nil {
		w, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = w
	}
	c.imports["dart:io"] = true
	c.imports["dart:convert"] = true
	c.imports["package:yaml/yaml.dart"] = true
	c.use("_load")
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "null"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "null"
	if s.With != nil {
		w, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = w
	}
	c.imports["dart:io"] = true
	c.imports["dart:convert"] = true
	c.imports["package:yaml/yaml.dart"] = true
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compilePackageImport(im *parser.ImportStmt) error {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	if c.packages[alias] {
		return nil
	}
	c.packages[alias] = true

	path := strings.Trim(im.Path, "\"")
	base := ""
	if strings.HasPrefix(path, "./") || strings.HasPrefix(path, "../") {
		base = filepath.Dir(im.Pos.Filename)
	}
	target := filepath.Join(base, path)
	info, err := os.Stat(target)
	if err != nil {
		if os.IsNotExist(err) && !strings.HasSuffix(target, ".mochi") {
			if fi, err2 := os.Stat(target + ".mochi"); err2 == nil {
				info = fi
				target += ".mochi"
			} else {
				return fmt.Errorf("import package: %w", err)
			}
		} else {
			return fmt.Errorf("import package: %w", err)
		}
	}
	var files []string
	if info.IsDir() {
		entries, err := os.ReadDir(target)
		if err != nil {
			return fmt.Errorf("import package: %w", err)
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(target, e.Name()))
			}
		}
		sort.Strings(files)
	} else {
		files = []string{target}
	}

	pkgEnv := types.NewEnv(c.env)
	origEnv := c.env
	c.env = pkgEnv
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			c.env = origEnv
			return err
		}
		if errs := types.Check(prog, pkgEnv); len(errs) > 0 {
			c.env = origEnv
			return errs[0]
		}
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				name := s.Fun.Name
				s.Fun.Name = alias + "_" + name
				if err := c.compileFun(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
	}
	c.env = origEnv
	return nil
}

func (c *Compiler) compileDartImport(im *parser.ImportStmt) error {
	alias := ""
	if im.As != "" {
		alias = sanitizeName(im.As)
	}
	path := strings.Trim(im.Path, "\"")
	key := alias
	if key == "" {
		key = path
	}
	if existing, ok := c.dartImports[key]; ok {
		if existing == path {
			return nil
		}
	}
	c.dartImports[key] = path
	return nil
}

func isMapExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			if u.Value.Target.Map != nil {
				return true
			}
			if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
				if c.env != nil {
					if t, err := c.env.GetVar(sel.Root); err == nil {
						if _, ok := t.(types.MapType); ok {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

func isStringExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			if lit := u.Value.Target.Lit; lit != nil && lit.Str != nil {
				return true
			}
			if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
				if c.env != nil {
					if t, err := c.env.GetVar(sel.Root); err == nil {
						if _, ok := t.(types.StringType); ok {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

func isMapPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}
	return isMapExpr(c, e)
}

func isStringPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}
	return isStringExpr(c, e)
}

func isFloatExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		return isFloatUnary(c, e.Binary.Left)
	}
	return false
}

func isFloatUnary(c *Compiler, u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isFloatPostfix(c, u.Value)
}

func isStringUnary(c *Compiler, u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isStringPostfix(c, u.Value)
}

func isFloatPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if last := p.Ops[len(p.Ops)-1]; last.Cast != nil {
			t := c.resolveTypeRef(last.Cast.Type)
			if isFloat(t) {
				return true
			}
		}
	}
	return isFloatPrimary(c, p.Target)
}

func isFloatPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if lit := p.Lit; lit != nil && lit.Float != nil {
		return true
	}
	if sel := p.Selector; sel != nil && len(sel.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(sel.Root); err == nil {
				if isFloat(t) {
					return true
				}
			}
		}
	}
	return false
}

func isIntExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		return isIntUnary(c, e.Binary.Left)
	}
	return false
}

func isIntUnary(c *Compiler, u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isIntPostfix(c, u.Value)
}

func isIntPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if last := p.Ops[len(p.Ops)-1]; last.Cast != nil {
			t := c.resolveTypeRef(last.Cast.Type)
			if isInt(t) || isInt64(t) {
				return true
			}
		}
	}
	return isIntPrimary(c, p.Target)
}

func isIntPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if lit := p.Lit; lit != nil && lit.Int != nil {
		return true
	}
	if sel := p.Selector; sel != nil && len(sel.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(sel.Root); err == nil {
				if isInt(t) || isInt64(t) {
					return true
				}
			}
		}
	}
	return false
}

func isStringPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: p}}}}
	return isStringExpr(c, e)
}

func hasTest(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func hasExpect(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if containsExpect(s) {
			return true
		}
	}
	return false
}

func containsExpect(s *parser.Statement) bool {
	switch {
	case s.Expect != nil:
		return true
	case s.If != nil:
		for _, t := range s.If.Then {
			if containsExpect(t) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if containsExpect(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if containsExpect(t) {
				return true
			}
		}
	case s.For != nil:
		for _, t := range s.For.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.While != nil:
		for _, t := range s.While.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if containsExpect(t) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	c.writeln("")
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
		c.buf.WriteByte('\n')
	}
}
