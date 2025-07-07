//go:build archived

package ktcode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Kotlin source code.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	mainStmts     []*parser.Statement
	helpers       map[string]bool
	packages      map[string]bool
	structs       map[string]bool
	models        bool
	handlerCount  int
	updateTargets map[string]bool
}

// New creates a new Kotlin compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), packages: make(map[string]bool), structs: make(map[string]bool), models: false, handlerCount: 0, updateTargets: make(map[string]bool)}
}

// Compile generates Kotlin code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if prog.Package != "" {
		c.writeln("package " + sanitizeName(prog.Package))
		c.writeln("")
	}

	for _, s := range prog.Statements {
		if s.Update != nil {
			c.updateTargets[s.Update.Target] = true
		}
	}

	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang == nil {
			if err := c.compilePackageImport(s.Import); err != nil {
				return nil, err
			}
		}
	}

	for _, s := range prog.Statements {
		if s.Model != nil {
			c.models = true
			break
		}
	}
	if c.models {
		c.writeln("var _models = mutableMapOf<String, Map<String, Any>>()")
		c.writeln("")
	}

	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.ExternVar != nil {
			if err := c.compileExternVar(s.ExternVar); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.ExternFun != nil {
			if err := c.compileExternFun(s.ExternFun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.ExternType != nil {
			if err := c.compileExternType(s.ExternType); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.ExternObject != nil {
			if err := c.compileExternObject(s.ExternObject); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		c.mainStmts = append(c.mainStmts, s)
	}
	c.writeln("fun main() {")
	c.indent++
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s()", name))
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.emitRuntime()
	c.writeln("")
	out := c.buf.Bytes()
	return FormatKotlin(out), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Model != nil:
		return c.compileModelDecl(s.Model)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Import != nil:
		if s.Import.Lang == nil {
			return c.compilePackageImport(s.Import)
		}
		return fmt.Errorf("foreign imports not supported")
	case s.ExternVar != nil:
		return c.compileExternVar(s.ExternVar)
	case s.ExternFun != nil:
		return c.compileExternFun(s.ExternFun)
	case s.ExternType != nil:
		return c.compileExternType(s.ExternType)
	case s.ExternObject != nil:
		return c.compileExternObject(s.ExternObject)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(stmt *parser.LetStmt) error {
	expr, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	if stmt.Type != nil {
		t := c.resolveTypeRef(stmt.Type)
		typ := ktType(t)
		if c.updateTargets[stmt.Name] {
			if lt, ok := t.(types.ListType); ok {
				typ = "MutableList<" + ktType(lt.Elem) + ">"
				expr += ".toMutableList()"
			}
		}
		if isFetchExpr(stmt.Value) {
			c.use("_cast")
			expr = fmt.Sprintf("_cast<%s>(%s)", typ, expr)
		}
		c.writeln(fmt.Sprintf("val %s: %s = %s", sanitizeName(stmt.Name), typ, expr))
		c.env.SetVar(stmt.Name, t, false)
	} else {
		if c.updateTargets[stmt.Name] {
			expr += ".toMutableList()"
		}
		c.writeln(fmt.Sprintf("val %s = %s", sanitizeName(stmt.Name), expr))
	}
	return nil
}

func (c *Compiler) compileVar(stmt *parser.VarStmt) error {
	value := "null"
	if stmt.Value != nil {
		v, err := c.compileExpr(stmt.Value)
		if err != nil {
			return err
		}
		if stmt.Type != nil && isFetchExpr(stmt.Value) {
			typ := ktType(c.resolveTypeRef(stmt.Type))
			c.use("_cast")
			v = fmt.Sprintf("_cast<%s>(%s)", typ, v)
		}
		value = v
	}
	if stmt.Type != nil {
		t := c.resolveTypeRef(stmt.Type)
		typ := ktType(t)
		if c.updateTargets[stmt.Name] {
			if lt, ok := t.(types.ListType); ok {
				typ = "MutableList<" + ktType(lt.Elem) + ">"
				value += ".toMutableList()"
			}
		}
		c.writeln(fmt.Sprintf("var %s: %s = %s", sanitizeName(stmt.Name), typ, value))
		c.env.SetVar(stmt.Name, t, true)
	} else {
		if c.updateTargets[stmt.Name] {
			value += ".toMutableList()"
		}
		c.writeln(fmt.Sprintf("var %s = %s", sanitizeName(stmt.Name), value))
	}
	return nil
}

func (c *Compiler) compileAssign(stmt *parser.AssignStmt) error {
	lhs := sanitizeName(stmt.Name)
	idxStrs := make([]string, 0, len(stmt.Index))
	for _, idx := range stmt.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		idxStrs = append(idxStrs, iexpr)
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	if len(stmt.Index) > 0 {
		if t, err := c.env.GetVar(stmt.Name); err == nil {
			if _, ok := t.(types.ListType); ok {
				idx := idxStrs[len(idxStrs)-1]
				c.writeln("run {")
				c.indent++
				c.writeln(fmt.Sprintf("val _tmp = %s.toMutableList()", sanitizeName(stmt.Name)))
				c.writeln(fmt.Sprintf("_tmp[%s] = %s", idx, rhs))
				c.writeln(fmt.Sprintf("%s = _tmp", sanitizeName(stmt.Name)))
				c.indent--
				c.writeln("}")
				return nil
			}
			if _, ok := t.(types.MapType); ok {
				idx := idxStrs[len(idxStrs)-1]
				c.writeln("run {")
				c.indent++
				c.writeln(fmt.Sprintf("val _tmp = %s.toMutableMap()", sanitizeName(stmt.Name)))
				c.writeln(fmt.Sprintf("_tmp[%s] = %s", idx, rhs))
				c.writeln(fmt.Sprintf("%s = _tmp", sanitizeName(stmt.Name)))
				c.indent--
				c.writeln("}")
				return nil
			}
		}
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, rhs))
	return nil
}

func (c *Compiler) compileReturn(stmt *parser.ReturnStmt) error {
	expr, err := c.compileExpr(stmt.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("fun " + name + "() {\n")
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
	c.writeln(fmt.Sprintf("check(%s)", expr))
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("sealed interface %s", name))
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			fields := []string{}
			for _, f := range v.Fields {
				typ := ktType(c.resolveTypeRef(f.Type))
				fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(f.Name), typ))
			}
			if len(fields) == 0 {
				c.writeln(fmt.Sprintf("data class %s() : %s", vname, name))
			} else {
				c.writeln(fmt.Sprintf("data class %s(%s) : %s", vname, joinArgs(fields), name))
			}
		}
		return nil
	}
	fields := []string{}
	var methods []*parser.FunStmt
	for _, m := range t.Members {
		if m.Field != nil {
			typ := ktType(c.resolveTypeRef(m.Field.Type))
			fields = append(fields, fmt.Sprintf("val %s: %s", sanitizeName(m.Field.Name), typ))
		} else if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	if len(methods) == 0 {
		c.writeln(fmt.Sprintf("data class %s(%s)", name, joinArgs(fields)))
		return nil
	}
	c.writeln(fmt.Sprintf("data class %s(%s) {", name, joinArgs(fields)))
	c.indent++
	for _, m := range methods {
		if err := c.compileMethod(name, m); err != nil {
			return err
		}
		c.writeln("")
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
		c.writeln(fmt.Sprintf("for (%s in %s until %s) {", name, start, end))
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
	loopSrc := src
	if isMap(stmt.Source, c.env) {
		loopSrc += ".keys"
	}
	c.writeln(fmt.Sprintf("for (%s in %s) {", name, loopSrc))
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

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
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

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	cur := stmt
	for cur.ElseIf != nil {
		next := cur.ElseIf
		cond, err := c.compileExpr(next.Cond)
		if err != nil {
			return err
		}
		c.writeIndent()
		c.buf.WriteString("} else if (" + cond + ") {\n")
		c.indent++
		for _, s := range next.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		cur = next
	}
	if len(cur.Else) == 0 {
		c.writeln("}")
		return nil
	}
	c.writeIndent()
	c.buf.WriteString("} else {\n")
	c.indent++
	for _, s := range cur.Else {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
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
	c.use("_load")
	if l.Type != nil {
		t := c.resolveTypeRef(l.Type)
		typ := ktType(t)
		c.use("_cast")
		var buf bytes.Buffer
		buf.WriteString("run {\n")
		buf.WriteString(fmt.Sprintf("        val _rows = _load(%s, %s)\n", path, opts))
		buf.WriteString(fmt.Sprintf("        val _out = mutableListOf<%s>()\n", typ))
		buf.WriteString("        for (r in _rows) {\n")
		buf.WriteString(fmt.Sprintf("                _out.add(_cast<%s>(r))\n", typ))
		buf.WriteString("        }\n")
		buf.WriteString("        _out\n")
		buf.WriteString("}")
		return buf.String(), nil
	}
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
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var prompt, text, model string
	params := []string{}
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
			params = append(params, fmt.Sprintf("%q to %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramMap := "null"
	if len(params) > 0 {
		paramMap = "mapOf(" + joinArgs(params) + ")"
	}
	if model == "" {
		model = "null"
	}
	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramMap), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_genStruct")
			return fmt.Sprintf("_genStruct<%s>(%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramMap), nil
		}
	}
	c.use("_genText")
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramMap), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
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
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileModelDecl(m *parser.ModelDecl) error {
	c.models = true
	parts := make([]string, len(m.Fields))
	for i, f := range m.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%q to %s", f.Name, v)
	}
	c.writeln(fmt.Sprintf("_models[%q] = mapOf(%s)", m.Name, joinArgs(parts)))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	c.compileStructType(st)
	varName := "_" + sanitizeName(s.Name) + "Stream"
	c.writeln(fmt.Sprintf("var %s = _Stream<%s>(%q)", varName, sanitizeName(st.Name), s.Name))
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
	c.writeln(fmt.Sprintf("fun %s(ev: %s) {", handlerName, sanitizeName(st.Name)))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("val %s = ev", alias))
	child := types.NewEnv(c.env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.env = orig
			return err
		}
	}
	c.env = orig
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s.register(::%s)", streamVar, handlerName))
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
		parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
	}
	lit := fmt.Sprintf("%s(%s)", sanitizeName(st.Name), joinArgs(parts))
	streamVar := "_" + sanitizeName(e.Stream) + "Stream"
	c.writeln(fmt.Sprintf("%s.append(%s)", streamVar, lit))
	c.use("_Stream")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	name := sanitizeName(u.Target)
	c.writeln(fmt.Sprintf("for (i in 0 until %s.size) {", name))
	c.indent++
	c.writeln(fmt.Sprintf("var _item = %s[i]", name))

	if t, err := c.env.GetVar(u.Target); err == nil {
		if lt, ok := t.(types.ListType); ok {
			if st, ok2 := lt.Elem.(types.StructType); ok2 {
				child := types.NewEnv(c.env)
				for _, f := range st.Order {
					ft := st.Fields[f]
					c.writeln(fmt.Sprintf("val %s = _item.%s", sanitizeName(f), sanitizeName(f)))
					child.SetVar(f, ft, true)
				}
				orig := c.env
				c.env = child
				var cond string
				var err error
				if u.Where != nil {
					cond, err = c.compileExpr(u.Where)
					if err != nil {
						c.env = orig
						return err
					}
					c.writeln(fmt.Sprintf("if (%s) {", cond))
					c.indent++
				}
				parts := make([]string, len(u.Set.Items))
				for i, it := range u.Set.Items {
					key, _ := identName(it.Key)
					val, err := c.compileExpr(it.Value)
					if err != nil {
						c.env = orig
						return err
					}
					parts[i] = fmt.Sprintf("%s = %s", sanitizeName(key), val)
				}
				if len(parts) > 0 {
					c.writeln("_item = _item.copy(" + joinArgs(parts) + ")")
				}
				if u.Where != nil {
					c.indent--
					c.writeln("}")
				}
				c.env = orig
			}
		}
	}
	c.writeln(fmt.Sprintf("%s[i] = _item", name))
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
	fields := make([]string, len(st.Order))
	for i, fn := range st.Order {
		typ := ktType(st.Fields[fn])
		fields[i] = fmt.Sprintf("val %s: %s", sanitizeName(fn), typ)
	}
	c.writeln(fmt.Sprintf("data class %s(%s)", name, joinArgs(fields)))
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
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
	c.writeln(fmt.Sprintf("object %s {", sanitizeName(alias)))
	c.indent++
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
			if s.Type != nil {
				if err := c.compileTypeDecl(s.Type); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				if err := c.compileFun(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	c.env = origEnv
	return nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	cast := ""
	if name, ok := identName(q.Source); ok {
		if t, err := c.env.GetVar(name); err == nil {
			if _, ok := t.(types.GroupType); ok {
				src += ".Items"
				cast = " as List<Any?>"
			}
		}
	}

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		c.use("_Group")
		c.use("_group_by")
		expr := fmt.Sprintf("_group_by(%s) { %s -> %s }.map { %s -> %s }", src, sanitizeName(q.Var), keyExpr, sanitizeName(q.Group.Name), valExpr)
		return expr, nil
	}

	if len(q.Joins) > 0 || q.Group != nil {
		return c.compileAdvancedQueryExpr(q, src)
	}

	// simple cross join without sort/skip/take/group/join
	if len(q.Froms) > 0 && q.Sort == nil && q.Skip == nil && q.Take == nil && q.Group == nil && len(q.Joins) == 0 {
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		for _, f := range q.Froms {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
		c.env = child
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		var where string
		if q.Where != nil {
			where, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			fromSrcs[i] = fs
		}
		c.env = orig

		var buf bytes.Buffer
		buf.WriteString("run {\n")
		buf.WriteString("                val _src = " + src + "\n")
		selType := c.inferExprType(q.Select)
		typ := ktType(selType)
		if typ == "Any" {
			buf.WriteString("                val _res = mutableListOf<Any>()\n")
		} else {
			buf.WriteString("                val _res = mutableListOf<" + typ + ">()\n")
		}
		buf.WriteString(fmt.Sprintf("                for (%s in _src) {\n", sanitizeName(q.Var)))
		indent := "                        "
		for i, fs := range fromSrcs {
			buf.WriteString(indent + fmt.Sprintf("for (%s in %s) {\n", sanitizeName(q.Froms[i].Var), fs))
			indent += "        "
		}
		if where != "" {
			buf.WriteString(indent + "if (" + where + ") {\n")
			indent += "        "
		}
		buf.WriteString(indent + "_res.add(" + sel + ")\n")
		if where != "" {
			indent = indent[:len(indent)-8]
			buf.WriteString(indent + "}\n")
		}
		for range fromSrcs {
			indent = indent[:len(indent)-8]
			buf.WriteString(indent + "}\n")
		}
		indent = indent[:len(indent)-8]
		buf.WriteString(indent + "}\n")
		buf.WriteString("                _res\n")
		buf.WriteString("        }")
		return buf.String(), nil
	}

	varName := sanitizeName(q.Var)
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var where, sortKey, skip, take string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		sortKey, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skip, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		take, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	var buf bytes.Buffer
	buf.WriteString("run {\n")
	buf.WriteString("                var res = " + src + cast + "\n")
	if where != "" {
		buf.WriteString(fmt.Sprintf("                res = res.filter { %s -> %s }\n", varName, where))
	}
	if sortKey != "" {
		buf.WriteString(fmt.Sprintf("                res = res.sortedBy { %s -> %s }\n", varName, sortKey))
	}
	if skip != "" {
		buf.WriteString("                res = res.drop(" + skip + ")\n")
	}
	if take != "" {
		buf.WriteString("                res = res.take(" + take + ")\n")
	}
	buf.WriteString(fmt.Sprintf("                res = res.map { %s -> %s }\n", varName, sel))
	buf.WriteString("                res\n")
	buf.WriteString("        }")
	return buf.String(), nil
}

func (c *Compiler) compileAdvancedQueryExpr(q *parser.QueryExpr, src string) (string, error) {
	orig := c.env
	child := types.NewEnv(c.env)
	if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, true)
	} else {
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	for _, f := range q.Froms {
		if lt, ok := c.inferExprType(f.Src).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	for _, j := range q.Joins {
		if lt, ok := c.inferExprType(j.Src).(types.ListType); ok {
			child.SetVar(j.Var, lt.Elem, true)
		} else {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
	}
	c.env = child

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = fs
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinSrcs[i] = js
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}

	var whereExpr, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		w, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		whereExpr = w
	}
	if q.Sort != nil {
		s, err := c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
		sortExpr = s
	}
	if q.Skip != nil {
		sk, err := c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
		skipExpr = sk
	}
	if q.Take != nil {
		tk, err := c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
		takeExpr = tk
	}

	var keyExpr, groupSel string
	if q.Group != nil {
		k, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		keyExpr = k
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		gs, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		groupSel = gs
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	c.env = orig

	paramNames := []string{sanitizeName(q.Var)}
	specs := make([]string, 0, len(q.Froms)+len(q.Joins))
	for i, fs := range fromSrcs {
		specs = append(specs, fmt.Sprintf("_JoinSpec(items = %s)", fs))
		paramNames = append(paramNames, sanitizeName(q.Froms[i].Var))
	}
	for i, js := range joinSrcs {
		onParams := append(append([]string(nil), paramNames...), sanitizeName(q.Joins[i].Var))
		unpack := ktUnpackArgs(onParams, "                        ", child)
		onBody := unpack + joinOns[i]
		spec := fmt.Sprintf("_JoinSpec(items = %s, on = { args ->\n%s\n                        })", js, onBody)
		if joinSides[i] == "left" || joinSides[i] == "outer" {
			spec += ", left = true"
		}
		if joinSides[i] == "right" || joinSides[i] == "outer" {
			spec += ", right = true"
		}
		spec += ")"
		specs = append(specs, spec)
		paramNames = append(paramNames, sanitizeName(q.Joins[i].Var))
	}

	unpackAll := ktUnpackArgs(paramNames, "                        ", child)
	selectBody := unpackAll + sel
	if q.Group != nil {
		rowExpr := sanitizeName(q.Var)
		if len(paramNames) > 1 {
			rowExpr = "arrayOf(" + joinArgs(paramNames) + ")"
		}
		selectBody = unpackAll + rowExpr
	}
	selectFn := fmt.Sprintf("{ args ->\n%s\n                        }", selectBody)

	var whereFn, sortFn string
	if whereExpr != "" {
		whereBody := ktUnpackArgs(paramNames, "                        ", child) + whereExpr
		whereFn = fmt.Sprintf("{ args ->\n%s\n                        }", whereBody)
	}
	if sortExpr != "" {
		sortBody := ktUnpackArgs(paramNames, "                        ", child) + sortExpr
		sortFn = fmt.Sprintf("{ args ->\n%s\n                        }", sortBody)
	}

	var buf bytes.Buffer
	buf.WriteString("run {\n")
	buf.WriteString("                val _src = " + src + "\n")
	buf.WriteString("                val _rows = _query(_src, listOf(\n")
	for i, sp := range specs {
		buf.WriteString("                        " + sp)
		if i != len(specs)-1 {
			buf.WriteString(",")
		}
		buf.WriteString("\n")
	}
	buf.WriteString("                ), _QueryOpts(selectFn = " + selectFn)
	if whereFn != "" {
		buf.WriteString(", where = " + whereFn)
	}
	if sortFn != "" {
		buf.WriteString(", sortKey = " + sortFn)
	}
	if skipExpr != "" {
		buf.WriteString(", skip = " + skipExpr)
	}
	if takeExpr != "" {
		buf.WriteString(", take = " + takeExpr)
	}
	buf.WriteString(") )\n")

	if q.Group != nil {
		buf.WriteString("                val _groups = _group_by(_rows) { args ->\n")
		buf.WriteString(ktUnpackArgs(paramNames, "                        ", child))
		buf.WriteString("                        " + keyExpr + "\n")
		buf.WriteString("                }\n")
		buf.WriteString("                val _res = mutableListOf<Any?>()\n")
		buf.WriteString(fmt.Sprintf("                for (%s in _groups) {\n", sanitizeName(q.Group.Name)))
		buf.WriteString("                        _res.add(" + groupSel + ")\n")
		buf.WriteString("                }\n")
		buf.WriteString("                _res\n")
	} else {
		buf.WriteString("                _rows\n")
	}
	buf.WriteString("        }")
	c.use("_query")
	c.use("_arrConcat")
	if q.Group != nil {
		c.use("_group_by")
		c.use("_Group")
	}
	return buf.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString("run {\n")
	buf.WriteString("                val _t = " + target + "\n")
	buf.WriteString("                when {\n")
	for i, cse := range m.Cases {
		res, err := c.compileExpr(cse.Result)
		if err != nil {
			return "", err
		}
		if call, ok := callPattern(cse.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				buf.WriteString("                        _t is " + sanitizeName(call.Func) + " -> {\n")
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := sanitizeName(st.Order[idx])
						buf.WriteString(fmt.Sprintf("                                val %s = _t.%s\n", sanitizeName(id), field))
					}
				}
				buf.WriteString("                                " + res + "\n")
				buf.WriteString("                        }\n")
				continue
			}
		}
		if ident, ok := identName(cse.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				buf.WriteString("                        _t is " + sanitizeName(ident) + " -> " + res + "\n")
				continue
			}
		}
		if isUnderscoreExpr(cse.Pattern) {
			buf.WriteString("                        else -> " + res + "\n")
			continue
		}
		pat, err := c.compileExpr(cse.Pattern)
		if err != nil {
			return "", err
		}
		buf.WriteString("                        _t == " + pat + " -> " + res + "\n")
		if i == len(m.Cases)-1 {
			buf.WriteString("                        else -> null\n")
		}
	}
	buf.WriteString("                }\n")
	buf.WriteString("        }")
	return buf.String(), nil
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
	return fmt.Sprintf("if (%s) %s else %s", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		param := sanitizeName(p.Name)
		if p.Type != nil {
			typ := ktType(c.resolveTypeRef(p.Type))
			param += ": " + typ
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
		params[i] = param
	}
	sub := &Compiler{env: child, helpers: c.helpers, packages: c.packages, structs: c.structs}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	ret := "Unit"
	if fn.Return != nil {
		ret = ktType(c.resolveTypeRef(fn.Return))
	}
	return "fun(" + joinArgs(params) + ") : " + ret + " {\n" + body + "}", nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fun " + name + "(")
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
		if p.Type != nil {
			typ := c.resolveTypeRef(p.Type)
			c.buf.WriteString(": " + ktType(typ))
			child.SetVar(p.Name, typ, true)
		}
	}
	ret := "Unit"
	if fun.Return != nil {
		ret = ktType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString(") : " + ret + " {")
	c.buf.WriteByte('\n')
	sub := &Compiler{env: child, indent: c.indent + 1, helpers: c.helpers, packages: c.packages, structs: c.structs}
	for _, s := range fun.Body {
		if err := sub.compileStmt(s); err != nil {
			return err
		}
	}
	c.buf.Write(sub.buf.Bytes())
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	c.writeIndent()
	c.buf.WriteString("fun " + name + "(")
	child := types.NewEnv(c.env)
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			for fname, t := range st.Fields {
				child.SetVar(fname, t, true)
			}
		}
	}
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
		if p.Type != nil {
			typ := c.resolveTypeRef(p.Type)
			c.buf.WriteString(": " + ktType(typ))
			child.SetVar(p.Name, typ, true)
		}
	}
	ret := "Unit"
	if fun.Return != nil {
		ret = ktType(c.resolveTypeRef(fun.Return))
	}
	c.buf.WriteString(") : " + ret + " {")
	c.buf.WriteByte('\n')
	sub := &Compiler{env: child, indent: c.indent + 1, helpers: c.helpers, packages: c.packages, structs: c.structs}
	for _, s := range fun.Body {
		if err := sub.compileStmt(s); err != nil {
			return err
		}
	}
	c.buf.Write(sub.buf.Bytes())
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileExternVar(ev *parser.ExternVarDecl) error {
	name := sanitizeName(ev.Name())
	typ := ktType(c.resolveTypeRef(ev.Type))
	c.writeln(fmt.Sprintf("var %s: %s = TODO(\"extern\")", name, typ))
	if c.env != nil {
		c.env.SetVar(ev.Name(), c.resolveTypeRef(ev.Type), true)
	}
	return nil
}

func (c *Compiler) compileExternFun(ef *parser.ExternFunDecl) error {
	params := make([]string, len(ef.Params))
	var ptypes []types.Type
	for i, p := range ef.Params {
		param := sanitizeName(p.Name)
		if p.Type != nil {
			typ := c.resolveTypeRef(p.Type)
			param += ": " + ktType(typ)
			if c.env != nil {
				ptypes = append(ptypes, typ)
			}
		}
		params[i] = param
	}
	ret := "Unit"
	if ef.Return != nil {
		ret = ktType(c.resolveTypeRef(ef.Return))
	}
	c.writeln(fmt.Sprintf("fun %s(%s): %s = TODO(\"extern\")", sanitizeName(ef.Name()), joinArgs(params), ret))
	if c.env != nil {
		ft := types.FuncType{Params: ptypes, Return: c.resolveTypeRef(ef.Return)}
		c.env.SetVar(ef.Name(), ft, false)
	}
	return nil
}

func (c *Compiler) compileExternType(et *parser.ExternTypeDecl) error {
	c.writeln(fmt.Sprintf("typealias %s = Any", sanitizeName(et.Name)))
	return nil
}

func (c *Compiler) compileExternObject(eo *parser.ExternObjectDecl) error {
	name := sanitizeName(eo.Name)
	c.use("_extern")
	c.writeln(fmt.Sprintf("val %s = ExternRegistry._externGet(\"%s\")", name, eo.Name))
	if c.env != nil {
		c.env.SetVar(eo.Name, types.AnyType{}, true)
	}
	return nil
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}

	operands := []string{left}
	lists := []bool{isListUnary(b.Left, c.env)}
	ops := []string{}
	for _, part := range b.Right {
		rhs, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		operands = append(operands, rhs)
		lists = append(lists, isListPostfix(part.Right, c.env))
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"&&"}, {"||"}, {"union", "union_all", "except", "intersect"}}

	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				op := ops[i]
				var expr string
				var isList bool
				if op == "in" {
					expr = fmt.Sprintf("%s.contains(%s)", r, l)
				} else if op == "union" {
					c.use("_union")
					expr = fmt.Sprintf("_union(%s, %s)", l, r)
					isList = true
				} else if op == "union_all" {
					c.use("_unionAll")
					expr = fmt.Sprintf("_unionAll(%s, %s)", l, r)
					isList = true
				} else if op == "except" {
					c.use("_except")
					expr = fmt.Sprintf("_except(%s, %s)", l, r)
					isList = true
				} else if op == "intersect" {
					c.use("_intersect")
					expr = fmt.Sprintf("_intersect(%s, %s)", l, r)
					isList = true
				} else if op == "+" && (lists[i] || lists[i+1]) {
					c.use("_concat")
					expr = fmt.Sprintf("_concat(%s, %s)", l, r)
					isList = true
				} else {
					expr = fmt.Sprintf("(%s %s %s)", l, op, r)
				}
				operands[i] = expr
				lists[i] = isList
				operands = append(operands[:i+1], operands[i+2:]...)
				lists = append(lists[:i+1], lists[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("invalid expression")
	}

	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" || op == "!" {
			expr = fmt.Sprintf("%s%s", op, expr)
		}
	}
	return expr, nil
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
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isStringPrimary(p.Target, c.env) {
					c.use("_indexString")
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
				continue
			}
			start := "0"
			if op.Index.Start != nil {
				s, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				start = s
			}
			end := fmt.Sprintf("%s.length", expr)
			if op.Index.End != nil {
				e, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				end = e
			}
			if isStringPrimary(p.Target, c.env) {
				c.use("_sliceString")
				expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
			} else {
				expr = fmt.Sprintf("%s.subList(%s, %s)", expr, start, end)
			}
			continue
		}
		if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			switch t.(type) {
			case types.FloatType:
				expr = fmt.Sprintf("(%s).toDouble()", expr)
			case types.IntType:
				expr = fmt.Sprintf("(%s).toInt()", expr)
			case types.StringType:
				expr = fmt.Sprintf("(%s).toString()", expr)
			default:
				expr = fmt.Sprintf("(%s) as %s", expr, ktType(t))
			}
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, arg)
			}
			if expr == "print" {
				expr = fmt.Sprintf("println(%s)", joinArgs(args))
			} else if expr == "len" {
				if len(op.Call.Args) == 1 && isString(op.Call.Args[0], c.env) {
					expr = fmt.Sprintf("%s.length", args[0])
				} else {
					expr = fmt.Sprintf("%s.size", args[0])
				}
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, joinArgs(args))
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return fmt.Sprintf("%g", *p.Lit.Float), nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", *p.Lit.Str), nil
		}
		if p.Lit.Null {
			return "null", nil
		}
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			ce, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems = append(elems, ce)
		}
		if len(elems) == 0 {
			return "listOf<Any?>()", nil
		}
		return "listOf(" + joinArgs(elems) + ")", nil
	case p.Map != nil:
		items := []string{}
		for _, it := range p.Map.Items {
			var k string
			if s, ok := simpleStringKey(it.Key); ok {
				k = fmt.Sprintf("\"%s\"", s)
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
			items = append(items, fmt.Sprintf("%s to %s", k, v))
		}
		if len(items) == 0 {
			return "mutableMapOf<Any, Any>()", nil
		}
		return "mutableMapOf(" + joinArgs(items) + ")", nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) == 0 {
			return expr, nil
		}
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			switch t.(type) {
			case types.StructType, types.GroupType, types.UnionType:
				for _, f := range p.Selector.Tail {
					expr += "." + sanitizeName(f)
				}
				return expr, nil
			}
		}
		for i, f := range p.Selector.Tail {
			if i == len(p.Selector.Tail)-1 && len(p.Selector.Tail) > 1 {
				expr += "." + sanitizeName(f)
			} else {
				expr += fmt.Sprintf("[%q]", sanitizeName(f))
			}
		}
		return expr, nil
	case p.Struct != nil:
		args := []string{}
		for _, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			args = append(args, fmt.Sprintf("%s = %s", sanitizeName(f.Name), v))
		}
		return sanitizeName(p.Struct.Name) + "(" + joinArgs(args) + ")", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			ce, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, ce)
		}
		name := sanitizeName(p.Call.Func)
		if name == "print" {
			return "println(" + joinArgs(args) + ")", nil
		}
		if name == "len" && len(args) == 1 {
			if isString(p.Call.Args[0], c.env) {
				return args[0] + ".length", nil
			}
			return args[0] + ".size", nil
		}
		if name == "count" && len(args) == 1 {
			return args[0] + ".size", nil
		}
		if name == "avg" && len(args) == 1 {
			t := c.inferExprType(p.Call.Args[0])
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.IntType); ok {
					return args[0] + ".average()", nil
				}
				if _, ok := lt.Elem.(types.FloatType); ok {
					return args[0] + ".average()", nil
				}
			}
			c.use("_avg")
			return fmt.Sprintf("_avg(%s)", args[0]), nil
		}
		if name == "sum" && len(args) == 1 {
			c.use("_sum")
			return fmt.Sprintf("_sum(%s)", args[0]), nil
		}
		if name == "min" && len(args) == 1 {
			c.use("_min")
			return fmt.Sprintf("_min(%s)", args[0]), nil
		}
		if name == "max" && len(args) == 1 {
			c.use("_max")
			return fmt.Sprintf("_max(%s)", args[0]), nil
		}
		if name == "reduce" && len(args) == 3 {
			var buf bytes.Buffer
			buf.WriteString("run {\n")
			buf.WriteString("                val _fn = " + args[1] + "\n")
			buf.WriteString("                " + args[0] + ".fold(" + args[2] + ") { acc, it -> _fn(acc, it) }\n")
			buf.WriteString("        }")
			return buf.String(), nil
		}
		if name == "now" && len(args) == 0 {
			return "(System.currentTimeMillis() * 1000000)", nil
		}
		if name == "json" && len(args) == 1 {
			c.use("_json")
			return fmt.Sprintf("_json(%s)", args[0]), nil
		}
		if name == "str" && len(args) == 1 {
			return args[0] + ".toString()", nil
		}
		if name == "input" && len(args) == 0 {
			return "readln()", nil
		}
		if name == "eval" && len(args) == 1 {
			c.use("_eval")
			return fmt.Sprintf("_eval(%s)", args[0]), nil
		}
		return name + "(" + joinArgs(args) + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}

func ktType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "Int"
	case types.Int64Type:
		return "Long"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return "List<" + ktType(tt.Elem) + ">"
	case types.MapType:
		return "MutableMap<" + ktType(tt.Key) + ", " + ktType(tt.Value) + ">"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	default:
		return "Any"
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func joinArgs(args []string) string {
	if len(args) == 0 {
		return ""
	}
	res := args[0]
	for i := 1; i < len(args); i++ {
		res += ", " + args[i]
	}
	return res
}

func ktUnpackArgs(names []string, indent string, env *types.Env) string {
	if len(names) == 0 {
		return ""
	}
	var b strings.Builder
	for i, n := range names {
		b.WriteString(indent)
		b.WriteString("val ")
		b.WriteString(n)
		b.WriteString(" = args[")
		b.WriteString(fmt.Sprint(i))
		b.WriteString("]")
		if env != nil {
			if t, err := env.GetVar(n); err == nil {
				switch tt := t.(type) {
				case types.MapType:
					b.WriteString(" as MutableMap<" + ktType(tt.Key) + ", " + ktType(tt.Value) + ">")
				case types.ListType:
					b.WriteString(" as List<" + ktType(tt.Elem) + ">")
				case types.StructType, types.UnionType:
					b.WriteString(" as " + ktType(tt))
				case types.GroupType:
					b.WriteString(" as _Group")
				}
			}
		}
		b.WriteByte('\n')
	}
	return b.String()
}

func (c *Compiler) use(name string) {
	c.helpers[name] = true
}

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.writeln(helperMap[n])
	}
}
