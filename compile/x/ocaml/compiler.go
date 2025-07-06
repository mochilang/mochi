package mlcode

import (
	"bytes"
	"fmt"
	"os/exec"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into OCaml source code (very limited subset).
type Compiler struct {
	pre       bytes.Buffer
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	tmp       int
	vars      map[string]bool
	mapVars   map[string]bool
	structs   map[string]bool
	setNth    bool
	slice     bool
	input     bool
	dataset   bool
	fetch     bool
	loopTmp   int
	loops     []loopCtx
	funTmp    int
	groupVars map[string]bool
	groupType bool
	concat    bool
	union     bool
	reverseS  bool
	reverseL  bool
	sumFn     bool
	avgFn     bool
	maxFn     bool
	minFn     bool
	substrFn  bool
}

type loopCtx struct {
	brk  string
	cont string
}

func hasLoopCtrl(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Break != nil || s.Continue != nil:
			return true
		case s.For != nil:
			if hasLoopCtrl(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasLoopCtrl(s.While.Body) {
				return true
			}
		case s.If != nil:
			if hasLoopCtrl(s.If.Then) || hasLoopCtrlIf(s.If.ElseIf) || hasLoopCtrl(s.If.Else) {
				return true
			}
		}
	}
	return false
}

func hasLoopCtrlIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasLoopCtrl(ifst.Then) || hasLoopCtrl(ifst.Else) {
		return true
	}
	return hasLoopCtrlIf(ifst.ElseIf)
}

func programHasLoopCtrl(stmts []*parser.Statement) bool {
	if hasLoopCtrl(stmts) {
		return true
	}
	for _, s := range stmts {
		if s.Fun != nil {
			if hasLoopCtrl(s.Fun.Body) {
				return true
			}
		}
	}
	return false
}

// New creates a new OCaml compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:       env,
		vars:      map[string]bool{},
		mapVars:   map[string]bool{},
		structs:   map[string]bool{},
		loops:     []loopCtx{},
		groupVars: map[string]bool{},
	}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newLoopID() string {
	id := fmt.Sprintf("%d", c.loopTmp)
	c.loopTmp++
	return id
}

func (c *Compiler) pushLoop(brk, cont string) {
	c.loops = append(c.loops, loopCtx{brk: brk, cont: cont})
}

func (c *Compiler) popLoop() {
	if len(c.loops) > 0 {
		c.loops = c.loops[:len(c.loops)-1]
	}
}

func hasTest(p *parser.Program) bool {
	for _, s := range p.Statements {
		if s.Test != nil {
			return true
		}
	}
	return false
}

// Compile returns OCaml source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if programHasLoopCtrl(prog.Statements) {
		c.pre.WriteString("exception BreakException of int\n")
		c.pre.WriteString("exception ContinueException of int\n\n")
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil || s.Type != nil {
			continue
		}
		if err := c.compileStmt(s, ""); err != nil {
			return nil, err
		}
	}
	if hasTest(prog) {
		for _, s := range prog.Statements {
			if s.Test != nil {
				name := "test_" + sanitizeName(s.Test.Name)
				c.writeln(name + " ();;")
			}
		}
	}
	if c.setNth || c.slice || c.input {
		c.writeln("")
	}
	var out bytes.Buffer
	out.Write(c.pre.Bytes())
	out.Write(c.buf.Bytes())
	return formatOCaml(out.Bytes()), nil
}

func formatOCaml(src []byte) []byte {
	if _, err := exec.LookPath("ocamlformat"); err == nil {
		cmd := exec.Command("ocamlformat", "--enable-outside-detected-project", "--name", "mochi.ml", "-")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			return bytes.TrimSpace(out.Bytes())
		}
	}
	if _, err := exec.LookPath("ocp-indent"); err == nil {
		cmd := exec.Command("ocp-indent")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			return bytes.TrimSpace(out.Bytes())
		}
	}
	return src
}

func (c *Compiler) ensureSetNth() {
	if c.setNth {
		return
	}
	c.setNth = true
	b := &c.pre
	b.WriteString("let rec _set_nth lst i v =\n")
	b.WriteString("  match lst with\n")
	b.WriteString("  | [] -> []\n")
	b.WriteString("  | x::xs ->\n")
	b.WriteString("      if i = 0 then v :: xs else x :: _set_nth xs (i - 1) v\n\n")
}

func (c *Compiler) ensureSlice() {
	if c.slice {
		return
	}
	c.slice = true
	b := &c.pre
	b.WriteString("let rec _slice lst i len =\n")
	b.WriteString("  match lst with\n")
	b.WriteString("  | [] -> []\n")
	b.WriteString("  | x::xs ->\n")
	b.WriteString("      if i > 0 then _slice xs (i - 1) len\n")
	b.WriteString("      else if len = 0 then []\n")
	b.WriteString("      else x :: _slice xs 0 (len - 1)\n\n")
}

func (c *Compiler) ensureInput() {
	if c.input {
		return
	}
	c.input = true
	b := &c.pre
	b.WriteString("let _input () = read_line ();;\n\n")
}

func (c *Compiler) ensureDataset() {
	if c.dataset {
		return
	}
	c.dataset = true
	b := &c.pre
	b.WriteString("let rec _read_all ic =\n")
	b.WriteString("  try let line = input_line ic in\n")
	b.WriteString("      line ^ \"\\n\" ^ _read_all ic\n")
	b.WriteString("  with End_of_file -> \"\";;\n")
	b.WriteString("\n")
	b.WriteString("let _read_input path =\n")
	b.WriteString("  let ic = match path with\n")
	b.WriteString("    | None -> stdin\n")
	b.WriteString("    | Some p when p = \"\" || p = \"-\" -> stdin\n")
	b.WriteString("    | Some p -> open_in p in\n")
	b.WriteString("  let txt = _read_all ic in\n")
	b.WriteString("  if ic != stdin then close_in ic;\n")
	b.WriteString("  txt;;\n")
	b.WriteString("\n")
	b.WriteString("let _load path _ =\n")
	b.WriteString("  let text = _read_input path in\n")
	b.WriteString("  match Yojson.Basic.from_string text with\n")
	b.WriteString("  | `List items -> items\n")
	b.WriteString("  | json -> [json];;\n")
	b.WriteString("\n")
	b.WriteString("let _save rows path _ =\n")
	b.WriteString("  let oc = match path with\n")
	b.WriteString("    | None -> stdout\n")
	b.WriteString("    | Some p when p = \"\" || p = \"-\" -> stdout\n")
	b.WriteString("    | Some p -> open_out p in\n")
	b.WriteString("  Yojson.Basic.to_channel oc (`List rows);\n")
	b.WriteString("  output_char oc '\\n';\n")
	b.WriteString("  if oc != stdout then close_out oc;;\n\n")
}

func (c *Compiler) ensureGroupType() {
	if c.groupType {
		return
	}
	c.groupType = true
	b := &c.pre
	b.WriteString("type ('k,'v) _group = { key: 'k; items: 'v list };;\n\n")
}

func (c *Compiler) ensureConcat() {
	if c.concat {
		return
	}
	c.concat = true
	c.pre.WriteString("let _concat a b = a @ b;;\n\n")
}

func (c *Compiler) ensureUnion() {
	if c.union {
		return
	}
	c.union = true
	c.pre.WriteString("let rec _union a b = match a with | [] -> b | x::xs -> if List.mem x b then _union xs b else _union xs (b @ [x]);;\n\n")
	c.pre.WriteString("let _union_all a b = a @ b;;\n\n")
}

func (c *Compiler) ensureReverseString() {
	if c.reverseS {
		return
	}
	c.reverseS = true
	c.pre.WriteString("let _reverse_string s = String.init (String.length s) (fun i -> s.[String.length s - i - 1]);;\n\n")
}

func (c *Compiler) ensureReverseList() {
	if c.reverseL {
		return
	}
	c.reverseL = true
	c.pre.WriteString("let _reverse_list l = List.rev l;;\n\n")
}

func (c *Compiler) ensureSum() {
	if c.sumFn {
		return
	}
	c.sumFn = true
	c.pre.WriteString("let _sum l = List.fold_left ( + ) 0 l;;\n\n")
}

func (c *Compiler) ensureAvg() {
	if c.avgFn {
		return
	}
	c.ensureSum()
	c.avgFn = true
	c.pre.WriteString("let _avg l = (_sum l) / List.length l;;\n\n")
}

func (c *Compiler) ensureMax() {
	if c.maxFn {
		return
	}
	c.maxFn = true
	c.pre.WriteString("let _max l = match l with [] -> 0 | h::t -> List.fold_left max h t;;\n\n")
}

func (c *Compiler) ensureMin() {
	if c.minFn {
		return
	}
	c.minFn = true
	c.pre.WriteString("let _min l = match l with [] -> 0 | h::t -> List.fold_left min h t;;\n\n")
}

func (c *Compiler) ensureSubstr() {
	if c.substrFn {
		return
	}
	c.substrFn = true
	c.pre.WriteString("let _substr s i len =\n  let start = if i < 0 then String.length s + i else i in\n  let start = if start < 0 then 0 else start in\n  let length = if start + len > String.length s then String.length s - start else len in\n  if length <= 0 then \"\" else String.sub s start length;;\n\n")
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	ex := fmt.Sprintf("Return_%d", c.tmp)
	c.tmp++
	retTyp := c.ocamlType(fn.Return)
	if retTyp == "" {
		c.writeln(fmt.Sprintf("exception %s", ex))
	} else {
		c.writeln(fmt.Sprintf("exception %s of %s", ex, retTyp))
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	// local environment with parameter types
	origEnv := c.env
	c.env = types.NewEnv(origEnv)
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil {
			var t types.Type
			switch *p.Type.Simple {
			case "int":
				t = types.IntType{}
			case "float":
				t = types.FloatType{}
			case "string":
				t = types.StringType{}
			case "bool":
				t = types.BoolType{}
			}
			if t != nil {
				c.env.SetVar(p.Name, t, false)
			}
		}
	}
	defer func() { c.env = origEnv }()
	c.writeln(fmt.Sprintf("let rec %s %s =", sanitizeName(fn.Name), strings.Join(params, " ")))
	c.indent++
	c.writeln("try")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(fmt.Sprintf("with %s v -> v", ex))
	c.indent--
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement, ex string) error {
	switch {
	case s.Type != nil:
		if err := c.compileTypeDecl(s.Type); err != nil {
			return err
		}
		if ex == "" {
			c.writeln("")
		}
		return nil
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		if ex == "" {
			c.writeln(fmt.Sprintf("let %s = %s;;", sanitizeName(s.Let.Name), val))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s in", sanitizeName(s.Let.Name), val))
		}
	case s.Var != nil:
		val := "()"
		if s.Var.Value != nil {
			var err error
			val, err = c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
		}
		name := sanitizeName(s.Var.Name)
		if c.vars == nil {
			c.vars = map[string]bool{}
		}
		c.vars[name] = true
		if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "map" {
			if c.mapVars == nil {
				c.mapVars = map[string]bool{}
			}
			c.mapVars[name] = true
		} else if s.Var.Value != nil && s.Var.Value.Binary != nil && s.Var.Value.Binary.Left != nil && s.Var.Value.Binary.Left.Value != nil && s.Var.Value.Binary.Left.Value.Target != nil && s.Var.Value.Binary.Left.Value.Target.Map != nil {
			if c.mapVars == nil {
				c.mapVars = map[string]bool{}
			}
			c.mapVars[name] = true
		}
		if strings.HasPrefix(val, "fun ") || strings.HasPrefix(val, "fun(") {
			val = "(" + val + ")"
		}
		if ex == "" {
			c.writeln(fmt.Sprintf("let %s = ref %s;;", name, val))
		} else {
			c.writeln(fmt.Sprintf("let %s = ref %s in", name, val))
		}
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Assign.Name)
		target := name
		if c.vars[name] {
			target = "!" + name
		}
		if len(s.Assign.Index) > 0 {
			idx, err := c.compileExpr(s.Assign.Index[0].Start)
			if err != nil {
				return err
			}
			if c.mapVars[name] {
				if ex == "" {
					c.writeln(fmt.Sprintf("Hashtbl.replace %s %s %s;;", target, idx, val))
				} else {
					c.writeln(fmt.Sprintf("Hashtbl.replace %s %s %s;", target, idx, val))
				}
				return nil
			} else if c.vars[name] {
				c.ensureSetNth()
				if ex == "" {
					c.writeln(fmt.Sprintf("%s := _set_nth %s %s (%s);;", name, target, idx, val))
				} else {
					c.writeln(fmt.Sprintf("%s := _set_nth %s %s (%s);", name, target, idx, val))
				}
				return nil
			}
		}
		if ex == "" {
			c.writeln(fmt.Sprintf("%s := %s;;", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s := %s;", name, val))
		}
	case s.Fun != nil:
		if err := c.compileFun(s.Fun); err != nil {
			return err
		}
		if ex != "" {
			c.writeln("in")
		} else {
			c.writeln("")
		}
	case s.While != nil:
		return c.compileWhile(s.While, ex)
	case s.Return != nil:
		if ex == "" {
			return fmt.Errorf("return outside function")
		}
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("raise (%s (%s))", ex, val))
	case s.For != nil:
		return c.compileFor(s.For, ex)
	case s.If != nil:
		return c.compileIf(s.If, ex)
	case s.Break != nil:
		if len(c.loops) == 0 {
			return fmt.Errorf("break not in loop")
		}
		c.writeln(fmt.Sprintf("raise (%s)", c.loops[len(c.loops)-1].brk))
	case s.Continue != nil:
		if len(c.loops) == 0 {
			return fmt.Errorf("continue not in loop")
		}
		c.writeln(fmt.Sprintf("raise (%s)", c.loops[len(c.loops)-1].cont))
	case s.Update != nil:
		return c.compileUpdate(s.Update, ex)
	case s.Expect != nil:
		return c.compileExpect(s.Expect, ex)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if ex == "" {
			c.writeln(expr + ";;")
		} else {
			c.writeln(expr + ";")
		}
	default:
		// ignore other statements
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt, ex string) error {
	// create scoped environment with loop variable type for proper codegen
	origEnv := c.env
	if origEnv != nil {
		child := types.NewEnv(origEnv)
		if f.RangeEnd != nil {
			// numeric range loops yield ints
			child.SetVar(f.Name, types.IntType{}, true)
		} else if types.IsStringExpr(f.Source, origEnv) {
			child.SetVar(f.Name, types.StringType{}, true)
		} else if types.IsListExpr(f.Source, origEnv) {
			child.SetVar(f.Name, types.AnyType{}, true)
		} else if types.IsMapExpr(f.Source, origEnv) {
			child.SetVar(f.Name, types.AnyType{}, true)
		} else {
			child.SetVar(f.Name, types.AnyType{}, true)
		}
		c.env = child
		defer func() { c.env = origEnv }()
	}
	ctrl := hasLoopCtrl(f.Body)
	var id string
	var brk, cont string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	if f.RangeEnd == nil {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		iter := "List.iter"
		if types.IsStringExpr(f.Source, c.env) {
			iter = "String.iter"
		}
		c.writeln(fmt.Sprintf("%s (fun %s ->", iter, sanitizeName(f.Name)))
		c.indent++
		if ctrl {
			c.pushLoop(brk, cont)
			c.writeln("try")
			c.indent++
		}
		bodyEx := ex
		if bodyEx == "" {
			bodyEx = "loop"
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st, bodyEx); err != nil {
				return err
			}
		}
		if ctrl {
			c.indent--
			c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
			c.popLoop()
		}
		c.indent--
		if ex == "" {
			if ctrl {
				c.writeln(fmt.Sprintf(") %s;", src))
			} else {
				c.writeln(fmt.Sprintf(") %s;;", src))
			}
		} else {
			c.writeln(fmt.Sprintf(") %s;", src))
		}
		if ctrl {
			c.indent--
			if ex == "" {
				c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ();;", id))
			} else {
				c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
			}
		}
		return nil
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("for %s = %s to %s - 1 do", sanitizeName(f.Name), start, end))
	c.indent++
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ex == "" {
		if ctrl {
			c.writeln("done;")
		} else {
			c.writeln("done;;")
		}
	} else {
		c.writeln("done;")
	}
	if ctrl {
		c.indent--
		if ex == "" {
			c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ();", id))
		} else {
			c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
		}
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt, ex string) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(w.Body)
	var id string
	var brk, cont string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	c.writeln(fmt.Sprintf("while %s do", cond))
	c.indent++
	bodyEx := ex
	if bodyEx == "" {
		bodyEx = "loop"
	}
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st, bodyEx); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ex == "" {
		c.writeln("done;;")
	} else {
		c.writeln("done;")
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
	}
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt, ex string) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then begin", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	c.indent--
	if ifst.ElseIf != nil {
		c.writeln("end else begin")
		c.indent++
		if err := c.compileIf(ifst.ElseIf, ex); err != nil {
			return err
		}
		c.indent--
		if ex == "" {
			c.writeln("end")
		} else {
			c.writeln("end;")
		}
	} else if len(ifst.Else) > 0 {
		c.writeln("end else begin")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st, ex); err != nil {
				return err
			}
		}
		c.indent--
		if ex == "" {
			c.writeln("end")
		} else {
			c.writeln("end;")
		}
	} else {
		if ex == "" {
			c.writeln("end")
		} else {
			c.writeln("end;")
		}
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt, ex string) error {
	cond, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	if ex == "" {
		c.writeln(fmt.Sprintf("if not (%s) then failwith \"expect failed\";;", cond))
	} else {
		c.writeln(fmt.Sprintf("if not (%s) then failwith \"expect failed\";", cond))
	}
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt, ex string) error {
	name := sanitizeName(u.Target)

	var st types.StructType
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if s, ok2 := lt.Elem.(types.StructType); ok2 {
					st = s
				}
			}
		}
	}

	child := types.NewEnv(c.env)
	if st.Name != "" {
		for _, fn := range st.Order {
			child.SetVar(fn, st.Fields[fn], true)
		}
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
	}

	pairs := make([]string, len(u.Set.Items))
	for i, it := range u.Set.Items {
		key, ok := simpleIdent(it.Key)
		if !ok {
			c.env = orig
			return fmt.Errorf("update key must be identifier")
		}
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = orig
			return err
		}
		pairs[i] = fmt.Sprintf("%s = %s", sanitizeName(key), val)
	}

	c.writeln(fmt.Sprintf("let %s = List.map (fun _item ->", name))
	c.indent++
	if st.Name != "" {
		for _, fn := range st.Order {
			c.writeln(fmt.Sprintf("let %s = _item.%s in", sanitizeName(fn), sanitizeName(fn)))
		}
	}
	if cond != "" {
		c.writeln(fmt.Sprintf("if %s then", cond))
		c.indent++
		c.writeln(fmt.Sprintf("{ _item with %s }", strings.Join(pairs, "; ")))
		c.indent--
		c.writeln("else")
		c.indent++
		c.writeln("_item")
		c.indent--
	} else {
		c.writeln(fmt.Sprintf("{ _item with %s }", strings.Join(pairs, "; ")))
	}
	c.indent--
	if ex == "" {
		c.writeln(fmt.Sprintf(") %s;;", name))
	} else {
		c.writeln(fmt.Sprintf(") %s in", name))
	}

	c.env = orig
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("let %s () =", name))
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st, "test"); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "()", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	stringMode := types.IsStringExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}, c.env)
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		oper := op.Op
		if oper == "==" {
			oper = "="
		} else if oper == "!=" {
			oper = "<>"
		} else if oper == "%" {
			oper = "mod"
		} else if oper == "+" {
			// use list concatenation or string concat if operand is list or string
			rightStr := types.IsStringExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env)
			if types.IsListExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) {
				oper = "@"
			} else if stringMode || rightStr || types.IsStringExpr(&parser.Expr{Binary: b}, c.env) {
				oper = "^"
				// convert char from String.get to string for concatenation
				if strings.HasPrefix(r, "(String.get ") {
					r = fmt.Sprintf("(String.make 1 %s)", r)
				}
				if strings.HasPrefix(expr, "(String.get ") {
					expr = fmt.Sprintf("(String.make 1 %s)", expr)
				}
				stringMode = true
			}
		} else if oper == "/" {
			// always use float division to match Mochi semantics
			if !types.IsFloatExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) {
				r = fmt.Sprintf("float_of_int (%s)", r)
			}
			if !types.IsFloatExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}}, c.env) {
				expr = fmt.Sprintf("float_of_int (%s)", expr)
			}
			oper = "/."
		} else if oper == "union_all" {
			oper = "@"
		} else if oper == "union" {
			c.ensureUnion()
			expr = fmt.Sprintf("(_union %s %s)", expr, r)
			continue
		} else if oper == "in" {
			expr = fmt.Sprintf("Hashtbl.mem %s %s", r, expr)
			continue
		}
		expr = fmt.Sprintf("%s %s %s", expr, oper, r)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "!" {
			expr = fmt.Sprintf("not %s", expr)
		} else if op == "-" && (strings.HasPrefix(expr, "!") || strings.HasPrefix(expr, "not ")) {
			expr = fmt.Sprintf("-(%s)", expr)
		} else {
			expr = fmt.Sprintf("%s%s", op, expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if f := fetchFromPrimary(p.Target); f != nil && len(p.Ops) == 1 && p.Ops[0].Cast != nil {
		typ := c.resolveTypeRef(p.Ops[0].Cast.Type)
		return c.compileFetchExprTyped(f, typ)
	}
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("(%s %s)", expr, strings.Join(args, " "))
		} else if op.Index != nil {
			start, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				end := ""
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				}
				if types.IsStringPrimary(p.Target, c.env) {
					if end == "" {
						expr = fmt.Sprintf("(String.sub %s %s (String.length %s - %s))", expr, start, expr, start)
					} else {
						expr = fmt.Sprintf("(String.sub %s %s (%s - %s))", expr, start, end, start)
					}
				} else {
					base := expr
					var length string
					if end == "" {
						length = fmt.Sprintf("(List.length %s - %s)", base, start)
					} else {
						length = fmt.Sprintf("(%s - %s)", end, start)
					}
					c.ensureSlice()
					expr = fmt.Sprintf("(_slice %s %s %s)", base, start, length)
				}
			} else {
				if types.IsStringPrimary(p.Target, c.env) {
					expr = fmt.Sprintf("(String.get %s %s)", expr, start)
				} else if types.IsMapPrimary(p.Target, c.env) {
					expr = fmt.Sprintf("(Hashtbl.find %s %s)", expr, start)
				} else {
					expr = fmt.Sprintf("(List.nth %s %s)", expr, start)
				}
			}
		} else if op.Cast != nil {
			typ := c.ocamlType(op.Cast.Type)
			if typ == "float" {
				expr = fmt.Sprintf("(float_of_int %s)", expr)
			} else if typ != "" {
				expr = fmt.Sprintf("(%s : %s)", expr, typ)
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			s := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += "."
			}
			return s, nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		return "0", nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, "; ") + "]", nil
	case p.Map != nil:
		var b strings.Builder
		b.WriteString("(let tbl = Hashtbl.create ")
		b.WriteString(strconv.Itoa(len(p.Map.Items)))
		b.WriteString(" in ")
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if id, ok := simpleIdent(it.Key); ok {
				k = strconv.Quote(id)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			b.WriteString("Hashtbl.add tbl ")
			b.WriteString(k)
			b.WriteString(" ")
			b.WriteString(v)
			if i < len(p.Map.Items)-1 {
				b.WriteString(";")
			}
		}
		if len(p.Map.Items) > 0 {
			b.WriteString("; ")
		}
		b.WriteString("tbl)")
		return b.String(), nil
	case p.Struct != nil:
		if st, ok := c.env.GetStruct(p.Struct.Name); ok {
			c.compileStructType(st)
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
		}
		return "{ " + strings.Join(parts, "; ") + " }", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if c.groupVars[name] && len(p.Selector.Tail) == 0 {
			return name + ".items", nil
		}
		if c.vars[name] {
			name = "!" + name
		}
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("fun %s -> %s", strings.Join(params, " "), body), nil
	}
	c.funTmp++
	origEnv := c.env
	c.env = types.NewEnv(origEnv)
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil {
			var t types.Type
			switch *p.Type.Simple {
			case "int":
				t = types.IntType{}
			case "float":
				t = types.FloatType{}
			case "string":
				t = types.StringType{}
			case "bool":
				t = types.BoolType{}
			}
			if t != nil {
				c.env.SetVar(p.Name, t, false)
			}
		}
	}
	ex := fmt.Sprintf("Return_%d", c.tmp)
	c.tmp++
	sub := &Compiler{env: c.env, indent: 2, vars: map[string]bool{}, mapVars: map[string]bool{}}
	for _, st := range fn.BlockBody {
		if err := sub.compileStmt(st, ex); err != nil {
			c.env = origEnv
			return "", err
		}
	}
	if sub.setNth {
		c.ensureSetNth()
	}
	if sub.slice {
		c.ensureSlice()
	}
	if sub.input {
		c.ensureInput()
	}
	body := strings.TrimRight(sub.buf.String(), "\n")
	c.env = origEnv
	var out strings.Builder
	out.WriteString("fun ")
	out.WriteString(strings.Join(params, " "))
	out.WriteString(" ->\n  let exception ")
	retTyp := c.ocamlType(fn.Return)
	if retTyp == "" {
		out.WriteString(ex)
	} else {
		out.WriteString(ex + " of " + retTyp)
	}
	out.WriteString(" in\n  try\n")
	out.WriteString(body)
	if body != "" && !strings.HasSuffix(body, "\n") {
		out.WriteByte('\n')
	}
	out.WriteString("  with ")
	out.WriteString(ex)
	out.WriteString(" v -> v")
	return out.String(), nil
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
	case "len":
		if len(args) == 1 {
			if types.IsStringExpr(call.Args[0], c.env) {
				return fmt.Sprintf("String.length %s", args[0]), nil
			}
			if types.IsMapExpr(call.Args[0], c.env) {
				return fmt.Sprintf("Hashtbl.length %s", args[0]), nil
			}
			return fmt.Sprintf("List.length %s", args[0]), nil
		}
	case "print":
		if len(args) == 1 {
			if types.IsStringExpr(call.Args[0], c.env) {
				if _, ok := simpleIdent(call.Args[0]); ok || isStringLiteral(call.Args[0]) {
					return fmt.Sprintf("print_endline %s", args[0]), nil
				}
				return fmt.Sprintf("print_endline (%s)", args[0]), nil
			}
			if types.IsBoolExpr(call.Args[0], c.env) {
				return fmt.Sprintf("print_endline (string_of_bool (%s))", args[0]), nil
			}
			if types.IsFloatExpr(call.Args[0], c.env) {
				return fmt.Sprintf("print_endline (string_of_float (%s))", args[0]), nil
			}
			return fmt.Sprintf("print_endline (string_of_int (%s))", args[0]), nil
		}
		if len(args) > 0 {
			parts := make([]string, len(args))
			for i, a := range call.Args {
				if types.IsStringExpr(a, c.env) {
					parts[i] = args[i]
				} else if types.IsBoolExpr(a, c.env) {
					parts[i] = fmt.Sprintf("string_of_bool (%s)", args[i])
				} else if types.IsFloatExpr(a, c.env) {
					parts[i] = fmt.Sprintf("string_of_float (%s)", args[i])
				} else {
					parts[i] = fmt.Sprintf("string_of_int (%s)", args[i])
				}
			}
			return fmt.Sprintf("print_endline (String.concat \" \" [%s])", strings.Join(parts, "; ")), nil
		}
	case "str":
		if len(args) == 1 {
			if types.IsStringExpr(call.Args[0], c.env) {
				return args[0], nil
			}
			if types.IsFloatExpr(call.Args[0], c.env) {
				return fmt.Sprintf("string_of_float (%s)", args[0]), nil
			}
			return fmt.Sprintf("string_of_int (%s)", args[0]), nil
		}
	case "input":
		if len(args) == 0 {
			c.ensureInput()
			return "_input ()", nil
		}
	case "sum":
		if len(args) == 1 {
			c.ensureSum()
			return fmt.Sprintf("_sum %s", args[0]), nil
		}
	case "avg":
		if len(args) == 1 {
			c.ensureAvg()
			return fmt.Sprintf("_avg %s", args[0]), nil
		}
	case "max":
		if len(args) == 1 {
			c.ensureMax()
			return fmt.Sprintf("_max %s", args[0]), nil
		}
	case "min":
		if len(args) == 1 {
			c.ensureMin()
			return fmt.Sprintf("_min %s", args[0]), nil
		}
	case "concat":
		if len(args) >= 2 {
			c.ensureConcat()
			expr := fmt.Sprintf("_concat %s %s", args[0], args[1])
			for i := 2; i < len(args); i++ {
				expr = fmt.Sprintf("_concat %s %s", expr, args[i])
			}
			return expr, nil
		}
	case "reverse":
		if len(args) == 1 {
			if types.IsStringExpr(call.Args[0], c.env) {
				c.ensureReverseString()
				return fmt.Sprintf("_reverse_string %s", args[0]), nil
			}
			c.ensureReverseList()
			return fmt.Sprintf("_reverse_list %s", args[0]), nil
		}
	case "pow":
		if len(args) == 2 {
			return fmt.Sprintf("Float.pow %s %s", args[0], args[1]), nil
		}
	case "substr":
		if len(args) == 3 {
			c.ensureSubstr()
			return fmt.Sprintf("_substr %s %s %s", args[0], args[1], args[2]), nil
		}
	case "count":
		if len(args) == 1 {
			if id, ok := simpleIdent(call.Args[0]); ok {
				n := sanitizeName(id)
				if c.groupVars[n] {
					return fmt.Sprintf("List.length %s.items", n), nil
				}
			}
			return fmt.Sprintf("List.length %s", args[0]), nil
		}
	}
	name := sanitizeName(call.Func)
	if c.vars[name] {
		name = "!" + name
	}
	return fmt.Sprintf("%s %s", name, strings.Join(args, " ")), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	parts := make([]string, len(m.Cases))
	for i, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if pat == "_" {
			parts[i] = fmt.Sprintf("_ -> %s", res)
		} else {
			parts[i] = fmt.Sprintf("%s -> %s", pat, res)
		}
	}
	return fmt.Sprintf("(match %s with %s)", target, strings.Join(parts, " | ")), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "None"
	if l.Path != nil {
		path = fmt.Sprintf("(Some %q)", *l.Path)
	}
	opts := "None"
	if l.With != nil {
		o, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.ensureDataset()
	return fmt.Sprintf("_load %s %s", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "None"
	if s.Path != nil {
		path = fmt.Sprintf("(Some %q)", *s.Path)
	}
	opts := "None"
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.ensureDataset()
	return fmt.Sprintf("_save %s %s %s", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "None"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.ensureFetch()
	return fmt.Sprintf("_fetch %s %s", url, opts), nil
}

func (c *Compiler) compileFetchExprTyped(f *parser.FetchExpr, typ types.Type) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "None"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	st, ok := typ.(types.StructType)
	if !ok {
		return "", fmt.Errorf("unsupported fetch type")
	}
	c.compileStructType(st)
	c.ensureFetch()
	return fmt.Sprintf("decode_%s (_fetch %s %s)", sanitizeName(st.Name), url, opts), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	return "[]", nil
}

func (c *Compiler) compileGroupedQuery(q *parser.QueryExpr) (string, error) {
	orig := c.env
	child := types.NewEnv(orig)
	child.SetVar(q.Var, types.AnyType{}, true)
	c.env = child
	var condStr string
	var err error
	if q.Where != nil {
		condStr, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	keyExpr, err := c.compileExpr(q.Group.Exprs[0])
	if err != nil {
		c.env = orig
		return "", err
	}
	genv := types.NewEnv(child)
	genv.SetVar(q.Group.Name, types.AnyType{}, true)
	c.env = genv
	if c.groupVars == nil {
		c.groupVars = map[string]bool{}
	}
	c.groupVars[sanitizeName(q.Group.Name)] = true
	valExpr, err := c.compileExpr(q.Select)
	delete(c.groupVars, sanitizeName(q.Group.Name))
	c.env = orig
	if err != nil {
		return "", err
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	c.ensureGroupType()
	var b strings.Builder
	b.WriteString("(let tbl = Hashtbl.create 16 in\n")
	b.WriteString(fmt.Sprintf(" List.iter (fun %s ->\n", sanitizeName(q.Var)))
	if condStr != "" {
		b.WriteString(fmt.Sprintf("  if %s then (\n", condStr))
	}
	b.WriteString(fmt.Sprintf("    let key = %s in\n", keyExpr))
	b.WriteString("    let bucket = match Hashtbl.find_opt tbl key with\n")
	b.WriteString("      | Some b -> b\n      | None -> let r = ref [] in Hashtbl.add tbl key r; r in\n")
	b.WriteString(fmt.Sprintf("    bucket := %s :: !bucket;\n", sanitizeName(q.Var)))
	if condStr != "" {
		b.WriteString("  ) else ();\n")
	}
	b.WriteString(fmt.Sprintf(") %s);\n", src))
	b.WriteString("let res = ref [] in\n")
	b.WriteString(fmt.Sprintf("Hashtbl.iter (fun key bucket ->\n  let %s = { key = key; items = List.rev !bucket } in\n  res := %s :: !res\n) tbl;\n", sanitizeName(q.Group.Name), valExpr))
	b.WriteString("List.rev !res)")
	return b.String(), nil
}

func (c *Compiler) ocamlType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "float"
		case "string":
			return "string"
		case "bool":
			return "bool"
		default:
			if c.env != nil {
				if _, ok := c.env.GetStruct(*t.Simple); ok {
					return sanitizeName(*t.Simple)
				}
				if _, ok := c.env.GetUnion(*t.Simple); ok {
					return sanitizeName(*t.Simple)
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem := c.ocamlType(t.Generic.Args[0])
			if elem == "" {
				elem = "unit"
			}
			return fmt.Sprintf("%s list", elem)
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			key := c.ocamlType(t.Generic.Args[0])
			if key == "" {
				key = "unit"
			}
			val := c.ocamlType(t.Generic.Args[1])
			if val == "" {
				val = "unit"
			}
			return fmt.Sprintf("(%s, %s) Hashtbl.t", key, val)
		}
	}
	return ""
}

func (c *Compiler) ocamlTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "int"
	case types.FloatType:
		return "float"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		elem := c.ocamlTypeOf(tt.Elem)
		if elem == "" {
			elem = "unit"
		}
		return fmt.Sprintf("%s list", elem)
	case types.MapType:
		key := c.ocamlTypeOf(tt.Key)
		if key == "" {
			key = "unit"
		}
		val := c.ocamlTypeOf(tt.Value)
		if val == "" {
			val = "unit"
		}
		return fmt.Sprintf("(%s, %s) Hashtbl.t", key, val)
	case types.StructType:
		return sanitizeName(tt.Name)
	default:
		return ""
	}
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

func (c *Compiler) ocamlDecodeType(t types.Type, expr string) string {
	switch tt := t.(type) {
	case types.IntType:
		return fmt.Sprintf("Yojson.Basic.Util.to_int (%s)", expr)
	case types.FloatType:
		return fmt.Sprintf("Yojson.Basic.Util.to_float (%s)", expr)
	case types.StringType:
		return fmt.Sprintf("Yojson.Basic.Util.to_string (%s)", expr)
	case types.BoolType:
		return fmt.Sprintf("Yojson.Basic.Util.to_bool (%s)", expr)
	case types.StructType:
		name := sanitizeName(tt.Name)
		c.compileStructType(tt)
		return fmt.Sprintf("decode_%s (%s)", name, expr)
	default:
		return expr
	}
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	if c.structs == nil {
		c.structs = map[string]bool{}
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("type %s = {", name))
	c.indent++
	for i, fn := range st.Order {
		ft := st.Fields[fn]
		typStr := c.ocamlTypeOf(ft)
		if typStr == "" {
			typStr = "Yojson.Basic.t"
		}
		sep := ";"
		if i == len(st.Order)-1 {
			sep = ";"
		}
		c.writeln(fmt.Sprintf("%s: %s%s", sanitizeName(fn), typStr, sep))
	}
	c.indent--
	c.writeln("};;")
	c.writeln("")
	c.writeln(fmt.Sprintf("let decode_%s json = {", name))
	c.indent++
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		member := fmt.Sprintf("Yojson.Basic.Util.member \"%s\" json", fn)
		c.writeln(fmt.Sprintf("%s = %s;", sanitizeName(fn), c.ocamlDecodeType(ft, member)))
	}
	c.indent--
	c.writeln("};;")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Members) == 0 {
		return nil
	}
	fields := map[string]types.Type{}
	order := []string{}
	for _, m := range t.Members {
		if m.Field != nil {
			ft := c.resolveTypeRef(m.Field.Type)
			fields[m.Field.Name] = ft
			order = append(order, m.Field.Name)
		}
	}
	st := types.StructType{Name: t.Name, Fields: fields, Order: order}
	c.compileStructType(st)
	return nil
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
	res := b.String()
	if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') || (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	switch res {
	case "end", "type", "module", "let", "in", "match", "with", "and", "or":
		res = "_" + res
	}
	return res
}

func simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil {
		return false
	}
	return true
}

func fetchFromPrimary(p *parser.Primary) *parser.FetchExpr {
	if p == nil {
		return nil
	}
	if p.Fetch != nil {
		return p.Fetch
	}
	if p.Group != nil && p.Group.Binary != nil && len(p.Group.Binary.Right) == 0 {
		u := p.Group.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil {
			return fetchFromPrimary(u.Value.Target)
		}
	}
	return nil
}

func joinConds(conds []string) string {
	if len(conds) == 0 {
		return ""
	}
	if len(conds) == 1 {
		return conds[0]
	}
	return strings.Join(conds, " && ")
}

func SplitAndClauses(e *parser.Expr) []*parser.Expr {
	if e == nil {
		return nil
	}
	if len(e.Binary.Right) == 0 {
		return []*parser.Expr{e}
	}
	allAnd := true
	for _, op := range e.Binary.Right {
		if op.Op != "&&" {
			allAnd = false
			break
		}
	}
	if !allAnd {
		return []*parser.Expr{e}
	}
	clauses := []*parser.Expr{}
	cur := &parser.Expr{Binary: &parser.BinaryExpr{Left: e.Binary.Left, Right: nil}}
	for _, op := range e.Binary.Right {
		if op.Op == "&&" {
			clauses = append(clauses, cur)
			cur = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}, Right: nil}}
		} else {
			cur.Binary.Right = append(cur.Binary.Right, op)
		}
	}
	clauses = append(clauses, cur)
	return clauses
}

func exprVars(e *parser.Expr) map[string]bool {
	vars := map[string]bool{}
	var walkExpr func(*parser.Expr)
	var walkUnary func(*parser.Unary)
	var walkPost func(*parser.PostfixExpr)
	var walkPrimary func(*parser.Primary)
	walkExpr = func(ex *parser.Expr) {
		if ex == nil {
			return
		}
		if ex.Binary != nil {
			walkUnary(ex.Binary.Left)
			for _, op := range ex.Binary.Right {
				walkPost(op.Right)
			}
		}
	}
	walkUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		walkPost(u.Value)
	}
	walkPost = func(pf *parser.PostfixExpr) {
		if pf == nil {
			return
		}
		walkPrimary(pf.Target)
		for _, op := range pf.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					walkExpr(a)
				}
			}
			if op.Index != nil {
				walkExpr(op.Index.Start)
				walkExpr(op.Index.End)
				walkExpr(op.Index.Step)
			}
		}
	}
	walkPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		if p.Selector != nil {
			vars[p.Selector.Root] = true
		}
		if p.Group != nil {
			walkExpr(p.Group)
		}
		if p.List != nil {
			for _, e := range p.List.Elems {
				walkExpr(e)
			}
		}
		if p.Map != nil {
			for _, it := range p.Map.Items {
				walkExpr(it.Key)
				walkExpr(it.Value)
			}
		}
		if p.Struct != nil {
			for _, f := range p.Struct.Fields {
				walkExpr(f.Value)
			}
		}
		if p.Call != nil {
			for _, a := range p.Call.Args {
				walkExpr(a)
			}
		}
		if p.Query != nil {
			walkExpr(p.Query.Source)
			for _, f := range p.Query.Froms {
				walkExpr(f.Src)
			}
			if p.Query.Where != nil {
				walkExpr(p.Query.Where)
			}
		}
		if p.If != nil {
			walkExpr(p.If.Cond)
		}
		if p.Match != nil {
			walkExpr(p.Match.Target)
			for _, cs := range p.Match.Cases {
				walkExpr(cs.Pattern)
				walkExpr(cs.Result)
			}
		}
	}
	walkExpr(e)
	return vars
}
func (c *Compiler) ensureFetch() {
	if c.fetch {
		return
	}
	c.fetch = true
	c.ensureDataset()
	b := &c.pre
	b.WriteString("let _fetch url opts =\n")
	b.WriteString("  let is_file =\n")
	b.WriteString("    (String.length url >= 7 && String.sub url 0 7 = \"file://\") || not (String.contains url ':') in\n")
	b.WriteString("  if is_file then (\n")
	b.WriteString("    let path = if String.length url >= 7 && String.sub url 0 7 = \"file://\" then String.sub url 7 (String.length url - 7) else url in\n")
	b.WriteString("    let txt = _read_input (Some path) in\n")
	b.WriteString("    Yojson.Basic.from_string txt) else (\n")
	b.WriteString("    let method_ =\n")
	b.WriteString("      match opts with\n")
	b.WriteString("      | None -> \"GET\"\n")
	b.WriteString("      | Some tbl ->\n")
	b.WriteString("          (try Yojson.Basic.Util.to_string (Hashtbl.find tbl \"method\") with Not_found -> \"GET\") in\n")
	b.WriteString("    let url_ref = ref url in\n")
	b.WriteString("    let args = ref [\"curl\"; \"-s\"; \"-X\"; method_] in\n")
	b.WriteString("    (match opts with\n")
	b.WriteString("    | Some tbl ->\n")
	b.WriteString("        (try\n")
	b.WriteString("          let headers = Yojson.Basic.Util.to_assoc (Hashtbl.find tbl \"headers\") in\n")
	b.WriteString("          List.iter (fun (k,v) -> args := !args @ [\"-H\"; k ^ \": \" ^ Yojson.Basic.to_string v]) headers\n")
	b.WriteString("        with Not_found -> ());\n")
	b.WriteString("        (try\n")
	b.WriteString("          let q = Yojson.Basic.Util.to_assoc (Hashtbl.find tbl \"query\") in\n")
	b.WriteString("          let qs = String.concat \"&\" (List.map (fun (k,v) -> k ^ \"=\" ^ Yojson.Basic.to_string v) q) in\n")
	b.WriteString("          let sep = if String.contains !url_ref '?' then '&' else '?' in\n")
	b.WriteString("          url_ref := !url_ref ^ (String.make 1 sep) ^ qs\n")
	b.WriteString("        with Not_found -> ());\n")
	b.WriteString("        (try\n")
	b.WriteString("          let body = Hashtbl.find tbl \"body\" in\n")
	b.WriteString("          args := !args @ [\"-d\"; Yojson.Basic.to_string body]\n")
	b.WriteString("        with Not_found -> ());\n")
	b.WriteString("        (try\n")
	b.WriteString("          let t = Hashtbl.find tbl \"timeout\" in\n")
	b.WriteString("          args := !args @ [\"--max-time\"; Yojson.Basic.to_string t]\n")
	b.WriteString("        with Not_found -> ());\n")
	b.WriteString("    | None -> ());\n")
	b.WriteString("    args := !args @ [!url_ref];\n")
	b.WriteString("    let ic = Unix.open_process_in (String.concat \" \" !args) in\n")
	b.WriteString("    let txt = _read_all ic in\n")
	b.WriteString("    close_in ic;\n")
	b.WriteString("    Yojson.Basic.from_string txt) ;;\n\n")
}
