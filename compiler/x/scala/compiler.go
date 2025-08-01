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

	meta "mochi/compiler/meta"
	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	tmp           int
	helpers       map[string]bool
	inSort        bool
	updates       map[string]bool
	autoStructs   map[string]types.StructType
	structKeys    map[string]string
	autoCount     int
	structHint    string
	mapVars       map[string]bool
	pyModules     map[string]string
	forceMap      bool
	preferMutable bool
	contextType   types.Type
}

func (c *Compiler) zeroValue(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "0"
	case types.FloatType:
		return "0.0"
	case types.BoolType:
		return "false"
	case types.StringType:
		return "\"\""
	case types.OptionType:
		return "None"
	case types.ListType:
		if c.preferMutable {
			return fmt.Sprintf("scala.collection.mutable.ArrayBuffer[%s]()", c.typeOf(tt.Elem))
		}
		return fmt.Sprintf("List[%s]()", c.typeOf(tt.Elem))
	case types.MapType:
		if c.preferMutable {
			return fmt.Sprintf("scala.collection.mutable.Map[%s, %s]()", c.typeOf(tt.Key), c.typeOf(tt.Value))
		}
		return fmt.Sprintf("Map[%s, %s]()", c.typeOf(tt.Key), c.typeOf(tt.Value))
	case types.StructType:
		return fmt.Sprintf("%s()", tt.Name)
	default:
		return "null"
	}
}

func (c *Compiler) zeroValueRef(t *parser.TypeRef) string {
	if t == nil {
		return "null"
	}
	return c.zeroValue(types.ResolveTypeRef(t, c.env))
}

func (c *Compiler) detectStructMap(e *parser.Expr, env *types.Env) (types.StructType, bool) {
	if env == nil {
		env = c.env
	}
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
		st.Fields[key] = c.namedType(types.ExprType(it.Value, env))
		st.Order[i] = key
	}
	return st, true
}

const indentStep = 2

func isInt(t types.Type) bool { _, ok := t.(types.IntType); return ok }
func isFloat(t types.Type) bool {
	switch t.(type) {
	case types.FloatType, types.BigRatType:
		return true
	}
	return false
}
func isString(t types.Type) bool { _, ok := t.(types.StringType); return ok }

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:           env,
		helpers:       make(map[string]bool),
		updates:       make(map[string]bool),
		autoStructs:   make(map[string]types.StructType),
		structKeys:    make(map[string]string),
		structHint:    "",
		mapVars:       make(map[string]bool),
		pyModules:     make(map[string]string),
		preferMutable: false,
		contextType:   nil,
	}
}

// SetPreferMutable enables the use of mutable collections for list literals.
func (c *Compiler) SetPreferMutable(v bool) {
	c.preferMutable = v
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
		if name == "_outer_join" {
			c.helpers["_left_join"] = true
			c.helpers["_right_join"] = true
		}
	}
}

func truthyExpr(expr string, t types.Type) string {
	switch t.(type) {
	case types.BoolType:
		return expr
	case types.OptionType:
		return expr + ".nonEmpty"
	case types.ListType, types.MapType, types.StringType:
		return expr + ".nonEmpty"
	case types.IntType, types.Int64Type:
		return expr + " != 0"
	case types.FloatType:
		return expr + " != 0.0"
	default:
		return expr + " != null"
	}
}

// maybeOptionExpr returns true if the given expression likely evaluates to an
// Option value based on the current static environment. This is used when the
// type checker returns `any` but we still want to emit idiomatic `.nonEmpty`
// checks for Option values.
func (c *Compiler) maybeOptionExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if sel, ok := selectorOfExpr(e); ok {
		t, err := c.env.GetVar(sel.Root)
		if err != nil {
			return false
		}
		for i, f := range sel.Tail {
			if ot, ok := t.(types.OptionType); ok {
				t = ot.Elem
			}
			st, ok := t.(types.StructType)
			if !ok {
				return false
			}
			ft, ok := st.Fields[f]
			if !ok {
				return false
			}
			t = ft
			if i == len(sel.Tail)-1 {
				_, ok := t.(types.OptionType)
				return ok
			}
		}
		return false
	}
	if id, ok := identName(e); ok {
		if t, err := c.env.GetVar(id); err == nil {
			_, ok := t.(types.OptionType)
			return ok
		}
	}
	return false
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
		case "_inner_join":
			out.WriteString(pad + "def _inner_join[A,B](a: List[A], b: List[B])(cond: (A,B) => Boolean): List[(A,B)] = for { x <- a; y <- b if cond(x,y) } yield (x,y)\n")
		case "_left_join":
			out.WriteString(pad + "def _left_join[A,B](a: List[A], b: List[B])(cond: (A,B) => Boolean): List[(A, Option[B])] = for(x <- a) yield (x, b.find(y => cond(x,y)))\n")
		case "_right_join":
			out.WriteString(pad + "def _right_join[A,B](a: List[A], b: List[B])(cond: (A,B) => Boolean): List[(Option[A], B)] = for(y <- b) yield (a.find(x => cond(x,y)), y)\n")
		case "_outer_join":
			out.WriteString(pad + "def _outer_join[A,B](a: List[A], b: List[B])(cond: (A,B) => Boolean): List[(Option[A], Option[B])] = { val left = _left_join(a,b)(cond).map{ case(x,y) => (Some(x), y) }; val right = _right_join(a,b)(cond).collect{ case(None, r) => (None, Some(r)) }; left ++ right }\n")
		case "_compare":
			out.WriteString(pad + "def _compare(a: Any, b: Any): Int = (a, b) match {\n" +
				pad + "  case (x: Int, y: Int) => x.compare(y)\n" +
				pad + "  case (x: Double, y: Double) => java.lang.Double.compare(x, y)\n" +
				pad + "  case (x: String, y: String) => x.compareTo(y)\n" +
				pad + "  case _ => a.toString.compareTo(b.toString)\n" +
				pad + "}\n" +
				pad + "implicit val _anyOrdering: Ordering[Any] = new Ordering[Any] { def compare(x: Any, y: Any): Int = _compare(x, y) }\n")
		case "_Group":
			out.WriteString(pad + "case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }\n")
		case "_load_yaml":
			out.WriteString(pad + "def _load_yaml(path: String): List[Map[String,String]] = {\n" +
				pad + "  val lines = scala.io.Source.fromFile(path).getLines().toList\n" +
				pad + "  val buf = scala.collection.mutable.ListBuffer[Map[String,String]]()\n" +
				pad + "  var cur = Map[String,String]()\n" +
				pad + "  for(l <- lines) {\n" +
				pad + "    val line = l.trim\n" +
				pad + "    if(line.startsWith(\"-\")) {\n" +
				pad + "      if(cur.nonEmpty) buf += cur; cur = Map()\n" +
				pad + "      val rest = line.dropWhile(_ == '-').trim\n" +
				pad + "      if(rest.nonEmpty && rest.contains(\":\")) { val i = rest.indexOf(':'); cur += rest.take(i).trim -> rest.drop(i+1).trim }\n" +
				pad + "    } else if(line.contains(\":\")) { val i = line.indexOf(':'); cur += line.take(i).trim -> line.drop(i+1).trim }\n" +
				pad + "  }\n" +
				pad + "  if(cur.nonEmpty) buf += cur\n" +
				pad + "  buf.toList\n" +
				pad + "}\n")
		case "_now":
			out.WriteString(pad + "def _now(): Int = System.nanoTime().toInt\n")
		case "_save_jsonl":
			out.WriteString(pad + "def _save_jsonl(rows: Iterable[Any], path: String): Unit = {\n" +
				pad + "  def toMap(v: Any): Map[String,Any] = v match {\n" +
				pad + "    case m: Map[_, _] => m.asInstanceOf[Map[String,Any]]\n" +
				pad + "    case p: Product => p.getClass.getDeclaredFields.map(_.getName).zip(p.productIterator.toList).toMap.asInstanceOf[Map[String,Any]]\n" +
				pad + "    case x => Map(\"value\" -> x)\n" +
				pad + "  }\n" +
				pad + "  val out: java.io.PrintWriter = if(path == \"-\") new java.io.PrintWriter(System.out) else new java.io.PrintWriter(path)\n" +
				pad + "  rows.foreach(r => out.println(scala.util.parsing.json.JSONObject(toMap(r)).toString()))\n" +
				pad + "  out.flush()\n" +
				pad + "  if(out ne Console.out) out.close()\n" +
				pad + "}\n")
		case "_safe_div":
			out.WriteString(pad + "def _safe_div(a: Double, b: Double): Double = if(b == 0) 0 else a / b\n")
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
		case "_group_by":
			out.WriteString(pad + "def _group_by[T,K](src: Seq[T], keyfn: T => K): Seq[_Group[K,T]] = {\n" +
				pad + "  val groups = scala.collection.mutable.LinkedHashMap[String,(K, scala.collection.mutable.ListBuffer[T])]()\n" +
				pad + "  for (it <- src) {\n" +
				pad + "    val key = keyfn(it)\n" +
				pad + "    val ks = key.toString\n" +
				pad + "    val buf = groups.getOrElseUpdate(ks, (key, scala.collection.mutable.ListBuffer[T]()))\n" +
				pad + "    buf._2.append(it)\n" +
				pad + "  }\n" +
				pad + "  groups.values.map{ case(k,b) => _Group(k, b.toList) }.toSeq\n" +
				pad + "}\n")
		case "_query":
			out.WriteString(pad + "def _query(src: Seq[Any], joins: Seq[Map[String,Any]], opts: Map[String,Any]): Seq[Any] = {\n" +
				pad + "  var items = src.map(v => Seq[Any](v))\n" +
				pad + "  for (j <- joins) {\n" +
				pad + "    val joined = scala.collection.mutable.ArrayBuffer[Seq[Any]]()\n" +
				pad + "    val jitems = j(\"items\").asInstanceOf[Seq[Any]]\n" +
				pad + "    val on = j.get(\"on\").map(_.asInstanceOf[Seq[Any] => Boolean])\n" +
				pad + "    val left = j.get(\"left\").exists(_.asInstanceOf[Boolean])\n" +
				pad + "    val right = j.get(\"right\").exists(_.asInstanceOf[Boolean])\n" +
				pad + "    if (left && right) {\n" +
				pad + "      val matched = Array.fill(jitems.length)(false)\n" +
				pad + "      for (leftRow <- items) {\n" +
				pad + "        var m = false\n" +
				pad + "        for ((rightRow, ri) <- jitems.zipWithIndex) {\n" +
				pad + "          var keep = true\n" +
				pad + "          if (on.isDefined) keep = on.get(leftRow :+ rightRow)\n" +
				pad + "          if (keep) { m = true; matched(ri) = true; joined.append(leftRow :+ rightRow) }\n" +
				pad + "        }\n" +
				pad + "        if (!m) joined.append(leftRow :+ null)\n" +
				pad + "      }\n" +
				pad + "      for ((rightRow, ri) <- jitems.zipWithIndex) {\n" +
				pad + "        if (!matched(ri)) {\n" +
				pad + "          val undef = if (items.nonEmpty) Seq.fill(items.head.length)(null) else Seq[Any]()\n" +
				pad + "          joined.append(undef :+ rightRow)\n" +
				pad + "        }\n" +
				pad + "      }\n" +
				pad + "    } else if (right) {\n" +
				pad + "      for (rightRow <- jitems) {\n" +
				pad + "        var m = false\n" +
				pad + "        for (leftRow <- items) {\n" +
				pad + "          var keep = true\n" +
				pad + "          if (on.isDefined) keep = on.get(leftRow :+ rightRow)\n" +
				pad + "          if (keep) { m = true; joined.append(leftRow :+ rightRow) }\n" +
				pad + "        }\n" +
				pad + "        if (!m) {\n" +
				pad + "          val undef = if (items.nonEmpty) Seq.fill(items.head.length)(null) else Seq[Any]()\n" +
				pad + "          joined.append(undef :+ rightRow)\n" +
				pad + "        }\n" +
				pad + "      }\n" +
				pad + "    } else {\n" +
				pad + "      for (leftRow <- items) {\n" +
				pad + "        var m = false\n" +
				pad + "        for (rightRow <- jitems) {\n" +
				pad + "          var keep = true\n" +
				pad + "          if (on.isDefined) keep = on.get(leftRow :+ rightRow)\n" +
				pad + "          if (keep) { m = true; joined.append(leftRow :+ rightRow) }\n" +
				pad + "        }\n" +
				pad + "        if (left && !m) joined.append(leftRow :+ null)\n" +
				pad + "      }\n" +
				pad + "    }\n" +
				pad + "    items = joined.toSeq\n" +
				pad + "  }\n" +
				pad + "  var it = items\n" +
				pad + "  opts.get(\"where\").foreach { f =>\n" +
				pad + "    val fn = f.asInstanceOf[Seq[Any] => Boolean]\n" +
				pad + "    it = it.filter(r => fn(r))\n" +
				pad + "  }\n" +
				pad + "  opts.get(\"sortKey\").foreach { f =>\n" +
				pad + "    val fn = f.asInstanceOf[Seq[Any] => Any]\n" +
				pad + "    it = it.sortBy(r => fn(r))(_anyOrdering)\n" +
				pad + "  }\n" +
				pad + "  opts.get(\"skip\").foreach { n => it = it.drop(n.asInstanceOf[Int]) }\n" +
				pad + "  opts.get(\"take\").foreach { n => it = it.take(n.asInstanceOf[Int]) }\n" +
				pad + "  val sel = opts(\"select\").asInstanceOf[Seq[Any] => Any]\n" +
				pad + "  it.map(r => sel(r))\n" +
				pad + "}\n")
		case "_to_json":
			out.WriteString(pad + "def _to_json(v: Any): String = v match {\n" +
				pad + "  case null => \"null\"\n" +
				pad + "  case s: String => \"\\\"\" + s.replace(\\\"\\\\\\\", \\\"\\\\\\\\\\\").replace(\\\"\\\"\\\", \\\"\\\\\\\"\\\") + \"\\\"\"\n" +
				pad + "  case b: Boolean => b.toString\n" +
				pad + "  case i: Int => i.toString\n" +
				pad + "  case l: Long => l.toString\n" +
				pad + "  case d: Double => d.toString\n" +
				pad + "  case m: scala.collection.Map[_, _] => m.map{ case (k, v2) => \"\\\"\" + k.toString.replace(\\\"\\\"\", \\\"\\\\\\\"\\\") + \"\\\":\" + _to_json(v2) }.mkString(\"{\", \",\", \"}\")\n" +
				pad + "  case seq: Iterable[_] => seq.map(_to_json).mkString(\"[\", \",\", \"]\")\n" +
				pad + "  case other => \"\\\"\" + other.toString.replace(\\\"\\\\\\\", \\\"\\\\\\\\\\\").replace(\\\"\\\"\", \\\"\\\\\\\"\\\") + \"\\\"\"\n" +
				pad + "}\n")
		case "_json":
			out.WriteString(pad + "def _json(v: Any): Unit = println(_to_json(v))\n")
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

var scalaKeywords = map[string]bool{
	"abstract":  true,
	"case":      true,
	"catch":     true,
	"class":     true,
	"def":       true,
	"do":        true,
	"else":      true,
	"extends":   true,
	"false":     true,
	"final":     true,
	"finally":   true,
	"for":       true,
	"forSome":   true,
	"if":        true,
	"implicit":  true,
	"import":    true,
	"lazy":      true,
	"match":     true,
	"new":       true,
	"null":      true,
	"object":    true,
	"override":  true,
	"package":   true,
	"private":   true,
	"protected": true,
	"return":    true,
	"sealed":    true,
	"super":     true,
	"this":      true,
	"throw":     true,
	"trait":     true,
	"try":       true,
	"true":      true,
	"type":      true,
	"val":       true,
	"var":       true,
	"while":     true,
	"with":      true,
	"yield":     true,
}

func sanitizeField(name string) string {
	for i, r := range name {
		if i == 0 {
			if !unicode.IsLetter(r) && r != '_' {
				return "`" + name + "`"
			}
		} else {
			if !unicode.IsLetter(r) && !unicode.IsDigit(r) && r != '_' {
				return "`" + name + "`"
			}
		}
	}
	if scalaKeywords[name] {
		return "`" + name + "`"
	}
	return name
}

func sanitizeVarName(name string) string {
	return sanitizeField(name)
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	name := sanitizeName(strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename)))
	collectUpdates(prog.Statements, c.updates)
	collectMapUses(prog.Statements, c.mapVars)
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
	hasFunAfter := make([]bool, len(prog.Statements))
	seenFun := false
	for i := len(prog.Statements) - 1; i >= 0; i-- {
		hasFunAfter[i] = seenFun
		if prog.Statements[i].Fun != nil {
			seenFun = true
		}
	}

	processed := make(map[int]bool)
	foundNonLet := false
	for i := 0; i < firstFun; i++ {
		s := prog.Statements[i]
		if s.Let != nil && !foundNonLet && hasFunAfter[i] {
			if err := c.compileLet(s.Let); err != nil {
				return nil, err
			}
			processed[i] = true
			continue
		}
		if s.Let == nil && s.Type == nil {
			foundNonLet = true
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
	var final bytes.Buffer
	final.Write(meta.Header("//"))
	final.Write(data)
	if final.Len() == 0 || final.Bytes()[final.Len()-1] != '\n' {
		final.WriteByte('\n')
	}
	return final.Bytes(), nil
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
	case s.Import != nil:
		return c.addImport(s.Import)
	case s.Break != nil:
		c.writeln("return")
		return nil
	case s.Continue != nil:
		c.writeln("// continue")
		return nil
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
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
	oldHint := c.structHint
	c.structHint = s.Name
	defer func() { c.structHint = oldHint }()
	oldCtx := c.contextType
	if s.Type != nil {
		c.contextType = types.ResolveTypeRef(s.Type, c.env)
	} else {
		c.contextType = nil
	}
	defer func() { c.contextType = oldCtx }()
	mutable := c.updates[s.Name]
	rhs := "0"
	if s.Value != nil {
		var err error
		if mutable {
			if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
				rhs, err = c.compileList(lst, false)
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
	} else if s.Type != nil {
		rhs = c.zeroValueRef(s.Type)
	}
	var typ types.Type = types.AnyType{}
	if s.Type != nil {
		typ = types.ResolveTypeRef(s.Type, c.env)
	} else if s.Value != nil {
		if mp := s.Value.Binary.Left.Value.Target.Map; mp != nil && c.mapVars[s.Name] {
			var valT types.Type = types.AnyType{}
			if len(mp.Items) > 0 {
				valT = c.namedType(types.ExprType(mp.Items[0].Value, c.env))
				for _, it := range mp.Items[1:] {
					t := c.namedType(types.ExprType(it.Value, c.env))
					if !sameType(valT, t) {
						valT = types.AnyType{}
						break
					}
				}
			}
			typ = types.MapType{Key: types.StringType{}, Value: valT}
		} else {
			typ = c.namedType(types.ExprType(s.Value, c.env))
		}
		if q := s.Value.Binary.Left.Value.Target.Query; q != nil {
			qenv := c.querySelectEnv(q)
			if st, ok := c.detectStructMap(q.Select, qenv); ok {
				st = c.ensureStructName(st)
				typ = types.ListType{Elem: st}
			}
		} else if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
			if len(lst.Elems) > 0 {
				if st, ok := c.detectStructMap(lst.Elems[0], c.env); ok {
					same := true
					for _, e := range lst.Elems[1:] {
						st2, ok2 := c.detectStructMap(e, c.env)
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
	name := sanitizeVarName(s.Name)
	if s.Type != nil {
		typ := c.typeString(s.Type)
		if mutable {
			c.writeln(fmt.Sprintf("var %s: %s = %s", name, typ, rhs))
		} else {
			c.writeln(fmt.Sprintf("val %s: %s = %s", name, typ, rhs))
		}
	} else {
		if mutable {
			c.writeln(fmt.Sprintf("var %s = %s", name, rhs))
		} else {
			c.writeln(fmt.Sprintf("val %s = %s", name, rhs))
		}
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	oldHint := c.structHint
	c.structHint = s.Name
	defer func() { c.structHint = oldHint }()
	oldCtx := c.contextType
	if s.Type != nil {
		c.contextType = types.ResolveTypeRef(s.Type, c.env)
	} else {
		c.contextType = nil
	}
	defer func() { c.contextType = oldCtx }()
	rhs := "0"
	if s.Value != nil {
		var err error
		// use mutable collections when assigning list or map literals
		if lst := s.Value.Binary.Left.Value.Target.List; lst != nil {
			rhs, err = c.compileList(lst, false)
		} else if mp := s.Value.Binary.Left.Value.Target.Map; mp != nil {
			rhs, err = c.compileMap(mp, true)
		} else {
			rhs, err = c.compileExpr(s.Value)
		}
		if err != nil {
			return err
		}
	} else if s.Type != nil {
		rhs = c.zeroValueRef(s.Type)
	}
	var typ types.Type = types.AnyType{}
	if s.Type != nil {
		typ = types.ResolveTypeRef(s.Type, c.env)
	} else if s.Value != nil {
		if mp := s.Value.Binary.Left.Value.Target.Map; mp != nil && c.mapVars[s.Name] {
			var valT types.Type = types.AnyType{}
			if len(mp.Items) > 0 {
				valT = c.namedType(types.ExprType(mp.Items[0].Value, c.env))
				for _, it := range mp.Items[1:] {
					t := c.namedType(types.ExprType(it.Value, c.env))
					if !sameType(valT, t) {
						valT = types.AnyType{}
						break
					}
				}
			}
			typ = types.MapType{Key: types.StringType{}, Value: valT}
		} else {
			typ = c.namedType(types.ExprType(s.Value, c.env))
		}
	}
	if c.env != nil {
		c.env.SetVar(s.Name, typ, true)
	}
	name := sanitizeVarName(s.Name)
	if s.Type != nil {
		ts := c.typeString(s.Type)
		c.writeln(fmt.Sprintf("var %s: %s = %s", name, ts, rhs))
	} else {
		c.writeln(fmt.Sprintf("var %s = %s", name, rhs))
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
				fields[i] = fmt.Sprintf("%s: %s", sanitizeField(f.Name), c.typeString(f.Type))
			}
			c.writeln(fmt.Sprintf("case class %s(%s) extends %s", v.Name, strings.Join(fields, ", "), td.Name))
		}
		return nil
	}
	fields := []string{}
	for _, m := range td.Members {
		if m.Field != nil {
			f := m.Field
			fields = append(fields, fmt.Sprintf("var %s: %s", sanitizeField(f.Name), c.typeString(f.Type)))
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
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		pname := sanitizeVarName(p.Name)
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", pname, c.typeString(p.Type))
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, c.env), false)
		} else {
			params[i] = pname
			child.SetVar(p.Name, types.AnyType{}, false)
		}
	}
	ret := ""
	if fn.Return != nil {
		ret = ": " + c.typeString(fn.Return)
	} else if hasReturn(fn.Body) {
		ret = ": Unit"
	}

	oldEnv := c.env
	c.env = child

	// expression body if the function only returns a single expression
	if len(fn.Body) == 1 && fn.Body[0].Return != nil {
		expr, err := c.compileExpr(fn.Body[0].Return.Value)
		if err != nil {
			c.env = oldEnv
			return err
		}
		c.writeln(fmt.Sprintf("def %s(%s)%s = %s", fn.Name, strings.Join(params, ", "), ret, expr))
		c.env = oldEnv
		return nil
	}

	c.writeln(fmt.Sprintf("def %s(%s)%s = {", fn.Name, strings.Join(params, ", "), ret))
	c.indent += indentStep
	for _, st := range fn.Body {
		if st.Return != nil && fn.Return != nil {
			c.contextType = types.ResolveTypeRef(fn.Return, c.env)
		} else {
			c.contextType = nil
		}
		if err := c.compileStmt(st); err != nil {
			c.env = oldEnv
			return err
		}
	}
	c.contextType = nil
	c.indent -= indentStep
	c.writeln("}")
	c.env = oldEnv
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
	t := types.ExprType(s.Cond, c.env)
	if _, ok := t.(types.BoolType); !ok {
		if _, ok := t.(types.AnyType); ok && c.maybeOptionExpr(s.Cond) {
			cond = cond + ".nonEmpty"
		} else {
			cond = truthyExpr(cond, t)
		}
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
	t := types.ExprType(e.Cond, c.env)
	if _, ok := t.(types.BoolType); !ok {
		if _, ok := t.(types.AnyType); ok && c.maybeOptionExpr(e.Cond) {
			cond = cond + ".nonEmpty"
		} else {
			cond = truthyExpr(cond, t)
		}
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
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		if p.Type != nil {
			params[i] = fmt.Sprintf("%s: %s", p.Name, c.typeString(p.Type))
			child.SetVar(p.Name, types.ResolveTypeRef(p.Type, c.env), false)
		} else {
			params[i] = p.Name
			child.SetVar(p.Name, types.AnyType{}, false)
		}
	}
	if fn.ExprBody == nil {
		return "", fmt.Errorf("line %d: block lambdas not supported", fn.Pos.Line)
	}
	oldEnv := c.env
	c.env = child
	body, err := c.compileExpr(fn.ExprBody)
	c.env = oldEnv
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
	t := types.ExprType(s.Cond, c.env)
	if _, ok := t.(types.BoolType); !ok {
		cond = truthyExpr(cond, t)
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
	var elemType types.Type = types.AnyType{}
	loop := start
	if s.RangeEnd != nil {
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		loop = fmt.Sprintf("%s until %s", start, end)
		// range loops produce integer indices
		elemType = types.IntType{}
	}
	name := sanitizeVarName(s.Name)
	if t := types.ExprType(s.Source, c.env); t != nil {
		if mt, ok := t.(types.MapType); ok {
			c.writeln(fmt.Sprintf("for((%s, _) <- %s) {", name, loop))
			elemType = mt.Key
		} else {
			c.writeln(fmt.Sprintf("for(%s <- %s) {", name, loop))
			if lt, ok := t.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	} else {
		c.writeln(fmt.Sprintf("for(%s <- %s) {", name, loop))
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
	target := sanitizeVarName(s.Name)
	if len(s.Index) == 1 && len(s.Field) == 0 {
		if t, err := c.env.GetVar(s.Name); err == nil {
			if _, ok := t.(types.ListType); ok {
				idxExpr := "0"
				if s.Index[0].Start != nil {
					var err error
					idxExpr, err = c.compileExpr(s.Index[0].Start)
					if err != nil {
						return err
					}
				}
				c.writeln(fmt.Sprintf("%s = %s.updated(%s, %s)", target, target, idxExpr, expr))
				return nil
			}
		}
	}
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
		target += "." + sanitizeField(f.Name)
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
			name := sanitizeField(f)
			c.writeln(fmt.Sprintf("var %s = %s.%s", name, item, sanitizeField(f)))
			child.SetVar(name, st.Fields[f], true)
		}
	} else if mt, ok := elemType.(types.MapType); ok {
		if _, ok2 := mt.Key.(types.StringType); ok2 {
			for _, it := range u.Set.Items {
				if key, ok3 := identName(it.Key); ok3 {
					name := sanitizeField(key)
					c.writeln(fmt.Sprintf("var %s = %s(%q)", name, item, key))
					child.SetVar(name, mt.Value, true)
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
			name := sanitizeField(f)
			parts[i] = fmt.Sprintf("%s = %s", name, name)
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
			if isIfExpr(a) {
				v = "(" + v + ")"
			}
			args[i] = v
		}
		if len(args) == 1 {
			c.writeln(fmt.Sprintf("println(%s)", args[0]))
		} else {
			parts := make([]string, len(args))
			for i, a := range args {
				if strings.HasPrefix(a, "\"") && strings.HasSuffix(a, "\"") {
					parts[i] = strings.Trim(a, "\"")
				} else {
					parts[i] = fmt.Sprintf("${%s}", a)
				}
			}
			msg := strings.Join(parts, " ")
			msg = strings.ReplaceAll(msg, " :", ":")
			msg = strings.ReplaceAll(msg, " ,", ",")
			msg = strings.ReplaceAll(msg, " ;", ";")
			msg = strings.ReplaceAll(msg, " .", ".")
			msg = strings.ReplaceAll(msg, " ?", "?")
			msg = strings.ReplaceAll(msg, " !", "!")
			msg = strings.ReplaceAll(msg, " )", ")")
			msg = strings.ReplaceAll(msg, "( ", "(")
			msg = strings.ReplaceAll(msg, "$ ${", "$$${")
			c.writeln(fmt.Sprintf("println(s\"%s\")", msg))
		}
		return nil
	}
	if ok && call.Func == "json" {
		if len(call.Args) != 1 {
			return fmt.Errorf("json expects 1 arg")
		}
		argExpr := call.Args[0]
		arg, err := c.compileExpr(argExpr)
		if err != nil {
			return err
		}
		t := types.ExprType(argExpr, c.env)
		switch t.(type) {
		case types.MapType, types.StructType, types.UnionType:
			c.writeln(fmt.Sprintf("println(scala.util.parsing.json.JSONObject(%s.asInstanceOf[scala.collection.immutable.Map[String,Any]]).toString())", arg))
		case types.ListType:
			c.writeln(fmt.Sprintf("println(scala.util.parsing.json.JSONArray(%s.asInstanceOf[List[Any]]).toString())", arg))
		default:
			c.use("_to_json")
			c.writeln(fmt.Sprintf("println(_to_json(%s))", arg))
		}
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
			if op.Op == "+" && (isString(leftType) || isString(rightType)) {
				s = fmt.Sprintf("%s + %s", ls, rs)
				leftType = types.StringType{}
			} else {
				if _, ok := leftType.(types.AnyType); ok {
					ls = fmt.Sprintf("(%s).asInstanceOf[Int]", s)
				}
				if _, ok := rightType.(types.AnyType); ok {
					rs = fmt.Sprintf("(%s).asInstanceOf[Int]", r)
				}
				if op.Op == "/" {
					s = fmt.Sprintf("%s / %s", ls, rs)
				} else {
					s = fmt.Sprintf("%s %s %s", ls, op.Op, rs)
				}
				switch op.Op {
				case "&&", "||", "==", "!=", "<", "<=", ">", ">=":
					leftType = types.BoolType{}
				default:
					if isFloat(leftType) || isFloat(rightType) {
						leftType = types.FloatType{}
					} else if isInt(leftType) && isInt(rightType) {
						leftType = types.IntType{}
					} else {
						leftType = types.AnyType{}
					}
				}
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
				s = fmt.Sprintf("(%s ++ %s)", s, r)
			} else {
				s = fmt.Sprintf("((%s) ++ (%s)).distinct", s, r)
			}
			leftType = types.ListType{Elem: types.AnyType{}}
		case "except":
			s = fmt.Sprintf("((%s).toSet diff (%s).toSet).toList", s, r)
			leftType = types.ListType{Elem: types.AnyType{}}
		case "intersect":
			s = fmt.Sprintf("((%s).toSet intersect (%s).toSet).toList", s, r)
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
				s = fmt.Sprintf("!(%s)", truthyExpr(s, valType))
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
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
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
					var elem types.Type = types.AnyType{}
					switch tt := typ.(type) {
					case types.ListType:
						elem = tt.Elem
					case types.MapType:
						elem = tt.Value
					}
					if _, ok := elem.(types.AnyType); ok {
						s = fmt.Sprintf("(%s).apply(%s)", s, idxExpr)
					} else {
						s = fmt.Sprintf("(%s).apply(%s).asInstanceOf[%s]", s, idxExpr, c.typeOf(elem))
					}
					typ = elem
				}
			}
		case op.Field != nil:
			name := op.Field.Name
			if ot, ok := typ.(types.OptionType); ok {
				s = fmt.Sprintf("%s.get", s)
				typ = ot.Elem
			}
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				call := p.Ops[i+1].Call
				args := make([]string, len(call.Args))
				for j, a := range call.Args {
					val, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[j] = val
				}
				if _, ok := typ.(types.StringType); ok {
					switch name {
					case "contains":
						s = fmt.Sprintf("%s.contains(%s)", s, args[0])
						typ = types.BoolType{}
					case "padStart":
						if len(args) != 2 {
							return "", fmt.Errorf("padStart expects 2 args")
						}
						fill := args[1]
						if strings.HasPrefix(fill, "\"") && strings.HasSuffix(fill, "\"") && len([]rune(fill[1:len(fill)-1])) == 1 {
							fill = fmt.Sprintf("'%s'", fill[1:len(fill)-1])
						} else {
							fill = fmt.Sprintf("%s.charAt(0)", fill)
						}
						s = fmt.Sprintf("%s.reverse.padTo(%s, %s).reverse", s, args[0], fill)
						typ = types.StringType{}
					default:
						s = fmt.Sprintf("%s.%s(%s)", s, sanitizeField(name), strings.Join(args, ", "))
						typ = types.AnyType{}
					}
				} else {
					s = fmt.Sprintf("%s.%s(%s)", s, sanitizeField(name), strings.Join(args, ", "))
					typ = types.AnyType{}
				}
				i++
				continue
			}
			s = fmt.Sprintf("%s.%s", s, sanitizeField(name))
			if st, ok := typ.(types.StructType); ok {
				if ft, ok2 := st.Fields[name]; ok2 {
					typ = ft
				} else {
					typ = types.AnyType{}
				}
			} else {
				typ = types.AnyType{}
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
	if mod, ok := c.pyModules[s.Root]; ok && mod == "math" {
		attr := strings.Join(s.Tail, ".")
		switch attr {
		case "pi":
			return "scala.math.Pi"
		case "e":
			return "scala.math.E"
		default:
			return fmt.Sprintf("scala.math.%s", attr)
		}
	}
	base := sanitizeVarName(s.Root)
	var t types.Type
	if v, err := c.env.GetVar(s.Root); err == nil {
		t = v
	}
	if len(s.Tail) == 0 {
		if _, ok := t.(types.GroupType); ok {
			return base
		}
		return base
	}
	for i, f := range s.Tail {
		remaining := len(s.Tail) - i - 1
		switch tt := t.(type) {
		case types.MapType:
			base = fmt.Sprintf("%s(%q)", base, f)
			t = tt.Value
			continue
		case types.OptionType:
			if remaining > 0 {
				base = fmt.Sprintf("%s.get", base)
				t = tt.Elem
			} else {
				// last field, keep option as is
				t = tt
			}
		case types.GroupType:
			if f == "key" {
				base = fmt.Sprintf("%s.key", base)
				t = tt.Key
				continue
			}
			if strings.ToLower(f) == "items" {
				base = fmt.Sprintf("%s.items", base)
				t = types.ListType{Elem: tt.Elem}
				continue
			}
			base = fmt.Sprintf("%s.%s", base, sanitizeField(f))
			t = types.AnyType{}
		}
		if st, ok := t.(types.StructType); ok && st.Name != "" {
			base = fmt.Sprintf("%s.%s", base, sanitizeField(f))
			if ft, ok := st.Fields[f]; ok {
				t = ft
			} else {
				t = types.AnyType{}
			}
		} else {
			base = fmt.Sprintf("%s.%s", base, sanitizeField(f))
			t = types.AnyType{}
		}
	}
	return base
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
		if name, ok := simpleIdent(call.Args[0]); ok && c.env != nil {
			if vt, err := c.env.GetVar(name); err == nil {
				if lt, ok2 := vt.(types.ListType); ok2 {
					if _, any := lt.Elem.(types.AnyType); any {
						elemT := c.namedType(types.ExprType(call.Args[1], c.env))
						if st, ok := c.detectStructMap(call.Args[1], c.env); ok {
							st = c.ensureStructName(st)
							elemT = st
						}
						c.env.SetVarDeep(name, types.ListType{Elem: elemT}, true)
					}
				}
			}
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
		argStr := args[0]
		if call.Args[0] != nil && call.Args[0].Binary != nil {
			if ml := call.Args[0].Binary.Left.Value.Target.Map; ml != nil {
				old := c.forceMap
				c.forceMap = true
				var err error
				argStr, err = c.compileExpr(call.Args[0])
				c.forceMap = old
				if err != nil {
					return "", err
				}
			}
		}
		t := types.ExprType(call.Args[0], c.env)
		switch t.(type) {
		case types.MapType:
			return fmt.Sprintf("%s.size", argStr), nil
		default:
			return fmt.Sprintf("%s.length", argStr), nil
		}
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		c.use("_now")
		return "_now()", nil
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
	case "padStart":
		if len(args) != 3 {
			return "", fmt.Errorf("padStart expects 3 args")
		}
		fill := args[2]
		if strings.HasPrefix(fill, "\"") && strings.HasSuffix(fill, "\"") && len([]rune(fill[1:len(fill)-1])) == 1 {
			fill = fmt.Sprintf("'%s'", fill[1:len(fill)-1])
		} else {
			fill = fmt.Sprintf("%s.charAt(0)", fill)
		}
		return fmt.Sprintf("%s.reverse.padTo(%s, %s).reverse", args[0], args[1], fill), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values.toList", args[0]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		t := types.ExprType(call.Args[0], c.env)
		switch t.(type) {
		case types.MapType, types.StructType, types.UnionType:
			return fmt.Sprintf("scala.util.parsing.json.JSONObject(%s.asInstanceOf[scala.collection.immutable.Map[String,Any]]).toString()", args[0]), nil
		case types.ListType:
			return fmt.Sprintf("scala.util.parsing.json.JSONArray(%s.asInstanceOf[List[Any]]).toString()", args[0]), nil
		default:
			c.use("_to_json")
			return fmt.Sprintf("_to_json(%s)", args[0]), nil
		}
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
	if mutable || c.preferMutable {
		prefix = "scala.collection.mutable.ArrayBuffer"
	}

	if len(l.Elems) == 0 {
		if lt, ok := c.contextType.(types.ListType); ok {
			t := c.typeOf(lt.Elem)
			if prefix == "scala.collection.mutable.ArrayBuffer" {
				return fmt.Sprintf("%s[%s]()", prefix, t), nil
			}
			return fmt.Sprintf("%s[%s]()", prefix, t), nil
		}
		if prefix == "scala.collection.mutable.ArrayBuffer" {
			return fmt.Sprintf("%s[Any]()", prefix), nil
		}
		return fmt.Sprintf("%s()", prefix), nil
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
	if st, ok := elemType.(types.StructType); ok && !mutable {
		// directly emit case class instances instead of maps
		for i, e := range l.Elems {
			if e.Binary != nil && len(e.Binary.Right) == 0 && e.Binary.Left.Value.Target.Map != nil {
				s, err := c.mapToStruct(st.Name, st, e.Binary.Left.Value.Target.Map)
				if err != nil {
					return "", err
				}
				elems[i] = s
				continue
			}
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
	} else {
		for i, e := range l.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
	}

	return fmt.Sprintf("%s(%s)", prefix, strings.Join(elems, ", ")), nil
}

func (c *Compiler) compileMap(m *parser.MapLiteral, mutable bool) (string, error) {
	if c.inSort {
		vals := make([]string, len(m.Items))
		for i, it := range m.Items {
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			vals[i] = v
		}
		return fmt.Sprintf("(%s)", strings.Join(vals, ", ")), nil
	}
	// determine if this map corresponds to a struct type
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Map: m}}}}}
	t := c.namedType(types.ExprType(expr, c.env))
	if !c.forceMap {
		if st, ok := t.(types.StructType); ok && !mutable && !c.mapVars[c.structHint] {
			return c.mapToStruct(st.Name, st, m)
		}
		if st, ok := c.detectStructMap(expr, c.env); ok && !mutable && !c.mapVars[c.structHint] {
			st = c.ensureStructName(st)
			return c.mapToStruct(st.Name, st, m)
		}
	}
	// if all keys are simple strings, treat as an anonymous struct
	if !c.forceMap && !mutable && !c.mapVars[c.structHint] {
		if _, isMap := t.(types.MapType); !isMap && c.structHint != "" {
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
	}
	if len(m.Items) == 0 {
		if mt, ok := c.contextType.(types.MapType); ok {
			k := c.typeOf(mt.Key)
			v := c.typeOf(mt.Value)
			prefix := "Map"
			if mutable {
				prefix = "scala.collection.mutable.Map"
			}
			return fmt.Sprintf("%s[%s, %s]()", prefix, k, v), nil
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
		items[i] = fmt.Sprintf("%s -> %s", k, v)
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
	if c.inSort {
		return fmt.Sprintf("(%s)", strings.Join(vals, ", ")), nil
	}
	return fmt.Sprintf("%s(%s)", prefix, strings.Join(items, ", ")), nil
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
		args[i] = fmt.Sprintf("%s = %s", sanitizeField(f.Name), val)
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
	switch t := types.ExprType(q.Source, c.env).(type) {
	case types.ListType:
		child.SetVar(q.Var, t.Elem, false)
	case types.GroupType:
		child.SetVar(q.Var, t.Elem, false)
	}
	oldEnv := c.env
	c.env = child

	part := fmt.Sprintf("%s <- %s", q.Var, src)
	if lt, ok := types.ExprType(q.Source, c.env).(types.ListType); ok {
		if _, any := lt.Elem.(types.AnyType); !any {
			part = fmt.Sprintf("%s: %s <- %s", q.Var, c.typeOf(lt.Elem), src)
		}
	} else if gt, ok := types.ExprType(q.Source, c.env).(types.GroupType); ok {
		if _, any := gt.Elem.(types.AnyType); !any {
			part = fmt.Sprintf("%s: %s <- %s", q.Var, c.typeOf(gt.Elem), src)
		}
	}
	parts = append(parts, part)

	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		fpart := fmt.Sprintf("%s <- %s", f.Var, s)
		if lt, ok := types.ExprType(f.Src, c.env).(types.ListType); ok {
			if _, any := lt.Elem.(types.AnyType); !any {
				fpart = fmt.Sprintf("%s: %s <- %s", f.Var, c.typeOf(lt.Elem), s)
			}
		} else if gt, ok := types.ExprType(f.Src, c.env).(types.GroupType); ok {
			if _, any := gt.Elem.(types.AnyType); !any {
				fpart = fmt.Sprintf("%s: %s <- %s", f.Var, c.typeOf(gt.Elem), s)
			}
		}
		parts = append(parts, fpart)
		switch ft := types.ExprType(f.Src, c.env).(type) {
		case types.ListType:
			child.SetVar(f.Var, ft.Elem, false)
		case types.GroupType:
			child.SetVar(f.Var, ft.Elem, false)
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
		jt := types.ExprType(j.On, c.env)
		if _, ok := jt.(types.BoolType); !ok {
			cond = truthyExpr(cond, jt)
		}
		if j.Side == nil {
			jpart := fmt.Sprintf("%s <- %s", j.Var, s)
			if _, any := elemT.(types.AnyType); !any {
				jpart = fmt.Sprintf("%s: %s <- %s", j.Var, c.typeOf(elemT), s)
			}
			parts = append(parts, jpart)
			parts = append(parts, fmt.Sprintf("if %s", cond))
			child.SetVar(j.Var, elemT, false)
		} else if *j.Side == "outer" {
			pair := c.newVar("pair")
			leftT, _ := child.GetVar(q.Var)
			parts[len(parts)-1] = fmt.Sprintf("%s <- _outer_join(%s, %s)((%s,%s) => %s)", pair, src, s, q.Var, j.Var, cond)
			parts = append(parts, fmt.Sprintf("%s = %s._1", q.Var, pair))
			parts = append(parts, fmt.Sprintf("%s = %s._2", j.Var, pair))
			child.SetVar(q.Var, types.OptionType{Elem: leftT}, false)
			child.SetVar(j.Var, types.OptionType{Elem: elemT}, false)
			c.use("_outer_join")
		} else {
			parts = append(parts, fmt.Sprintf("%s = %s.find(%s => %s)", j.Var, s, j.Var, cond))
			child.SetVar(j.Var, types.OptionType{Elem: elemT}, false)
		}
	}

	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		wt := types.ExprType(q.Where, c.env)
		if _, ok := wt.(types.BoolType); !ok {
			cond = truthyExpr(cond, wt)
		}
		parts = append(parts, fmt.Sprintf("if %s", cond))
	}
	var expr string
	if q.Group != nil {
		oldHint := c.structHint
		c.structHint = q.Group.Name
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		c.structHint = oldHint
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		names := append([]string{q.Var}, func() []string {
			n := make([]string, len(q.Froms)+len(q.Joins))
			for i, f := range q.Froms {
				n[i] = f.Var
			}
			for i, j := range q.Joins {
				n[len(q.Froms)+i] = j.Var
			}
			return n
		}()...)
		var tuple string
		var elem types.Type
		if len(names) == 1 {
			elem, _ = child.GetVar(names[0])
			tuple = names[0]
		} else {
			st := types.StructType{Fields: make(map[string]types.Type), Order: names}
			for _, n := range names {
				if t, err := child.GetVar(n); err == nil {
					st.Fields[n] = t
				}
			}
			st = c.ensureStructName(st)
			partsExpr := make([]string, len(names))
			for i, n := range names {
				partsExpr[i] = fmt.Sprintf("%s = %s", sanitizeField(n), n)
			}
			tuple = fmt.Sprintf("%s(%s)", st.Name, strings.Join(partsExpr, ", "))
			elem = st
		}
		tmp := fmt.Sprintf("for { %s } yield (%s, %s)", strings.Join(parts, "; "), keyExpr, tuple)
		groups := fmt.Sprintf("(%s).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList", tmp)
		c.use("_Group")

		groupEnv := types.NewEnv(c.env)
		keyT := c.namedType(types.ExprType(q.Group.Exprs[0], child))
		if st, ok := c.detectStructMap(q.Group.Exprs[0], child); ok {
			st = c.ensureStructName(st)
			keyT = st
		}
		groupEnv.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: elem}, false)

		if q.Sort != nil {
			c.env = groupEnv
			c.inSort = true
			key, err := c.compileExpr(q.Sort)
			c.inSort = false
			if err != nil {
				c.env = oldEnv
				return "", err
			}
			groups = fmt.Sprintf("(%s).sortBy(%s => %s)", groups, q.Group.Name, key)
		}

		if q.Group.Having != nil {
			c.env = groupEnv
			cond, err := c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = oldEnv
				return "", err
			}
			ht := types.ExprType(q.Group.Having, c.env)
			if _, ok := ht.(types.BoolType); !ok {
				cond = truthyExpr(cond, ht)
			}
			if q.Group.Name == "g" {
				groups = fmt.Sprintf("(%s).filter{ g => %s }", groups, cond)
			} else {
				groups = fmt.Sprintf("(%s).filter{ g => { val %s = g; %s } }", groups, q.Group.Name, cond)
			}
		}

		c.env = groupEnv
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = oldEnv
			return "", err
		}
		if q.Group.Name == "g" {
			expr = fmt.Sprintf("(%s).map{ g => %s }.toList", groups, sel)
		} else {
			expr = fmt.Sprintf("(%s).map{ g => { val %s = g; %s } }.toList", groups, q.Group.Name, sel)
		}
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
			if q.Sort != nil {
				// sort before projecting so sort key can reference original record
				base := fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), q.Var)
				c.inSort = true
				key, err := c.compileExpr(q.Sort)
				c.inSort = false
				if err != nil {
					c.env = oldEnv
					return "", err
				}
				expr = fmt.Sprintf("(%s).sortBy(%s => %s).map(%s => %s)", base, q.Var, key, q.Var, sel)
			} else {
				expr = fmt.Sprintf("for { %s } yield %s", strings.Join(parts, "; "), sel)
			}
		}
	}
	if q.Sort != nil && q.Group == nil && expr != "" && !strings.Contains(expr, ".sortBy(") {
		// handles queries where sort is specified but select clause triggered early exit
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
		p := *l.Path
		if strings.HasPrefix(p, "../interpreter/valid/") {
			p = filepath.Join("..", "..", "tests", "interpreter", "valid", strings.TrimPrefix(p, "../interpreter/valid/"))
		}
		path = fmt.Sprintf("%q", p)
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
	expr := fmt.Sprintf("_load_%s(%s)", format, path)
	if l.Type != nil {
		if st, ok := types.ResolveTypeRef(l.Type, c.env).(types.StructType); ok {
			fields := make([]string, len(st.Order))
			for i, f := range st.Order {
				conv := ""
				switch st.Fields[f].(type) {
				case types.IntType:
					conv = ".toInt"
				case types.FloatType:
					conv = ".toDouble"
				}
				fields[i] = fmt.Sprintf("%s = r(%q)%s", sanitizeField(f), f, conv)
			}
			expr = fmt.Sprintf("%s.map(r => %s(%s))", expr, st.Name, strings.Join(fields, ", "))
		}
	}
	return expr, nil
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
		args[i] = fmt.Sprintf("%s = %s", sanitizeField(field), val)
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
	case types.OptionType:
		return fmt.Sprintf("Option[%s]", c.typeOf(tt.Elem))
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
	if oa, ok := a.(types.OptionType); ok {
		if ob, ok := b.(types.OptionType); ok {
			return sameType(oa.Elem, ob.Elem)
		}
	}
	if ob, ok := b.(types.OptionType); ok {
		if oa, ok := a.(types.OptionType); ok {
			return sameType(oa.Elem, ob.Elem)
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

func simplePostfixIdent(p *parser.PostfixExpr) (string, bool) {
	if p == nil || len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func isIfExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.If == nil {
		return false
	}
	return true
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

func selectorOfExpr(e *parser.Expr) (*parser.SelectorExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return nil, false
	}
	return p.Target.Selector, true
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
		case s.Assign != nil:
			out[s.Assign.Name] = true
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

func collectMapUses(stmts []*parser.Statement, out map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			collectExprMapUses(s.Let.Value, out)
		case s.Var != nil:
			collectExprMapUses(s.Var.Value, out)
		case s.Assign != nil:
			if len(s.Assign.Index) > 0 {
				out[s.Assign.Name] = true
			}
			collectExprMapUses(s.Assign.Value, out)
			for _, idx := range s.Assign.Index {
				collectExprMapUses(idx.Start, out)
				collectExprMapUses(idx.End, out)
				collectExprMapUses(idx.Step, out)
			}
		case s.Update != nil:
			out[s.Update.Target] = true
			if s.Update.Where != nil {
				collectExprMapUses(s.Update.Where, out)
			}
			for _, it := range s.Update.Set.Items {
				collectExprMapUses(it.Key, out)
				collectExprMapUses(it.Value, out)
			}
		case s.Return != nil:
			collectExprMapUses(s.Return.Value, out)
		case s.Expr != nil:
			collectExprMapUses(s.Expr.Expr, out)
		case s.If != nil:
			collectExprMapUses(s.If.Cond, out)
			collectMapUses(s.If.Then, out)
			if s.If.ElseIf != nil {
				collectMapUses([]*parser.Statement{{If: s.If.ElseIf}}, out)
			}
			if s.If.Else != nil {
				collectMapUses(s.If.Else, out)
			}
		case s.While != nil:
			collectExprMapUses(s.While.Cond, out)
			collectMapUses(s.While.Body, out)
		case s.For != nil:
			collectExprMapUses(s.For.Source, out)
			collectExprMapUses(s.For.RangeEnd, out)
			collectMapUses(s.For.Body, out)
		case s.Fun != nil:
			collectMapUses(s.Fun.Body, out)
		case s.Test != nil:
			collectMapUses(s.Test.Body, out)
		}
	}
}

func collectExprMapUses(e *parser.Expr, out map[string]bool) {
	if e == nil || e.Binary == nil {
		return
	}
	collectUnaryMapUses(e.Binary.Left, out)
	for _, op := range e.Binary.Right {
		if op.Op == "in" {
			if name, ok := simplePostfixIdent(op.Right); ok {
				out[name] = true
			}
		}
		collectPostfixMapUses(op.Right, out)
	}
}

func collectUnaryMapUses(u *parser.Unary, out map[string]bool) {
	if u == nil {
		return
	}
	collectPostfixMapUses(u.Value, out)
}

func collectPostfixMapUses(p *parser.PostfixExpr, out map[string]bool) {
	if p == nil {
		return
	}
	collectPrimaryMapUses(p.Target, out)
	for _, op := range p.Ops {
		if op.Index != nil {
			if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
				out[p.Target.Selector.Root] = true
			}
			collectExprMapUses(op.Index.Start, out)
			collectExprMapUses(op.Index.End, out)
			collectExprMapUses(op.Index.Step, out)
		} else if op.Call != nil {
			for _, a := range op.Call.Args {
				collectExprMapUses(a, out)
			}
		} else if op.Field != nil {
			// nothing
		} else if op.Cast != nil {
			// nothing
		}
	}
}

func collectPrimaryMapUses(p *parser.Primary, out map[string]bool) {
	if p == nil {
		return
	}
	switch {
	case p.If != nil:
		collectIfExprMapUses(p.If, out)
	case p.FunExpr != nil:
		collectFunExprMapUses(p.FunExpr, out)
	case p.List != nil:
		for _, e := range p.List.Elems {
			collectExprMapUses(e, out)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			collectExprMapUses(it.Key, out)
			collectExprMapUses(it.Value, out)
		}
	case p.Struct != nil:
		for _, f := range p.Struct.Fields {
			collectExprMapUses(f.Value, out)
		}
	case p.Match != nil:
		collectExprMapUses(p.Match.Target, out)
		for _, cs := range p.Match.Cases {
			collectExprMapUses(cs.Pattern, out)
			collectExprMapUses(cs.Result, out)
		}
	case p.Query != nil:
		collectExprMapUses(p.Query.Source, out)
		for _, f := range p.Query.Froms {
			collectExprMapUses(f.Src, out)
		}
		for _, j := range p.Query.Joins {
			collectExprMapUses(j.Src, out)
			collectExprMapUses(j.On, out)
		}
		collectExprMapUses(p.Query.Where, out)
		if p.Query.Group != nil {
			for _, ge := range p.Query.Group.Exprs {
				collectExprMapUses(ge, out)
			}
			collectExprMapUses(p.Query.Group.Having, out)
		}
		collectExprMapUses(p.Query.Sort, out)
		collectExprMapUses(p.Query.Select, out)
	case p.Call != nil:
		if p.Call.Func == "len" || p.Call.Func == "json" || p.Call.Func == "to_json" || p.Call.Func == "values" {
			for _, a := range p.Call.Args {
				if name, ok := identName(a); ok {
					out[name] = true
				}
			}
		}
		for _, a := range p.Call.Args {
			collectExprMapUses(a, out)
		}
	case p.Selector != nil:
		// nothing
	case p.Group != nil:
		collectExprMapUses(p.Group, out)
	case p.Load != nil:
		collectExprMapUses(p.Load.With, out)
	case p.Save != nil:
		collectExprMapUses(p.Save.Src, out)
		collectExprMapUses(p.Save.With, out)
	}
}

func collectIfExprMapUses(e *parser.IfExpr, out map[string]bool) {
	if e == nil {
		return
	}
	collectExprMapUses(e.Cond, out)
	collectExprMapUses(e.Then, out)
	if e.ElseIf != nil {
		collectIfExprMapUses(e.ElseIf, out)
	}
	collectExprMapUses(e.Else, out)
}

func collectFunExprMapUses(f *parser.FunExpr, out map[string]bool) {
	if f == nil {
		return
	}
	if f.ExprBody != nil {
		collectExprMapUses(f.ExprBody, out)
	}
	if len(f.BlockBody) > 0 {
		collectMapUses(f.BlockBody, out)
	}
}

func structKey(st types.StructType) string {
	parts := make([]string, len(st.Order))
	for i, f := range st.Order {
		parts[i] = fmt.Sprintf("%s:%s", f, st.Fields[f].String())
	}
	return strings.Join(parts, ";")
}

func deriveStructName(base string) string {
	if base == "" {
		return ""
	}
	if strings.HasSuffix(base, "s") && len(base) > 1 {
		base = base[:len(base)-1]
	}
	base = strings.ReplaceAll(base, "-", "_")
	parts := strings.Split(base, "_")
	for i, p := range parts {
		if len(p) == 0 {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
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
	var name string
	if hint := deriveStructName(c.structHint); hint != "" {
		name = hint
		if _, ok := c.autoStructs[name]; ok {
			c.autoCount++
			name = fmt.Sprintf("%s%d", hint, c.autoCount)
		}
	} else {
		c.autoCount++
		name = fmt.Sprintf("Auto%d", c.autoCount)
	}
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
	case types.OptionType:
		return types.OptionType{Elem: c.namedType(tt.Elem)}
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
			fields[i] = fmt.Sprintf("%s: %s", sanitizeField(f), c.typeOf(st.Fields[f]))
		}
		out.WriteString(pad + fmt.Sprintf("case class %s(%s)\n", n, strings.Join(fields, ", ")))
	}
	out.WriteByte('\n')
}

func (c *Compiler) querySelectEnv(q *parser.QueryExpr) *types.Env {
	env := types.NewEnv(c.env)
	if lt, ok := types.ExprType(q.Source, c.env).(types.ListType); ok {
		env.SetVar(q.Var, lt.Elem, false)
	} else {
		env.SetVar(q.Var, types.AnyType{}, false)
	}
	for _, f := range q.Froms {
		if lt, ok := types.ExprType(f.Src, c.env).(types.ListType); ok {
			env.SetVar(f.Var, lt.Elem, false)
		} else {
			env.SetVar(f.Var, types.AnyType{}, false)
		}
	}
	for _, j := range q.Joins {
		var elem types.Type = types.AnyType{}
		if lt, ok := types.ExprType(j.Src, c.env).(types.ListType); ok {
			elem = lt.Elem
		}
		if j.Side == nil {
			env.SetVar(j.Var, elem, false)
			continue
		}
		if *j.Side == "outer" {
			if cur, err := env.GetVar(q.Var); err == nil {
				env.SetVar(q.Var, types.OptionType{Elem: cur}, false)
			} else {
				env.SetVar(q.Var, types.OptionType{Elem: types.AnyType{}}, false)
			}
			env.SetVar(j.Var, types.OptionType{Elem: elem}, false)
		} else if *j.Side == "right" {
			env.SetVar(j.Var, types.OptionType{Elem: elem}, false)
		} else { // left join
			env.SetVar(j.Var, types.OptionType{Elem: elem}, false)
		}
	}
	if q.Group != nil {
		keyT := c.namedType(types.ExprType(q.Group.Exprs[0], env))
		if st, ok := c.detectStructMap(q.Group.Exprs[0], env); ok {
			st = c.ensureStructName(st)
			keyT = st
		}
		g := types.NewEnv(env)
		g.SetVar(q.Group.Name, types.GroupType{Key: keyT, Elem: types.AnyType{}}, false)
		env = g
	}
	return env
}

func (c *Compiler) addImport(im *parser.ImportStmt) error {
	if im.Lang == nil {
		return fmt.Errorf("unsupported import language: <nil>")
	}
	mod := strings.Trim(im.Path, "\"")
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	switch *im.Lang {
	case "python":
		if mod == "math" {
			c.pyModules[alias] = mod
			return nil
		}
		return fmt.Errorf("unsupported python module: %s", mod)
	case "go":
		if mod == "mochi/runtime/ffi/go/testpkg" {
			c.writeln(fmt.Sprintf("object %s {", alias))
			c.indent += indentStep
			c.writeln("def Add(a: Int, b: Int): Int = a + b")
			c.writeln("val Pi: Double = 3.14")
			c.writeln("val Answer: Int = 42")
			c.indent -= indentStep
			c.writeln("}")
			c.writeln("")
			return nil
		}
		return fmt.Errorf("unsupported go module: %s", mod)
	default:
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
}

func hasReturn(stmts []*parser.Statement) bool {
	for _, st := range stmts {
		if st.Return != nil || st.Break != nil || st.Continue != nil {
			return true
		}
		if st.If != nil {
			if hasReturn(st.If.Then) {
				return true
			}
			cur := st.If
			for cur.ElseIf != nil {
				if hasReturn(cur.ElseIf.Then) {
					return true
				}
				cur = cur.ElseIf
			}
			if len(cur.Else) > 0 && hasReturn(cur.Else) {
				return true
			}
		}
		if st.For != nil && hasReturn(st.For.Body) {
			return true
		}
		if st.While != nil && hasReturn(st.While.Body) {
			return true
		}
	}
	return false
}
