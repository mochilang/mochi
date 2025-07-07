//go:build archived

package scalacode

import (
	"sort"
	"strings"
)

const (
	helperCompare = `def _compare(a: Any, b: Any): Int = (a, b) match {
        case (x: Int, y: Int) => x.compare(y)
        case (x: Double, y: Double) => java.lang.Double.compare(x, y)
        case (x: String, y: String) => x.compareTo(y)
        case _ => a.toString.compareTo(b.toString)
}
        implicit val _anyOrdering: Ordering[Any] = new Ordering[Any] { def compare(x: Any, y: Any): Int = _compare(x, y) }
`
	helperCast = `def _cast[T](v: Any)(implicit ct: scala.reflect.ClassTag[T]): T = {
        val cls = ct.runtimeClass
        if (cls == classOf[Int]) v match {
                case i: Int => i
                case d: Double => d.toInt
                case s: String => s.toInt
                case _ => 0
        } else if (cls == classOf[Double]) v match {
                case d: Double => d
                case i: Int => i.toDouble
                case s: String => s.toDouble
                case _ => 0.0
        } else if (cls == classOf[Boolean]) v match {
                case b: Boolean => b
                case s: String => s == "true"
                case _ => false
        } else if (cls == classOf[String]) v.toString.asInstanceOf[T]
        else if (cls.isInstance(v)) v.asInstanceOf[T]
        else if (v.isInstanceOf[scala.collection.Map[_, _]]) {
                val m = v.asInstanceOf[scala.collection.Map[String, Any]]
                val ctor = cls.getConstructors.head
                val params = ctor.getParameters.map { p =>
                        m.getOrElse(p.getName, null).asInstanceOf[AnyRef]
                }
                ctor.newInstance(params: _*).asInstanceOf[T]
        } else {
                v.asInstanceOf[T]
        }
}`
	helperFetch = `def _fetch(url: String, opts: Map[String, Any]): Any = {
        import java.net.{HttpURLConnection, URL, URLEncoder}
        import java.io.{BufferedWriter, OutputStreamWriter}
        var u = url
        if (opts != null && opts.contains("query")) {
                val q = opts("query").asInstanceOf[scala.collection.Map[String, Any]]
                val qs = q.map { case (k, v) =>
                        URLEncoder.encode(k, "UTF-8") + "=" + URLEncoder.encode(String.valueOf(v), "UTF-8")
                }.mkString("&")
                val sep = if (u.contains("?")) "&" else "?"
                u = u + sep + qs
        }
        val conn = new URL(u).openConnection().asInstanceOf[HttpURLConnection]
        var method = "GET"
        if (opts != null && opts.contains("method")) {
                method = opts("method").toString
        }
        conn.setRequestMethod(method)
        if (opts != null && opts.contains("headers")) {
                val hs = opts("headers").asInstanceOf[scala.collection.Map[String, Any]]
                hs.foreach { case (k, v) => conn.setRequestProperty(k, v.toString) }
        }
        if (opts != null && opts.contains("timeout")) {
                val t = opts("timeout") match {
                        case i: Int => i.toDouble
                        case l: Long => l.toDouble
                        case f: Float => f.toDouble
                        case d: Double => d
                        case other => other.toString.toDouble
                }
                val ms = (t * 1000).toInt
                conn.setConnectTimeout(ms)
                conn.setReadTimeout(ms)
        }
        if (opts != null && opts.contains("body")) {
                conn.setDoOutput(true)
                val data = _to_json(opts("body"))
                val w = new BufferedWriter(new OutputStreamWriter(conn.getOutputStream))
                w.write(data)
                w.flush()
                w.close()
        }
        val src = scala.io.Source.fromInputStream(conn.getInputStream)
        try {
                val data = src.mkString
                scala.util.parsing.json.JSON.parseFull(data).getOrElse(data)
        } finally src.close()
}
`
	helperLoad = `def _load(path: String, opts: Map[String, Any]): Seq[Any] = {
        val fmt = opts.getOrElse("format", "json").asInstanceOf[String]
        val src = if (path == "" || path == "-") scala.io.Source.stdin else scala.io.Source.fromFile(path)
        try {
                val data = src.mkString
                fmt match {
                        case "jsonl" =>
                                data.split("\n").toSeq.filter(_.nonEmpty).map { line =>
                                        scala.util.parsing.json.JSON.parseFull(line).getOrElse(line)
                                }
                        case "json" =>
                                scala.util.parsing.json.JSON.parseFull(data) match {
                                        case Some(xs: List[_]) => xs
                                        case Some(m) => Seq(m)
                                        case _ => Seq.empty
                                }
                        case _ =>
                                scala.util.parsing.json.JSON.parseFull(data) match {
                                        case Some(xs: List[_]) => xs
                                        case Some(m) => Seq(m)
                                        case _ => data.split("\n").toSeq
                                }
                }
        } finally src.close()
        }`
	helperSave = `def _save(src: Any, path: String, opts: Map[String, Any]): Unit = {
        val fmt = opts.getOrElse("format", "json").asInstanceOf[String]
        val out = if (path == "" || path == "-") new java.io.PrintWriter(System.out) else new java.io.PrintWriter(new java.io.File(path))
        try {
                fmt match {
                        case "jsonl" =>
                                src.asInstanceOf[Seq[Any]].foreach(v => out.println(_to_json(v)))
                        case "json" =>
                                out.println(_to_json(src))
                        case _ =>
                                src match {
                                        case seq: Seq[_] => seq.foreach(v => out.println(v.toString))
                                        case other => out.println(other.toString)
                                }
                }
        } finally if (path != "" && path != "-") out.close()
        }`
	helperIndexString = `def _indexString(s: String, i: Int): String = {
        var idx = i
        val chars = s.toVector
        if (idx < 0) idx += chars.length
        if (idx < 0 || idx >= chars.length) throw new RuntimeException("index out of range")
        chars(idx).toString
}
`
	helperIndexList = `def _indexList[T](arr: scala.collection.mutable.ArrayBuffer[T], i: Int): T = {
        var idx = i
        val n = arr.length
        if (idx < 0) idx += n
        if (idx < 0 || idx >= n) throw new RuntimeException("index out of range")
        arr(idx)
}
`
	helperSliceString = `def _sliceString(s: String, i: Int, j: Int): String = {
        var start = i
        var end = j
        val chars = s.toVector
        val n = chars.length
        if (start < 0) start += n
        if (end < 0) end += n
        if (start < 0) start = 0
        if (end > n) end = n
        if (end < start) end = start
        chars.slice(start, end).mkString
}
`
	helperSlice = `def _slice[T](arr: scala.collection.mutable.ArrayBuffer[T], i: Int, j: Int): scala.collection.mutable.ArrayBuffer[T] = {
        var start = i
        var end = j
        val n = arr.length
        if (start < 0) start += n
        if (end < 0) end += n
        if (start < 0) start = 0
        if (end > n) end = n
        if (end < start) end = start
        arr.slice(start, end)
}
`
	helperGenText = `def _genText(prompt: String, model: String, params: Map[String, Any]): String = {
        // TODO: integrate with an LLM
        prompt
}
`
	helperGenEmbed = `def _genEmbed(text: String, model: String, params: Map[String, Any]): Seq[Double] = {
        text.map(c => c.toDouble)
}
`
	helperGenStruct = `def _genStruct[T](prompt: String, model: String, params: Map[String, Any])(implicit ct: scala.reflect.ClassTag[T]): T = {
        // TODO: integrate with an LLM and parse JSON
        ct.runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[T]
}
`
	helperReduce = `def _reduce[T](src: Iterable[T], fn: (T, T) => T, init: T): T = {
        var acc = init
        for (it <- src) {
                acc = fn(acc, it)
        }
        acc
        }`
	helperExists = `def _exists(v: Any): Boolean = v match {
        case null => false
        case s: String => s.nonEmpty
        case seq: Iterable[_] => seq.nonEmpty
        case m: scala.collection.Map[_, _] =>
                if (m.contains("items") && m("items").isInstanceOf[Iterable[_]]) m("items").asInstanceOf[Iterable[_]].nonEmpty
                else if (m.contains("Items") && m("Items").isInstanceOf[Iterable[_]]) m("Items").asInstanceOf[Iterable[_]].nonEmpty
                else m.nonEmpty
        case g: _Group => g.Items.nonEmpty
        case _ => false
}`
	helperGroup = `class _Group(var key: Any) {
        val Items = scala.collection.mutable.ArrayBuffer[Any]()
}`
	helperGroupBy = `def _group_by(src: Seq[Any], keyfn: Any => Any): Seq[_Group] = {
        val groups = scala.collection.mutable.LinkedHashMap[String,_Group]()
        for (it <- src) {
                val key = keyfn(it)
                val ks = key.toString
                val g = groups.getOrElseUpdate(ks, new _Group(key))
                g.Items.append(it)
        }
        groups.values.toSeq
}`
	helperQuery = `def _query(src: Seq[Any], joins: Seq[Map[String,Any]], opts: Map[String,Any]): Seq[Any] = {
        var items = src.map(v => Seq[Any](v))
        for (j <- joins) {
                val joined = scala.collection.mutable.ArrayBuffer[Seq[Any]]()
                val jitems = j("items").asInstanceOf[Seq[Any]]
                val on = j.get("on").map(_.asInstanceOf[Seq[Any] => Boolean])
                val left = j.get("left").exists(_.asInstanceOf[Boolean])
                val right = j.get("right").exists(_.asInstanceOf[Boolean])
                if (left && right) {
                        val matched = Array.fill(jitems.length)(false)
                        for (leftRow <- items) {
                                var m = false
                                for ((rightRow, ri) <- jitems.zipWithIndex) {
                                        var keep = true
                                        if (on.isDefined) keep = on.get(leftRow :+ rightRow)
                                        if (keep) { m = true; matched(ri) = true; joined.append(leftRow :+ rightRow) }
                                }
                                if (!m) joined.append(leftRow :+ null)
                        }
                        for ((rightRow, ri) <- jitems.zipWithIndex) {
                                if (!matched(ri)) {
                                        val undef = if (items.nonEmpty) Seq.fill(items.head.length)(null) else Seq[Any]()
                                        joined.append(undef :+ rightRow)
                                }
                        }
                } else if (right) {
                        for (rightRow <- jitems) {
                                var m = false
                                for (leftRow <- items) {
                                        var keep = true
                                        if (on.isDefined) keep = on.get(leftRow :+ rightRow)
                                        if (keep) { m = true; joined.append(leftRow :+ rightRow) }
                                }
                                if (!m) {
                                        val undef = if (items.nonEmpty) Seq.fill(items.head.length)(null) else Seq[Any]()
                                        joined.append(undef :+ rightRow)
                                }
                        }
                } else {
                        for (leftRow <- items) {
                                var m = false
                                for (rightRow <- jitems) {
                                        var keep = true
                                        if (on.isDefined) keep = on.get(leftRow :+ rightRow)
                                        if (keep) { m = true; joined.append(leftRow :+ rightRow) }
                                }
                                if (left && !m) joined.append(leftRow :+ null)
                        }
                }
                items = joined.toSeq
        }
        var it = items
        opts.get("where").foreach { f =>
                val fn = f.asInstanceOf[Seq[Any] => Boolean]
                it = it.filter(r => fn(r))
        }
        opts.get("sortKey").foreach { f =>
                val fn = f.asInstanceOf[Seq[Any] => Any]
                it = it.sortBy(r => fn(r))(_anyOrdering)
        }
        opts.get("skip").foreach { n => it = it.drop(n.asInstanceOf[Int]) }
        opts.get("take").foreach { n => it = it.take(n.asInstanceOf[Int]) }
        val sel = opts("select").asInstanceOf[Seq[Any] => Any]
        it.map(r => sel(r))
}`
	helperToJSON = `def _to_json(v: Any): String = v match {
        case null => "null"
        case s: String => "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
        case b: Boolean => b.toString
        case i: Int => i.toString
        case l: Long => l.toString
        case d: Double => d.toString
        case m: scala.collection.Map[_, _] =>
                m.map{ case (k, v2) => "\"" + k.toString.replace("\"", "\\\"") + "\":" + _to_json(v2) }.mkString("{", ",", "}")
        case seq: Iterable[_] => seq.map(_to_json).mkString("[", ",", "]")
        case other => "\"" + other.toString.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
}`
	helperJSON   = `def _json(v: Any): Unit = println(_to_json(v))`
	helperExpect = `def expect(cond: Boolean): Unit = {
        if (!cond) throw new RuntimeException("expect failed")
}`
	helperPyAttr = `def _pyAttr(module: String, name: String, args: Seq[Any]): Any = {
        import scala.sys.process._
        import scala.util.parsing.json.{JSON, JSONArray}
        val script = "import json, os, importlib, sys, inspect\n" +
          s"args = json.loads(os.environ.get(\"MOCHI_ARGS\", \"[]\"))\n" +
          s"mod = importlib.import_module(module)\n" +
          s"attr = getattr(mod, name)\n" +
          "res = attr(*args) if inspect.isroutine(attr) else attr\n" +
          "json.dump(res, sys.stdout)\n"
        val argsJson = JSONArray(args.toList).toString()
        val proc = Process(Seq("python3", "-c", script), None, "MOCHI_ARGS" -> argsJson)
        val out = proc.!!.trim
        JSON.parseFull(out).getOrElse(out)
}
`
	helperExtern = `object ExternRegistry {
        private val externObjects = scala.collection.mutable.Map[String, Any]()
        def registerExtern(name: String, obj: Any): Unit = externObjects(name) = obj
        def _externGet(name: String): Any =
          externObjects.getOrElse(name, throw new RuntimeException("extern object not registered: " + name))
}`
	helperEval = `def _eval(code: String): Any = {
        import scala.tools.reflect.ToolBox
        val tb = scala.reflect.runtime.currentMirror.mkToolBox()
        tb.eval(tb.parse(code))
}`
)

var helperMap = map[string]string{
	"_compare":     helperCompare,
	"_cast":        helperCast,
	"_fetch":       helperFetch,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_indexString": helperIndexString,
	"_indexList":   helperIndexList,
	"_sliceString": helperSliceString,
	"_slice":       helperSlice,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_Group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_query":       helperQuery,
	"_reduce":      helperReduce,
	"_exists":      helperExists,
	"_pyAttr":      helperPyAttr,
	"_extern":      helperExtern,
	"_eval":        helperEval,
	"_to_json":     helperToJSON,
	"_json":        helperJSON,
	"_expect":      helperExpect,
}

func (c *Compiler) use(name string) {
	if c.helpers == nil {
		c.helpers = make(map[string]bool)
	}
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
		lines := strings.Split(strings.TrimSuffix(helperMap[n], "\n"), "\n")
		for _, line := range lines {
			c.writeln(line)
		}
		c.writeln("")
	}
}
