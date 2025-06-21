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
	helperFetch = `def _fetch(url: String, opts: Map[String, Any]): Any = {
        val src = scala.io.Source.fromURL(url)
        try {
                val data = src.mkString
                scala.util.parsing.json.JSON.parseFull(data).getOrElse(data)
        } finally src.close()
}
`
	helperLoad = `def _load(path: String, opts: Map[String, Any]): Seq[Any] = {
        val src = if (path == "" || path == "-") scala.io.Source.stdin else scala.io.Source.fromFile(path)
        try {
                val data = src.mkString
                scala.util.parsing.json.JSON.parseFull(data) match {
                        case Some(xs: List[_]) => xs
                        case Some(m) => Seq(m)
                        case _ => data.split('\n').toSeq
                }
        } finally src.close()
}
`
	helperSave = `def _save(src: Any, path: String, opts: Map[String, Any]): Unit = {
        val out = if (path == "" || path == "-") new java.io.PrintWriter(System.out) else new java.io.PrintWriter(new java.io.File(path))
        try {
                src match {
                        case seq: Seq[_] => seq.foreach(v => out.println(v.toString))
                        case other => out.println(other.toString)
                }
        } finally if (path != "" && path != "-") out.close()
}
`
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
)

var helperMap = map[string]string{
	"_compare":     helperCompare,
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
