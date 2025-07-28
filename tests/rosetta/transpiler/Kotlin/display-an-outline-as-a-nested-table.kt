import java.math.BigInteger

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

fun split(s: String, sep: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        if (((i + sep.length) <= s.length) && (s.substring(i, i + sep.length) == sep)) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
            cur = ""
            i = i + sep.length
        } else {
            cur = cur + (s.substring(i, i + 1)).toString()
            i = i + 1
        }
    }
    out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    return out
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]
        i = i + 1
    }
    return res
}

fun trimLeftSpaces(s: String): String {
    var i: Int = 0
    while ((i < s.length) && (s.substring(i, i + 1) == " ")) {
        i = i + 1
    }
    return s.substring(i, s.length)
}

fun makeIndent(outline: String, tab: Int): MutableList<MutableMap<String, Any?>> {
    val lines: MutableList<String> = split(outline, "\n")
    var nodes: MutableList<MutableMap<String, Any?>> = mutableListOf<MutableMap<String, Any?>>()
    for (line in lines) {
        val line2: String = trimLeftSpaces(line)
        val level: BigInteger = ((line.length - line2.length) / tab).toBigInteger()
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(mutableMapOf<String, Any?>("level" to (level), "name" to (line2))); _tmp } as MutableList<MutableMap<String, Any?>>
    }
    return nodes
}

fun toNest(nodes: MutableList<MutableMap<String, Any?>>, start: Int, level: Int, n: MutableMap<String, Any?>): Unit {
    if (level == 0) {
        (n)["name"] as Any? = (nodes[0])["name"] as Any?
    }
    var i: BigInteger = (start + 1).toBigInteger()
    while (i.compareTo(nodes.size.toBigInteger()) < 0) {
        val node: MutableMap<String, Any?> = nodes[(i).toInt()]
        val lev: Int = (node)["level"] as Int
        if (lev == (level + 1)) {
            var child: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to ((node)["name"] as Any?), "children" to (mutableListOf<Any?>()))
            toNest(nodes, i.toInt(), level + 1, child)
            var cs: MutableList<Any?> = ((n)["children"] as Any?) as MutableList<Any?>
            cs = run { val _tmp = cs.toMutableList(); _tmp.add(child as Any?); _tmp } as MutableList<Any?>
            (n)["children"] as Any? = cs as Any?
        } else {
            if (lev <= level) {
                return
            }
        }
        i = i.add(1.toBigInteger())
    }
}

fun countLeaves(n: MutableMap<String, Any?>): Int {
    val kids: MutableList<Any?> = ((n)["children"] as Any?) as MutableList<Any?>
    if (kids.size == 0) {
        return 1
    }
    var total: Int = 0
    for (k in kids) {
        total = total + countLeaves(k as MutableMap<String, Any?>)
    }
    return total
}

fun nodesByDepth(root: MutableMap<String, Any?>, depth: Int): MutableList<MutableList<MutableMap<String, Any?>>> {
    var levels: MutableList<MutableList<MutableMap<String, Any?>>> = mutableListOf<MutableList<MutableMap<String, Any?>>>()
    var current: MutableList<MutableMap<String, Any?>> = mutableListOf(root)
    var d: Int = 0
    while (d < depth) {
        levels = run { val _tmp = levels.toMutableList(); _tmp.add(current); _tmp } as MutableList<MutableList<MutableMap<String, Any?>>>
        var next: MutableList<MutableMap<String, Any?>> = mutableListOf<MutableMap<String, Any?>>()
        for (n in current) {
            val kids: MutableList<Any?> = ((n)["children"] as Any?) as MutableList<Any?>
            for (k in kids) {
                next = run { val _tmp = next.toMutableList(); _tmp.add(k as MutableMap<String, Any?>); _tmp } as MutableList<MutableMap<String, Any?>>
            }
        }
        current = next
        d = d + 1
    }
    return levels
}

fun toMarkup(n: MutableMap<String, Any?>, cols: MutableList<String>, depth: Int): String {
    var lines: MutableList<String> = mutableListOf<String>()
    lines = run { val _tmp = lines.toMutableList(); _tmp.add("{| class=\"wikitable\" style=\"text-align: center;\""); _tmp } as MutableList<String>
    val l1: String = "|-"
    lines = run { val _tmp = lines.toMutableList(); _tmp.add(l1); _tmp } as MutableList<String>
    val span: Int = countLeaves(n)
    lines = run { val _tmp = lines.toMutableList(); _tmp.add((((("| style=\"background: " + cols[0]) + " \" colSpan=") + span.toString()) + " | ") + (n)["name"] as String); _tmp } as MutableList<String>
    lines = run { val _tmp = lines.toMutableList(); _tmp.add(l1); _tmp } as MutableList<String>
    val lvls: MutableList<MutableList<MutableMap<String, Any?>>> = nodesByDepth(n, depth)
    var lvl: Int = 1
    while (lvl < depth) {
        val nodes: MutableList<MutableMap<String, Any?>> = lvls[lvl]
        if (nodes.size == 0) {
            lines = run { val _tmp = lines.toMutableList(); _tmp.add("|  |"); _tmp } as MutableList<String>
        } else {
            var idx: Int = 0
            while (idx < nodes.size) {
                val node: MutableMap<String, Any?> = nodes[idx]
                span = countLeaves(node)
                var col: Int = lvl
                if (lvl == 1) {
                    col = idx + 1
                }
                if (col >= cols.size) {
                    col = cols.size - 1
                }
                val cell: String = (((("| style=\"background: " + cols[col]) + " \" colspan=") + span.toString()) + " | ") + (node)["name"] as String
                lines = run { val _tmp = lines.toMutableList(); _tmp.add(cell); _tmp } as MutableList<String>
                idx = idx + 1
            }
        }
        if (lvl < (depth - 1)) {
            lines = run { val _tmp = lines.toMutableList(); _tmp.add(l1); _tmp } as MutableList<String>
        }
        lvl = lvl + 1
    }
    lines = run { val _tmp = lines.toMutableList(); _tmp.add("|}"); _tmp } as MutableList<String>
    return join(lines, "\n")
}

fun user_main(): Unit {
    val outline: String = (((((((((("Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n") + "        measuring the indent of each line,\n") + "        translating the indentation to a nested structure,\n") + "        and padding the tree to even depth.\n") + "    count the leaves descending from each node,\n") + "        defining the width of a leaf as 1,\n") + "        and the width of a parent node as a sum.\n") + "            (The sum of the widths of its children)\n") + "    and write out a table with 'colspan' values\n") + "        either as a wiki table,\n") + "        or as HTML."
    val yellow: String = "#ffffe6;"
    val orange: String = "#ffebd2;"
    val green: String = "#f0fff0;"
    val blue: String = "#e6ffff;"
    val pink: String = "#ffeeff;"
    val cols: MutableList<String> = mutableListOf(yellow, orange, green, blue, pink)
    val nodes: MutableList<MutableMap<String, Any?>> = makeIndent(outline, 4)
    var n: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to (""), "children" to (mutableListOf<Any?>()))
    toNest(nodes, 0, 0, n)
    println(toMarkup(n, cols, 4))
    println("\n")
    val outline2: String = (((((((((((("Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n") + "        measuring the indent of each line,\n") + "        translating the indentation to a nested structure,\n") + "        and padding the tree to even depth.\n") + "    count the leaves descending from each node,\n") + "        defining the width of a leaf as 1,\n") + "        and the width of a parent node as a sum.\n") + "            (The sum of the widths of its children)\n") + "            Propagating the sums upward as necessary.\n") + "    and write out a table with 'colspan' values\n") + "        either as a wiki table,\n") + "        or as HTML.\n") + "    Optionally add color to the nodes."
    val cols2: MutableList<String> = mutableListOf(blue, yellow, orange, green, pink)
    val nodes2: MutableList<MutableMap<String, Any?>> = makeIndent(outline2, 4)
    var n2: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to (""), "children" to (mutableListOf<Any?>()))
    toNest(nodes2, 0, 0, n2)
    println(toMarkup(n2, cols2, 4))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
