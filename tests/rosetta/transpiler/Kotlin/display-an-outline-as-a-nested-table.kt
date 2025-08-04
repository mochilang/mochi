import java.math.BigInteger

fun split(s: String, sep: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        if (((i + sep.length) <= s.length) && (s.substring(i, i + sep.length) == sep)) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp }
            cur = ""
            i = i + sep.length
        } else {
            cur = cur + s.substring(i, i + 1)
            i = i + 1
        }
    }
    out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp }
    return out
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]!!
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
    var lines: MutableList<String> = split(outline, "\n")
    var nodes: MutableList<MutableMap<String, Any?>> = mutableListOf<MutableMap<String, Any?>>()
    for (line in lines) {
        var line2: String = trimLeftSpaces(line)
        var level: BigInteger = ((line.length - line2.length) / tab).toBigInteger()
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(mutableMapOf<String, Any?>("level" to (level), "name" to (line2))); _tmp }
    }
    return nodes
}

fun toNest(nodes: MutableList<MutableMap<String, Any?>>, start: Int, level: Int, n: MutableMap<String, Any?>): Unit {
    if (level == 0) {
        (n)["name"] = (((nodes[0] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["name"]!!
    }
    var i: BigInteger = ((start + 1).toBigInteger())
    while (i.compareTo((nodes.size).toBigInteger()) < 0) {
        var node: MutableMap<String, Any?> = nodes[(i).toInt()] as MutableMap<String, Any?>
        var lev: Int = ((node)["level"] as java.math.BigInteger).toInt()
        if (lev == (level + 1)) {
            var child: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to ((node)["name"]!!), "children" to (mutableListOf<Any?>()))
            toNest(nodes, (i.toInt()), level + 1, child)
            var cs: MutableList<Any?> = (((n)["children"]!!) as MutableList<Any?>)
            cs = run { val _tmp = cs.toMutableList(); _tmp.add((child as Any?)); _tmp }
            (n)["children"] = (cs as Any?)
        } else {
            if (lev <= level) {
                return
            }
        }
        i = i.add((1).toBigInteger())
    }
}

fun countLeaves(n: MutableMap<String, Any?>): Int {
    var kids: MutableList<Any?> = (((n)["children"]!!) as MutableList<Any?>)
    if (kids.size == 0) {
        return 1
    }
    var total: Int = 0
    for (k in kids) {
        total = total + countLeaves((k as MutableMap<String, Any?>))
    }
    return total
}

fun nodesByDepth(root: MutableMap<String, Any?>, depth: Int): MutableList<MutableList<MutableMap<String, Any?>>> {
    var levels: MutableList<MutableList<MutableMap<String, Any?>>> = mutableListOf<MutableList<MutableMap<String, Any?>>>()
    var current: MutableList<MutableMap<String, Any?>> = mutableListOf(root)
    var d: Int = 0
    while (d < depth) {
        levels = run { val _tmp = levels.toMutableList(); _tmp.add(current); _tmp }
        var next: MutableList<MutableMap<String, Any?>> = mutableListOf<MutableMap<String, Any?>>()
        for (n in current) {
            var kids: MutableList<Any?> = (((n)["children"]!!) as MutableList<Any?>)
            for (k in kids) {
                next = run { val _tmp = next.toMutableList(); _tmp.add((k as MutableMap<String, Any?>)); _tmp }
            }
        }
        current = next
        d = d + 1
    }
    return levels
}

fun toMarkup(n: MutableMap<String, Any?>, cols: MutableList<String>, depth: Int): String {
    var lines: MutableList<String> = mutableListOf<String>()
    lines = run { val _tmp = lines.toMutableList(); _tmp.add("{| class=\"wikitable\" style=\"text-align: center;\""); _tmp }
    var l1: String = "|-"
    lines = run { val _tmp = lines.toMutableList(); _tmp.add(l1); _tmp }
    var span: Int = countLeaves(n)
    lines = run { val _tmp = lines.toMutableList(); _tmp.add((((("| style=\"background: " + cols[0]!!) + " \" colSpan=") + span.toString()) + " | ") + (n)["name"] as String); _tmp }
    lines = run { val _tmp = lines.toMutableList(); _tmp.add(l1); _tmp }
    var lvls: MutableList<MutableList<MutableMap<String, Any?>>> = nodesByDepth(n, depth)
    var lvl: Int = 1
    while (lvl < depth) {
        var nodes: MutableList<MutableMap<String, Any?>> = lvls[lvl] as MutableList<MutableMap<String, Any?>>
        if (nodes.size == 0) {
            lines = run { val _tmp = lines.toMutableList(); _tmp.add("|  |"); _tmp }
        } else {
            var idx: Int = 0
            while (idx < nodes.size) {
                var node: MutableMap<String, Any?> = nodes[idx] as MutableMap<String, Any?>
                span = countLeaves(node)
                var col: Int = lvl
                if (lvl == 1) {
                    col = idx + 1
                }
                if (col >= cols.size) {
                    col = cols.size - 1
                }
                var cell: String = (((("| style=\"background: " + cols[col]!!) + " \" colspan=") + span.toString()) + " | ") + (node)["name"] as String
                lines = run { val _tmp = lines.toMutableList(); _tmp.add(cell); _tmp }
                idx = idx + 1
            }
        }
        if (lvl < (depth - 1)) {
            lines = run { val _tmp = lines.toMutableList(); _tmp.add(l1); _tmp }
        }
        lvl = lvl + 1
    }
    lines = run { val _tmp = lines.toMutableList(); _tmp.add("|}"); _tmp }
    return join(lines, "\n")
}

fun user_main(): Unit {
    var outline: String = (((((((((("Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n") + "        measuring the indent of each line,\n") + "        translating the indentation to a nested structure,\n") + "        and padding the tree to even depth.\n") + "    count the leaves descending from each node,\n") + "        defining the width of a leaf as 1,\n") + "        and the width of a parent node as a sum.\n") + "            (The sum of the widths of its children)\n") + "    and write out a table with 'colspan' values\n") + "        either as a wiki table,\n") + "        or as HTML."
    var yellow: String = "#ffffe6;"
    var orange: String = "#ffebd2;"
    var green: String = "#f0fff0;"
    var blue: String = "#e6ffff;"
    var pink: String = "#ffeeff;"
    var cols: MutableList<String> = mutableListOf(yellow, orange, green, blue, pink)
    var nodes: MutableList<MutableMap<String, Any?>> = makeIndent(outline, 4)
    var n: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to (""), "children" to (mutableListOf<Any?>()))
    toNest(nodes, 0, 0, n)
    println(toMarkup(n, cols, 4))
    println("\n")
    var outline2: String = (((((((((((("Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n") + "        measuring the indent of each line,\n") + "        translating the indentation to a nested structure,\n") + "        and padding the tree to even depth.\n") + "    count the leaves descending from each node,\n") + "        defining the width of a leaf as 1,\n") + "        and the width of a parent node as a sum.\n") + "            (The sum of the widths of its children)\n") + "            Propagating the sums upward as necessary.\n") + "    and write out a table with 'colspan' values\n") + "        either as a wiki table,\n") + "        or as HTML.\n") + "    Optionally add color to the nodes."
    var cols2: MutableList<String> = mutableListOf(blue, yellow, orange, green, pink)
    var nodes2: MutableList<MutableMap<String, Any?>> = makeIndent(outline2, 4)
    var n2: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to (""), "children" to (mutableListOf<Any?>()))
    toNest(nodes2, 0, 0, n2)
    println(toMarkup(n2, cols2, 4))
}

fun main() {
    user_main()
}
