import java.math.BigInteger

fun Node(data: Int): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("Data" to (data), "Balance" to (0), "Link" to (mutableListOf(null, null)))
}

fun getLink(n: MutableMap<String, Any?>, dir: Int): Any? {
    return ((((n)["Link"] as Any?) as MutableList<Any?>))[dir] as Any?
}

fun setLink(n: MutableMap<String, Any?>, dir: Int, v: Any?): Unit {
    var links: MutableList<Any?> = (((n)["Link"] as Any?) as MutableList<Any?>)
    links[dir] = v
    (n)["Link"] = (links as Any?)
}

fun opp(dir: Int): Int {
    return 1 - dir
}

fun single(root: MutableMap<String, Any?>, dir: Int): MutableMap<String, Any?> {
    var tmp: Any? = getLink(root, opp(dir))
    setLink(root, opp(dir), getLink((tmp as MutableMap<String, Any?>), dir))
    setLink((tmp as MutableMap<String, Any?>), dir, (root as Any?))
    return (tmp as MutableMap<String, Any?>)
}

fun double(root: MutableMap<String, Any?>, dir: Int): MutableMap<String, Any?> {
    var tmp: Any? = getLink(((getLink(root, opp(dir))) as MutableMap<String, Any?>), dir)
    setLink(((getLink(root, opp(dir))) as MutableMap<String, Any?>), dir, getLink((tmp as MutableMap<String, Any?>), opp(dir)))
    setLink((tmp as MutableMap<String, Any?>), opp(dir), getLink(root, opp(dir)))
    setLink(root, opp(dir), tmp)
    tmp = getLink(root, opp(dir))
    setLink(root, opp(dir), getLink((tmp as MutableMap<String, Any?>), dir))
    setLink((tmp as MutableMap<String, Any?>), dir, (root as Any?))
    return (tmp as MutableMap<String, Any?>)
}

fun adjustBalance(root: MutableMap<String, Any?>, dir: Int, bal: Int): Unit {
    var n: MutableMap<String, Any?> = ((getLink(root, dir)) as MutableMap<String, Any?>)
    var nn: MutableMap<String, Any?> = ((getLink(n, opp(dir))) as MutableMap<String, Any?>)
    if ((nn)["Balance"] as Any? == 0) {
        (root)["Balance"] = (0 as Any?)
        (n)["Balance"] = (0 as Any?)
    } else {
        if ((nn)["Balance"] as Any? == bal) {
            (root)["Balance"] = ((0 - bal) as Any?)
            (n)["Balance"] = (0 as Any?)
        } else {
            (root)["Balance"] = (0 as Any?)
            (n)["Balance"] = (bal as Any?)
        }
    }
    (nn)["Balance"] = (0 as Any?)
}

fun insertBalance(root: MutableMap<String, Any?>, dir: Int): MutableMap<String, Any?> {
    var n: MutableMap<String, Any?> = ((getLink(root, dir)) as MutableMap<String, Any?>)
    var bal: BigInteger = ((2 * dir) - 1).toBigInteger()
    if (((n)["Balance"] as Any? as Int).toBigInteger().compareTo((bal)) == 0) {
        (root)["Balance"] = (0 as Any?)
        (n)["Balance"] = (0 as Any?)
        return single(root, opp(dir))
    }
    adjustBalance(root, dir, (bal.toInt()))
    return double(root, opp(dir))
}

fun insertR(root: Any?, data: Int): MutableMap<String, Any?> {
    if (root == null) {
        return mutableMapOf<String, Any?>("node" to (Node(data)), "done" to (false))
    }
    var node: MutableMap<String, Any?> = (root as MutableMap<String, Any?>)
    var dir: Int = 0
    if ((node)["Data"] as Int < data) {
        dir = 1
    }
    var r: MutableMap<String, Any?> = insertR(getLink(node, dir), data)
    setLink(node, dir, (r)["node"] as Any?)
    if ((((r)["done"] as Any?) as Boolean)) {
        return mutableMapOf<String, Any?>("node" to (node), "done" to (true))
    }
    (node)["Balance"] = (((node)["Balance"] as Int + ((2 * dir) - 1)) as Any?)
    if ((node)["Balance"] as Any? == 0) {
        return mutableMapOf<String, Any?>("node" to (node), "done" to (true))
    }
    if (((node)["Balance"] as Any? == 1) || ((node)["Balance"] as Any? == (0 - 1))) {
        return mutableMapOf<String, Any?>("node" to (node), "done" to (false))
    }
    return mutableMapOf<String, Any?>("node" to (insertBalance(node, dir)), "done" to (true))
}

fun Insert(tree: Any?, data: Int): Any? {
    var r: MutableMap<String, Any?> = insertR(tree, data)
    return (r)["node"] as Any?
}

fun removeBalance(root: MutableMap<String, Any?>, dir: Int): MutableMap<String, Any?> {
    var n: MutableMap<String, Any?> = ((getLink(root, opp(dir))) as MutableMap<String, Any?>)
    var bal: BigInteger = ((2 * dir) - 1).toBigInteger()
    if (((n)["Balance"] as Any? as Int).toBigInteger().compareTo(((0).toBigInteger().subtract((bal)))) == 0) {
        (root)["Balance"] = (0 as Any?)
        (n)["Balance"] = (0 as Any?)
        return mutableMapOf<String, Any?>("node" to (single(root, dir)), "done" to (false))
    }
    if (((n)["Balance"] as Any? as Int).toBigInteger().compareTo((bal)) == 0) {
        adjustBalance(root, opp(dir), (((0).toBigInteger().subtract((bal))).toInt()))
        return mutableMapOf<String, Any?>("node" to (double(root, dir)), "done" to (false))
    }
    (root)["Balance"] = (((0).toBigInteger().subtract((bal))) as Any?)
    (n)["Balance"] = (bal as Any?)
    return mutableMapOf<String, Any?>("node" to (single(root, dir)), "done" to (true))
}

fun removeR(root: Any?, data: Int): MutableMap<String, Any?> {
    var data: Int = data
    if (root == null) {
        return mutableMapOf<String, Any?>("node" to (null), "done" to (false))
    }
    var node: MutableMap<String, Any?> = (root as MutableMap<String, Any?>)
    if ((node)["Data"] as Int == data) {
        if (getLink(node, 0) == null) {
            return mutableMapOf<String, Any?>("node" to (getLink(node, 1)), "done" to (false))
        }
        if (getLink(node, 1) == null) {
            return mutableMapOf<String, Any?>("node" to (getLink(node, 0)), "done" to (false))
        }
        var heir: Any? = getLink(node, 0)
        while (getLink((heir as MutableMap<String, Any?>), 1) != null) {
            heir = getLink((heir as MutableMap<String, Any?>), 1)
        }
        (node)["Data"] = (((heir as MutableMap<String, Any?>)["Data"]) as Any?)
        data = (heir as MutableMap<String, Any?>)["Data"] as Int
    }
    var dir: Int = 0
    if ((node)["Data"] as Int < data) {
        dir = 1
    }
    var r: MutableMap<String, Any?> = removeR(getLink(node, dir), data)
    setLink(node, dir, (r)["node"] as Any?)
    if ((((r)["done"] as Any?) as Boolean)) {
        return mutableMapOf<String, Any?>("node" to (node), "done" to (true))
    }
    (node)["Balance"] = ((((node)["Balance"] as Int + 1) - (2 * dir)) as Any?)
    if (((node)["Balance"] as Any? == 1) || ((node)["Balance"] as Any? == (0 - 1))) {
        return mutableMapOf<String, Any?>("node" to (node), "done" to (true))
    }
    if ((node)["Balance"] as Any? == 0) {
        return mutableMapOf<String, Any?>("node" to (node), "done" to (false))
    }
    return removeBalance(node, dir)
}

fun Remove(tree: Any?, data: Int): Any? {
    var r: MutableMap<String, Any?> = removeR(tree, data)
    return (r)["node"] as Any?
}

fun indentStr(n: Int): String {
    var s: String = ""
    var i: Int = 0
    while (i < n) {
        s = s + " "
        i = i + 1
    }
    return s
}

fun dumpNode(node: Any?, indent: Int, comma: Boolean): Unit {
    var sp: String = indentStr(indent)
    if (node == null) {
        var line: String = sp + "null"
        if ((comma as Boolean)) {
            line = line + ","
        }
        println(line)
    } else {
        println(sp + "{")
        println(((indentStr(indent + 3) + "\"Data\": ") + ((node as MutableMap<String, Any?>)["Data"]).toString()) + ",")
        println(((indentStr(indent + 3) + "\"Balance\": ") + ((node as MutableMap<String, Any?>)["Balance"]).toString()) + ",")
        println(indentStr(indent + 3) + "\"Link\": [")
        dumpNode(getLink((node as MutableMap<String, Any?>), 0), indent + 6, true)
        dumpNode(getLink((node as MutableMap<String, Any?>), 1), indent + 6, false)
        println(indentStr(indent + 3) + "]")
        var end: String = sp + "}"
        if ((comma as Boolean)) {
            end = end + ","
        }
        println(end)
    }
}

fun dump(node: Any?, indent: Int): Unit {
    dumpNode(node, indent, false)
}

fun user_main(): Unit {
    var tree: Any? = null
    println("Empty tree:")
    dump(tree, 0)
    println("")
    println("Insert test:")
    tree = Insert(tree, 3)
    tree = Insert(tree, 1)
    tree = Insert(tree, 4)
    tree = Insert(tree, 1)
    tree = Insert(tree, 5)
    dump(tree, 0)
    println("")
    println("Remove test:")
    tree = Remove(tree, 3)
    tree = Remove(tree, 1)
    var t: MutableMap<String, Any?> = (tree as MutableMap<String, Any?>)
    (t)["Balance"] = (0 as Any?)
    tree = (t as Any?)
    dump(tree, 0)
}

fun main() {
    user_main()
}
