var tr: Any? = null
var i: Int = 1
fun node(cl: String, le: Any?, aa: Int, ri: Any?): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("cl" to (cl), "le" to (le), "aa" to (aa), "ri" to (ri))
}

fun treeString(t: Any?): String {
    if (t == null) {
        return "E"
    }
    val m: MutableMap<String, Any?> = t as MutableMap<String, Any?>
    return ((((((("T(" + ((m)["cl"] as Any?).toString()) + ", ") + treeString((m)["le"] as Any?)) + ", ") + ((m)["aa"] as Any?).toString()) + ", ") + treeString((m)["ri"] as Any?)) + ")"
}

fun balance(t: Any?): Any? {
    if (t == null) {
        return t as Any?
    }
    val m: MutableMap<String, Any?> = t as MutableMap<String, Any?>
    if ((m)["cl"] as Any? != "B") {
        return t as Any?
    }
    val le: Any? = (m)["le"] as Any?
    val ri: Any? = (m)["ri"] as Any?
    if (le != null) {
        val leMap: MutableMap<String, Any?> = le as MutableMap<String, Any?>
        if ((leMap)["cl"] as Any? == "R") {
            val lele: Any? = (leMap)["le"] as Any?
            if (lele != null) {
                val leleMap: MutableMap<String, Any?> = lele as MutableMap<String, Any?>
                if ((leleMap)["cl"] as Any? == "R") {
                    return node("R", node("B", (leleMap)["le"] as Any?, (leleMap)["aa"] as Any? as Int, (leleMap)["ri"] as Any?), (leMap)["aa"] as Any? as Int, node("B", (leMap)["ri"] as Any?, (m)["aa"] as Any? as Int, ri)) as Any?
                }
            }
            val leri: Any? = (leMap)["ri"] as Any?
            if (leri != null) {
                val leriMap: MutableMap<String, Any?> = leri as MutableMap<String, Any?>
                if ((leriMap)["cl"] as Any? == "R") {
                    return node("R", node("B", (leMap)["le"] as Any?, (leMap)["aa"] as Any? as Int, (leriMap)["le"] as Any?), (leriMap)["aa"] as Any? as Int, node("B", (leriMap)["ri"] as Any?, (m)["aa"] as Any? as Int, ri)) as Any?
                }
            }
        }
    }
    if (ri != null) {
        val riMap: MutableMap<String, Any?> = ri as MutableMap<String, Any?>
        if ((riMap)["cl"] as Any? == "R") {
            val rile: Any? = (riMap)["le"] as Any?
            if (rile != null) {
                val rileMap: MutableMap<String, Any?> = rile as MutableMap<String, Any?>
                if ((rileMap)["cl"] as Any? == "R") {
                    return node("R", node("B", (m)["le"] as Any?, (m)["aa"] as Any? as Int, (rileMap)["le"] as Any?), (rileMap)["aa"] as Any? as Int, node("B", (rileMap)["ri"] as Any?, (riMap)["aa"] as Any? as Int, (riMap)["ri"] as Any?)) as Any?
                }
            }
            val riri: Any? = (riMap)["ri"] as Any?
            if (riri != null) {
                val ririMap: MutableMap<String, Any?> = riri as MutableMap<String, Any?>
                if ((ririMap)["cl"] as Any? == "R") {
                    return node("R", node("B", (m)["le"] as Any?, (m)["aa"] as Any? as Int, (riMap)["le"] as Any?), (riMap)["aa"] as Any? as Int, node("B", (ririMap)["le"] as Any?, (ririMap)["aa"] as Any? as Int, (ririMap)["ri"] as Any?)) as Any?
                }
            }
        }
    }
    return t as Any?
}

fun ins(tr: Any?, x: Int): Any? {
    if (tr == null) {
        return node("R", null, x, null) as Any?
    }
    if (x < ((tr as MutableMap<String, Any?>)["aa"] as Number).toDouble()) {
        return balance(node((tr as MutableMap<String, Any?>)["cl"] as String, ins((tr as MutableMap<String, Any?>)["le"], x), (tr as MutableMap<String, Any?>)["aa"] as Int, (tr as MutableMap<String, Any?>)["ri"]))
    }
    if (x > ((tr as MutableMap<String, Any?>)["aa"] as Number).toDouble()) {
        return balance(node((tr as MutableMap<String, Any?>)["cl"] as String, (tr as MutableMap<String, Any?>)["le"], (tr as MutableMap<String, Any?>)["aa"] as Int, ins((tr as MutableMap<String, Any?>)["ri"], x)))
    }
    return tr as Any?
}

fun insert(tr: Any?, x: Int): Any? {
    val t: Any? = ins(tr, x)
    if (t == null) {
        return null as Any?
    }
    val m: MutableMap<String, Any?> = t as MutableMap<String, Any?>
    return node("B", (m)["le"] as Any?, (m)["aa"] as Any? as Int, (m)["ri"] as Any?) as Any?
}

fun main() {
    while (i <= 16) {
        tr = insert(tr, i)
        i = i + 1
    }
    println(treeString(tr))
}
