fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}

fun _load(path: String?, opts: Map<String, Any?>?): MutableList<MutableMap<String, Any?>> {
    val fmt = opts?.get("format") as? String ?: "csv"
    val lines = if (path == null || path == "-") {
        listOf<String>()
    } else {
        java.io.File(path).readLines()
    }
    return when (fmt) {
        "yaml" -> loadYamlSimple(lines)
        else -> mutableListOf()
    }
}

fun loadYamlSimple(lines: List<String>): MutableList<MutableMap<String, Any?>> {
    val res = mutableListOf<MutableMap<String, Any?>>()
    var cur: MutableMap<String, Any?>? = null
    for (ln in lines) {
        val t = ln.trim()
        if (t.startsWith("- ")) {
            cur?.let { res.add(it) }
            cur = mutableMapOf()
            val idx = t.indexOf(':', 2)
            if (idx >= 0) {
                val k = t.substring(2, idx).trim()
                val v = parseSimpleValue(t.substring(idx + 1))
                cur!![k] = v
            }
        } else if (t.contains(':')) {
            val idx = t.indexOf(':')
            val k = t.substring(0, idx).trim()
            val v = parseSimpleValue(t.substring(idx + 1))
            cur?.set(k, v)
        }
    }
    cur?.let { res.add(it) }
    return res
}

fun parseSimpleValue(s: String): Any? {
    val t = s.trim()
    return when {
        t.matches(Regex("^-?\\d+$")) -> t.toInt()
        t.matches(Regex("^-?\\d+\\.\\d+$")) -> t.toDouble()
        t.equals("true", true) -> true
        t.equals("false", true) -> false
        t.startsWith("\"") && t.endsWith("\"") -> t.substring(1, t.length - 1)
        else -> t
    }
}
data class Person(var name: String, var age: Int, var email: String)

data class Adult(var name: Any?, var email: Any?)

val people = _load("../interpreter/valid/people.yaml", mutableMapOf("format" to "yaml")).map { Person(name = it["name"] as String, age = it["age"] as Int, email = it["email"] as String) }.toMutableList()

val adults = run {
    val __res = mutableListOf<Adult>()
    for (p in people) {
        if (toBool(p.age >= 18)) {
            __res.add(Adult(name = p.name, email = p.email))
        }
    }
    __res
}

fun main() {
    for (a in adults) {
        println(listOf(a.name, a.email).joinToString(" "))
    }
}
