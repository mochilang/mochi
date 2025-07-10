fun _save(rows: List<Any?>, path: String?, opts: Map<String, Any?>?) {
    val fmt = opts?.get("format") as? String ?: "csv"
    val writer = if (path == null || path == "-") {
        java.io.BufferedWriter(java.io.OutputStreamWriter(System.out))
    } else {
        java.io.File(path).bufferedWriter()
    }
    if (fmt == "jsonl") {
        for (r in rows) {
            writer.write(toJson(r))
            writer.newLine()
        }
    }
    if (path != null && path != "-") writer.close()
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}
data class People(var name: String, var age: Int)

val people = mutableListOf(People(name = "Alice", age = 30), People(name = "Bob", age = 25))

fun main() {
    _save(people, "-", mutableMapOf("format" to "jsonl"))
}
