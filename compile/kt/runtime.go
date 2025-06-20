package ktcode

const (
	helperCast = `inline fun <reified T> _cast(v: Any?): T {
    return when (T::class) {
        Int::class -> when (v) { is Number -> v.toInt(); is String -> v.toInt(); else -> 0 } as T
        Double::class -> when (v) { is Number -> v.toDouble(); is String -> v.toDouble(); else -> 0.0 } as T
        Boolean::class -> when (v) { is Boolean -> v; is String -> v == "true"; else -> false } as T
        String::class -> v.toString() as T
        else -> v as T
    }
}`

	helperLoad = `fun _load(path: String?, opts: Map<String, Any>?): List<Map<String, Any>> {
    var format = opts?.get("format") as? String ?: "csv"
    var header = opts?.get("header") as? Boolean ?: true
    var delim = (opts?.get("delimiter") as? String)?.firstOrNull() ?: ','
    if (format == "tsv") delim = '\t'
    val text = if (path == null || path == "-" || path == "") generateSequence(::readLine).joinToString("\n") else java.io.File(path).readText()
    if (format != "csv") return emptyList()
    val lines = text.trim().split(Regex("\\r?\\n")).filter { it.isNotEmpty() }
    if (lines.isEmpty()) return emptyList()
    val headers = if (header) lines[0].split(delim) else List(lines[0].split(delim).size) { "c$it" }
    val start = if (header) 1 else 0
    val out = mutableListOf<Map<String, Any>>()
    for (i in start until lines.size) {
        val parts = lines[i].split(delim)
        val row = mutableMapOf<String, Any>()
        for (j in headers.indices) {
            row[headers[j]] = parts.getOrElse(j) { "" }
        }
        out.add(row)
    }
    return out
}`

	helperSave = `fun _save(src: Any?, path: String?, opts: Map<String, Any>?): Unit {
    val rows = src as? List<Map<String, Any>> ?: return
    var format = opts?.get("format") as? String ?: "csv"
    var header = opts?.get("header") as? Boolean ?: false
    var delim = (opts?.get("delimiter") as? String)?.firstOrNull() ?: ','
    if (format == "tsv") delim = '\t'
    if (format != "csv") return
    val headers = if (rows.isNotEmpty()) rows[0].keys.sorted() else emptyList()
    val lines = mutableListOf<String>()
    if (header) lines.add(headers.joinToString(delim.toString()))
    for (row in rows) {
        lines.add(headers.joinToString(delim.toString()) { row[it].toString() })
    }
    val text = lines.joinToString("\n") + "\n"
    if (path == null || path == "-" || path == "") print(text) else java.io.File(path).writeText(text)
}`

	helperJson = `fun _json(v: Any?) {
    fun encode(x: Any?): String = when (x) {
        null -> "null"
        is String -> \"""${x.replace("\"", "\\\"")}\"""
        is Int, is Double, is Boolean -> x.toString()
        is List<*> -> x.joinToString(prefix = "[", postfix = "]") { encode(it) }
        is Map<*, *> -> x.entries.joinToString(prefix = "{", postfix = "}") { e ->
            "\"" + e.key.toString().replace("\"", "\\\"") + "\":" + encode(e.value)
        }
        else -> \"""${x.toString().replace("\"", "\\\"")}\"""
    }
    println(encode(v))
}`
)

var helperMap = map[string]string{
	"_cast": helperCast,
	"_load": helperLoad,
	"_save": helperSave,
	"_json": helperJson,
}
