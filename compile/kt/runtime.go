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
    if (format == "jsonl") {
        val eng = javax.script.ScriptEngineManager().getEngineByName("javascript")
        val out = mutableListOf<Map<String, Any>>()
        for (line in text.trim().split(Regex("\\r?\\n"))) {
            if (line.isBlank()) continue
            val obj = eng.eval("Java.asJSONCompatible($line)") as java.util.Map<*, *>
            out.add(obj as Map<String, Any>)
        }
        return out
    }
    if (format == "json") {
        val eng = javax.script.ScriptEngineManager().getEngineByName("javascript")
        val obj = eng.eval("Java.asJSONCompatible($text)")
        when (obj) {
            is java.util.Map<*, *> -> return listOf(obj as Map<String, Any>)
            is java.util.List<*> -> return obj.map { it as Map<String, Any> }
        }
        return emptyList()
    }
    if (format == "yaml") {
        val rows = mutableListOf<Map<String, Any>>()
        var current = mutableMapOf<String, Any>()
        fun finish() {
            if (current.isNotEmpty()) { rows.add(current); current = mutableMapOf() }
        }
        for (line in text.trim().split(Regex("\\r?\\n"))) {
            val l = line.trim()
            if (l.startsWith("- ")) {
                finish()
                val idx = l.indexOf(':', 2)
                if (idx > 0) {
                    val k = l.substring(2, idx).trim()
                    val v = l.substring(idx + 1).trim()
                    current[k] = v.toIntOrNull() ?: v.trim('"')
                }
            } else if (l.contains(':')) {
                val idx = l.indexOf(':')
                val k = l.substring(0, idx).trim()
                val v = l.substring(idx + 1).trim()
                current[k] = v.toIntOrNull() ?: v.trim('"')
            }
        }
        finish()
        return rows
    }
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
    fun encode(x: Any?): String = when (x) {
        null -> "null"
        is String -> \"""${x.replace("\"", "\\\"")}\"""
        is Int, is Double, is Boolean -> x.toString()
        is List<*> -> x.joinToString(prefix = "[", postfix = "]") { encode(it) }
        is Map<*, *> -> x.entries.joinToString(prefix = "{", postfix = "}") { e -> "\"" + e.key.toString().replace("\"", "\\\"") + "\":" + encode(e.value) }
        else -> \"""${x.toString().replace("\"", "\\\"")}\"""
    }
    val text = when (format) {
        "jsonl" -> rows.joinToString("") { encode(it) + "\n" }
        "json" -> if (rows.size == 1) encode(rows[0]) + "\n" else rows.joinToString(prefix = "[", postfix = "]\n") { encode(it) }
        "yaml" -> {
            val sb = StringBuilder()
            for (row in rows) {
                val keys = row.keys.sorted()
                sb.append("- ")
                var first = true
                for (k in keys) {
                    val v = encode(row[k])
                    if (first) {
                        sb.append("$k: $v\n")
                        first = false
                    } else {
                        sb.append("  $k: $v\n")
                    }
                }
            }
            sb.toString()
        }
        else -> {
            if (format != "csv") return
            val headers = if (rows.isNotEmpty()) rows[0].keys.sorted() else emptyList()
            val lines = mutableListOf<String>()
            if (header) lines.add(headers.joinToString(delim.toString()))
            for (row in rows) {
                lines.add(headers.joinToString(delim.toString()) { row[it].toString() })
            }
            lines.joinToString("\n") + "\n"
        }
    }
    if (path == null || path == "-" || path == "") print(text) else java.io.File(path).writeText(text)
}`

	helperEval = `fun _eval(code: String): Any? {
    val engine = javax.script.ScriptEngineManager().getEngineByExtension("kts")
        ?: return null
    return engine.eval(code)
}`

	helperGenText = `fun _genText(prompt: String, model: String?, params: Map<String, Any>?): String {
    return prompt
}`

	helperGenEmbed = `fun _genEmbed(text: String, model: String?, params: Map<String, Any>?): List<Double> {
    return text.map { it.code.toDouble() }
}`

	helperGenStruct = `inline fun <reified T : Any> _genStruct(prompt: String, model: String?, params: Map<String, Any>?): T {
    return T::class.java.getDeclaredConstructor().newInstance()
}`

	helperFetch = `fun _fetch(url: String, opts: Map<String, Any>?): Map<String, Any> {
    return try {
        val client = java.net.http.HttpClient.newHttpClient()
        val req = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).build()
        val resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
        mutableMapOf<String, Any>("status" to resp.statusCode(), "body" to resp.body())
    } catch (e: Exception) {
        throw RuntimeException(e)
    }
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

	helperUnionAll = `fun <T> _unionAll(a: List<T>, b: List<T>): List<T> = a + b`

	helperUnion = `fun <T> _union(a: List<T>, b: List<T>): List<T> {
    val res = a.toMutableList()
    for (it in b) {
        if (!res.contains(it)) {
            res.add(it)
        }
    }
    return res
}`

	helperExcept = `fun <T> _except(a: List<T>, b: List<T>): List<T> {
    val res = mutableListOf<T>()
    for (it in a) {
        if (!b.contains(it)) {
            res.add(it)
        }
    }
    return res
}`

	helperIntersect = `fun <T> _intersect(a: List<T>, b: List<T>): List<T> {
    val res = mutableListOf<T>()
    for (it in a) {
        if (b.contains(it) && !res.contains(it)) {
            res.add(it)
        }
    }
    return res
}`

	helperStream = `class _Stream<T>(val name: String) {
    private val handlers = mutableListOf<(T) -> Unit>()
    fun append(data: T) {
        for (h in handlers.toList()) { h(data) }
    }
    fun register(handler: (T) -> Unit) { handlers.add(handler) }
}`

	helperExtern = `object ExternRegistry {
    private val externObjects = mutableMapOf<String, Any?>()
    fun registerExtern(name: String, obj: Any?) { externObjects[name] = obj }
    fun _externGet(name: String): Any? =
        externObjects[name] ?: throw RuntimeException("extern object not registered: $name")
}`

	helperWaitAll = `fun _waitAll() {}`
)

var helperMap = map[string]string{
	"_cast":      helperCast,
	"_load":      helperLoad,
	"_save":      helperSave,
	"_eval":      helperEval,
	"_genText":   helperGenText,
	"_genEmbed":  helperGenEmbed,
	"_genStruct": helperGenStruct,
	"_fetch":     helperFetch,
	"_json":      helperJson,
	"_extern":    helperExtern,
	"_unionAll":  helperUnionAll,
	"_union":     helperUnion,
	"_except":    helperExcept,
	"_intersect": helperIntersect,
	"_Stream":    helperStream,
	"_waitAll":   helperWaitAll,
}
