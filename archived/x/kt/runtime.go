//go:build archived

package ktcode

const (
	helperCast = `import kotlin.reflect.full.primaryConstructor

inline fun <reified T> _cast(v: Any?): T {
    return when (T::class) {
        Int::class -> when (v) { is Number -> v.toInt(); is String -> v.toInt(); else -> 0 } as T
        Double::class -> when (v) { is Number -> v.toDouble(); is String -> v.toDouble(); else -> 0.0 } as T
        Boolean::class -> when (v) { is Boolean -> v; is String -> v == "true"; else -> false } as T
        String::class -> v.toString() as T
        else -> {
            if (v is Map<*, *>) {
                val ctor = T::class.primaryConstructor
                if (ctor != null) {
                    val args = ctor.parameters.associateWith { p ->
                        val value = v[p.name]
                        when (p.type.classifier) {
                            Int::class -> when (value) { is Number -> value.toInt(); is String -> value.toInt(); else -> 0 }
                            Double::class -> when (value) { is Number -> value.toDouble(); is String -> value.toDouble(); else -> 0.0 }
                            Boolean::class -> when (value) { is Boolean -> value; is String -> value == "true"; else -> false }
                            String::class -> value?.toString()
                            else -> value
                        }
                    }
                    return ctor.callBy(args)
                }
            }
            v as T
        }
    }
}`

	helperIndexString = `fun _indexString(s: String, i: Int): String {
    var idx = i
    val arr = s.toCharArray()
    if (idx < 0) idx += arr.size
    if (idx < 0 || idx >= arr.size) throw RuntimeException("index out of range")
    return arr[idx].toString()
}`

	helperSliceString = `fun _sliceString(s: String, i: Int, j: Int): String {
    var start = i
    var end = j
    val arr = s.toCharArray()
    val n = arr.size
    if (start < 0) start += n
    if (end < 0) end += n
    if (start < 0) start = 0
    if (end > n) end = n
    if (end < start) end = start
    return String(arr.sliceArray(start until end))
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

	helperFetch = `fun _fetch(url: String, opts: Map<String, Any>?): Any? {
    fun encode(x: Any?): String = when (x) {
        null -> "null"
        is String -> "\"" + x.replace("\"", "\\\"") + "\""
        is Int, is Double, is Boolean -> x.toString()
        is List<*> -> x.joinToString(prefix = "[", postfix = "]") { encode(it) }
        is Map<*, *> -> x.entries.joinToString(prefix = "{", postfix = "}") { e ->
            "\"" + e.key.toString().replace("\"", "\\\"") + "\":" + encode(e.value)
        }
        else -> "\"" + x.toString().replace("\"", "\\\"") + "\""
    }

    var full = url
    val method = opts?.get("method")?.toString() ?: "GET"
    if (opts?.get("query") is Map<*, *>) {
        val q = opts["query"] as Map<*, *>
        val qs = q.entries.joinToString("&") { e ->
            java.net.URLEncoder.encode(e.key.toString(), java.nio.charset.StandardCharsets.UTF_8) +
            "=" +
            java.net.URLEncoder.encode(e.value.toString(), java.nio.charset.StandardCharsets.UTF_8)
        }
        val sep = if (full.contains("?")) "&" else "?"
        full += sep + qs
    }

    val builder = java.net.http.HttpRequest.newBuilder(java.net.URI.create(full))
    val body = opts?.get("body")
    if (body != null) {
        builder.method(method, java.net.http.HttpRequest.BodyPublishers.ofString(encode(body)))
    } else {
        builder.method(method, java.net.http.HttpRequest.BodyPublishers.noBody())
    }
    if (opts?.get("headers") is Map<*, *>) {
        val hs = opts["headers"] as Map<*, *>
        for ((k, v) in hs) {
            builder.header(k.toString(), v.toString())
        }
    }

    val clientBuilder = java.net.http.HttpClient.newBuilder()
    if (opts?.get("timeout") != null) {
        val t = opts["timeout"]
        val secs = when (t) {
            is Int -> t.toLong()
            is Long -> t
            is Double -> t.toLong()
            is Float -> t.toLong()
            else -> null
        }
        if (secs != null) {
            clientBuilder.connectTimeout(java.time.Duration.ofMillis((secs * 1000).toLong()))
        }
    }
    val client = clientBuilder.build()

    return try {
        val resp = client.send(builder.build(), java.net.http.HttpResponse.BodyHandlers.ofString())
        val text = resp.body()
        val eng = javax.script.ScriptEngineManager().getEngineByName("javascript")
        eng.eval("Java.asJSONCompatible($text)")
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

	helperConcat = `fun <T> _concat(a: List<T>, b: List<T>): List<T> = a + b`

	helperArrConcat = `fun _arrConcat(a: Array<Any?>, b: Array<Any?>): Array<Any?> {
    return a + b
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

	helperAvg = `fun _avg(v: Any?): Double {
    var list: List<Any?>? = null
    when (v) {
        is List<*> -> list = v as List<Any?>
        is Map<*, *> -> {
            val items = when {
                v["items"] is List<*> -> v["items"] as List<*>
                v["Items"] is List<*> -> v["Items"] as List<*>
                else -> null
            }
            if (items != null) list = items as List<Any?>
        }
        is _Group -> list = v.Items
    }
    if (list == null || list.isEmpty()) return 0.0
    var sum = 0.0
    for (n in list!!) {
        sum += (n as Number).toDouble()
    }
    return sum / list!!.size
}`

	helperSum = `fun _sum(v: Any?): Double {
    var list: List<Any?>? = null
    when (v) {
        is List<*> -> list = v as List<Any?>
        is Map<*, *> -> {
            val items = when {
                v["items"] is List<*> -> v["items"] as List<*>
                v["Items"] is List<*> -> v["Items"] as List<*>
                else -> null
            }
            if (items != null) list = items as List<Any?>
        }
        is _Group -> list = v.Items
    }
    if (list == null || list.isEmpty()) return 0.0
    var sum = 0.0
    for (n in list!!) {
        sum += (n as Number).toDouble()
    }
    return sum
}`

	helperMin = `fun _min(v: Any?): Any? {
    var list: List<Any?>? = null
    when (v) {
        is List<*> -> list = v as List<Any?>
        is Map<*, *> -> {
            val items = when {
                v["items"] is List<*> -> v["items"] as List<*>
                v["Items"] is List<*> -> v["Items"] as List<*>
                else -> null
            }
            if (items != null) list = items as List<Any?>
        }
        is _Group -> list = v.Items
    }
    if (list == null || list.isEmpty()) return 0
    var m = list[0]
    for (n in list!!) {
        if ((n as Comparable<Any?>) < (m as Comparable<Any?>)) m = n
    }
    return m
}`

	helperMax = `fun _max(v: Any?): Any? {
    var list: List<Any?>? = null
    when (v) {
        is List<*> -> list = v as List<Any?>
        is Map<*, *> -> {
            val items = when {
                v["items"] is List<*> -> v["items"] as List<*>
                v["Items"] is List<*> -> v["Items"] as List<*>
                else -> null
            }
            if (items != null) list = items as List<Any?>
        }
        is _Group -> list = v.Items
    }
    if (list == null || list.isEmpty()) return 0
    var m = list[0]
    for (n in list!!) {
        if ((n as Comparable<Any?>) > (m as Comparable<Any?>)) m = n
    }
    return m
}`

	helperGroup = `class _Group(var key: Any?) {
    val Items = mutableListOf<Any?>()
    val size: Int
        get() = Items.size
}`

	helperGroupBy = `fun _group_by(src: List<Any?>, keyfn: (Any?) -> Any?): List<_Group> {
    val groups = mutableMapOf<String, _Group>()
    val order = mutableListOf<String>()
    for (it in src) {
        val key = keyfn(it)
        val ks = key.toString()
        var g = groups[ks]
        if (g == null) {
            g = _Group(key)
            groups[ks] = g
            order.add(ks)
        }
        if (it is Array<*> && it.size == 1) {
            g.Items.add(it[0])
        } else {
            g.Items.add(it)
        }
    }
        return order.map { groups[it]!! }
}`

	helperQuery = `data class _JoinSpec(
    val items: List<Any?>,
    val on: ((Array<Any?>) -> Boolean)? = null,
    val left: Boolean = false,
    val right: Boolean = false,
)

data class _QueryOpts(
    val selectFn: (Array<Any?>) -> Any?,
    val where: ((Array<Any?>) -> Boolean)? = null,
    val sortKey: ((Array<Any?>) -> Any?)? = null,
    val skip: Int = -1,
    val take: Int = -1,
)

fun _query(src: List<Any?>, joins: List<_JoinSpec>, opts: _QueryOpts): List<Any?> {
    var items = src.map { arrayOf(it) }.toMutableList()
    if (opts.where != null) {
        items = items.filter { opts.where.invoke(it) }.toMutableList()
    }
    for (j in joins) {
        val joined = mutableListOf<Array<Any?>>()
        if (j.right && j.left) {
            val matched = BooleanArray(j.items.size)
            for (left in items) {
                var m = false
                for ((ri, right) in j.items.withIndex()) {
                    var keep = true
                    if (j.on != null) {
                    keep = j.on.invoke(_arrConcat(left, arrayOf(right)))
                    }
                    if (!keep) continue
                    m = true
                    matched[ri] = true
                    joined.add(_arrConcat(left, arrayOf(right)))
                }
                if (!m) joined.add(_arrConcat(left, arrayOf<Any?>(null)))
            }
            for ((ri, right) in j.items.withIndex()) {
                if (!matched[ri]) {
                    val undef = Array<Any?>(items.firstOrNull()?.size ?: 0) { null }
                    joined.add(_arrConcat(undef, arrayOf(right)))
                }
            }
        } else if (j.right) {
            for (right in j.items) {
                var m = false
                for (left in items) {
                    var keep = true
                    if (j.on != null) {
                    keep = j.on.invoke(_arrConcat(left, arrayOf(right)))
                    }
                    if (!keep) continue
                    m = true
                    joined.add(_arrConcat(left, arrayOf(right)))
                }
                if (!m) {
                    val undef = Array<Any?>(items.firstOrNull()?.size ?: 0) { null }
                    joined.add(_arrConcat(undef, arrayOf(right)))
                }
            }
        } else {
            for (left in items) {
                var m = false
                for (right in j.items) {
                    var keep = true
                    if (j.on != null) {
                    keep = j.on.invoke(_arrConcat(left, arrayOf(right)))
                    }
                    if (!keep) continue
                    m = true
                    joined.add(_arrConcat(left, arrayOf(right)))
                }
                if (j.left && !m) joined.add(_arrConcat(left, arrayOf<Any?>(null)))
            }
        }
        items = joined
        if (opts.where != null) {
            items = items.filter { opts.where.invoke(it) }.toMutableList()
        }
    }
    if (opts.where != null) {
        items = items.filter { opts.where.invoke(it) }.toMutableList()
    }
    if (opts.sortKey != null) {
        val pairs = items.map { it to opts.sortKey.invoke(it) }.toMutableList()
        pairs.sortWith(java.util.Comparator { a, b ->
            val av = a.second
            val bv = b.second
            when (av) {
                is Int -> when (bv) {
                    is Int -> av.compareTo(bv)
                    is Double -> av.toDouble().compareTo(bv)
                    else -> av.toString().compareTo(bv.toString())
                }
                is Double -> when (bv) {
                    is Int -> av.compareTo(bv.toDouble())
                    is Double -> av.compareTo(bv)
                    else -> av.toString().compareTo(bv.toString())
                }
                is String -> av.compareTo(bv.toString())
                else -> av.toString().compareTo(bv.toString())
            }
        })
        items = pairs.map { it.first }.toMutableList()
    }
    if (opts.skip >= 0) {
        items = if (opts.skip < items.size) items.drop(opts.skip).toMutableList() else mutableListOf()
    }
    if (opts.take >= 0) {
        if (opts.take < items.size) items = items.take(opts.take).toMutableList()
    }
    val res = mutableListOf<Any?>()
    for (r in items) {
        res.add(opts.selectFn.invoke(r))
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
	"_cast":        helperCast,
	"_indexString": helperIndexString,
	"_sliceString": helperSliceString,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_eval":        helperEval,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_fetch":       helperFetch,
	"_json":        helperJson,
	"_concat":      helperConcat,
	"_arrConcat":   helperArrConcat,
	"_extern":      helperExtern,
	"_unionAll":    helperUnionAll,
	"_union":       helperUnion,
	"_except":      helperExcept,
	"_intersect":   helperIntersect,
	"_avg":         helperAvg,
	"_sum":         helperSum,
	"_min":         helperMin,
	"_max":         helperMax,
	"_Group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_query":       helperQuery,
	"_Stream":      helperStream,
	"_waitAll":     helperWaitAll,
}
