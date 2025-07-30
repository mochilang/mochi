import kastree.ast.psi.Parser
import kotlin.reflect.full.memberProperties

fun escape(s: String): String =
    s.replace("\\", "\\\\")
     .replace("\"", "\\\"")
     .replace("\n", "\\n")
     .replace("\r", "\\r")

fun dumpValue(v: Any?, sb: StringBuilder) {
    when (v) {
        null -> sb.append("null")
        is String -> sb.append('"').append(escape(v)).append('"')
        is Boolean, is Number -> sb.append(v.toString())
        is List<*> -> {
            sb.append('[')
            for (i in v.indices) {
                if (i > 0) sb.append(',')
                dumpValue(v[i], sb)
            }
            sb.append(']')
        }
        else -> dumpNode(v, sb)
    }
}

fun dumpNode(v: Any, sb: StringBuilder) {
    val cls = v.javaClass
    sb.append("{\"type\":\"").append(cls.simpleName).append("\"")
    for (prop in cls.kotlin.memberProperties) {
        val value = prop.getter.call(v)
        if (value == null) continue
        sb.append(",\"").append(prop.name).append("\":")
        dumpValue(value, sb)
    }
    sb.append('}')
}

fun main() {
    val src = generateSequence(::readLine).joinToString("\n")
    val file = Parser.parseFile(src)
    val sb = StringBuilder()
    dumpNode(file, sb)
    println("{\"file\":" + sb.toString() + "}")
}
