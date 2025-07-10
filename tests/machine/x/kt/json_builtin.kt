fun main() {
    val m = mapOf("a" to 1, "b" to 2)
    val json = m.entries.joinToString(prefix = "{", postfix = "}") { "\"${it.key}\": ${it.value}" }
    println(json)
}
