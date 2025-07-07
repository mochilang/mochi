fun main() {
    val people = listOf(
        mapOf("name" to "Alice", "age" to 30),
        mapOf("name" to "Bob", "age" to 25)
    )
    for (p in people) {
        val line = p.entries.joinToString(prefix = "{", postfix = "}") { "\"${it.key}\": ${it.value}" }
        println(line)
    }
}
