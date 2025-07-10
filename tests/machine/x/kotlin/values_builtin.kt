fun <T> values(m: Map<*, T>): MutableList<T> = m.values.toMutableList()
val m = mutableMapOf("a" to 1, "b" to 2, "c" to 3)

fun main() {
    println(values(m))
}
