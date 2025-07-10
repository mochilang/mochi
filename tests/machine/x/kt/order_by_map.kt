data class Entry(val a: Int, val b: Int)

fun main() {
    val data = listOf(
        Entry(1, 2),
        Entry(1, 1),
        Entry(0, 5)
    )
    val sorted = data.sortedWith(compareBy<Entry> { it.a }.thenBy { it.b })
    println(sorted)
}
