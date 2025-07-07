data class Item(val n: Int, val v: String)

fun main() {
    val items = listOf(
        Item(1, "a"),
        Item(1, "b"),
        Item(2, "c")
    )
    val result = items.sortedBy { it.n }.map { it.v }
    println(result)
}
