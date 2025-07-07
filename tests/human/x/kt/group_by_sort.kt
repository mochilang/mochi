data class Item(val cat: String, val value: Int)
data class Grouped(val cat: String, val total: Int)

fun main() {
    val items = listOf(
        Item("a", 3),
        Item("a", 1),
        Item("b", 5),
        Item("b", 2)
    )
    val grouped = items.groupBy { it.cat }
        .map { (cat, list) -> Grouped(cat, list.sumOf { it.value }) }
        .sortedByDescending { it.total }
    println(grouped)
}
