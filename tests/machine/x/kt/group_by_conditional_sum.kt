data class Item(val cat: String, val val_: Int, val flag: Boolean)

data class Share(val cat: String, val share: Double)

fun main() {
    val items = listOf(
        Item("a", 10, true),
        Item("a", 5, false),
        Item("b", 20, true)
    )
    val result = items.groupBy { it.cat }
        .toSortedMap()
        .map { (cat, list) ->
            val numerator = list.sumOf { if (it.flag) it.val_ else 0 }
            val denominator = list.sumOf { it.val_ }
            Share(cat, numerator.toDouble() / denominator)
        }
    println(result)
}
