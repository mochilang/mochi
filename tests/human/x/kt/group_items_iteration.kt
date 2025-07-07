data class Data(val tag: String, val val_: Int)
data class Tot(val tag: String, val total: Int)

fun main() {
    val data = listOf(
        Data("a", 1),
        Data("a", 2),
        Data("b", 3)
    )
    val groups = data.groupBy { it.tag }
    val tmp = mutableListOf<Tot>()
    for ((tag, items) in groups) {
        var total = 0
        for (x in items) {
            total += x.val_
        }
        tmp.add(Tot(tag, total))
    }
    val result = tmp.sortedBy { it.tag }
    println(result)
}
