data class Nation(val id: Int, val name: String)
data class Supplier(val id: Int, val nation: Int)
data class PartSupp(val part: Int, val supplier: Int, val cost: Double, val qty: Int)

data class Filtered(val part: Int, val value: Double)
data class Result(val part: Int, val total: Double)

fun main() {
    val nations = listOf(
        Nation(1, "A"),
        Nation(2, "B")
    )
    val suppliers = listOf(
        Supplier(1, 1),
        Supplier(2, 2)
    )
    val partsupp = listOf(
        PartSupp(100, 1, 10.0, 2),
        PartSupp(100, 2, 20.0, 1),
        PartSupp(200, 1, 5.0, 3)
    )
    val filtered = mutableListOf<Filtered>()
    for (ps in partsupp) {
        val s = suppliers.find { it.id == ps.supplier } ?: continue
        val n = nations.find { it.id == s.nation }
        if (n != null && n.name == "A") {
            filtered.add(Filtered(ps.part, ps.cost * ps.qty))
        }
    }
    val grouped = filtered.groupBy { it.part }
        .map { (part, list) -> Result(part, list.sumOf { it.value }) }
    println(grouped)
}
