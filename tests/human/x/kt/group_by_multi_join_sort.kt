data class Nation(val n_nationkey: Int, val n_name: String)
data class Customer(
    val c_custkey: Int,
    val c_name: String,
    val c_acctbal: Double,
    val c_nationkey: Int,
    val c_address: String,
    val c_phone: String,
    val c_comment: String
)
data class Order(val o_orderkey: Int, val o_custkey: Int, val o_orderdate: String)
data class LineItem(
    val l_orderkey: Int,
    val l_returnflag: String,
    val l_extendedprice: Double,
    val l_discount: Double
)

data class Key(
    val c_custkey: Int,
    val c_name: String,
    val c_acctbal: Double,
    val c_address: String,
    val c_phone: String,
    val c_comment: String,
    val n_name: String
)
data class Result(
    val c_custkey: Int,
    val c_name: String,
    val revenue: Double,
    val c_acctbal: Double,
    val n_name: String,
    val c_address: String,
    val c_phone: String,
    val c_comment: String
)

fun main() {
    val nation = listOf(Nation(1, "BRAZIL"))
    val customer = listOf(
        Customer(
            1,
            "Alice",
            100.0,
            1,
            "123 St",
            "123-456",
            "Loyal"
        )
    )
    val orders = listOf(
        Order(1000, 1, "1993-10-15"),
        Order(2000, 1, "1994-01-02")
    )
    val lineitem = listOf(
        LineItem(1000, "R", 1000.0, 0.1),
        LineItem(2000, "N", 500.0, 0.0)
    )
    val startDate = "1993-10-01"
    val endDate = "1994-01-01"

    val rows = mutableListOf<Pair<Key, Double>>()
    for (c in customer) {
        for (o in orders.filter { it.o_custkey == c.c_custkey }) {
            if (o.o_orderdate < startDate || o.o_orderdate >= endDate) continue
            for (l in lineitem.filter { it.l_orderkey == o.o_orderkey && it.l_returnflag == "R" }) {
                val n = nation.find { it.n_nationkey == c.c_nationkey } ?: continue
                val key = Key(c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone, c.c_comment, n.n_name)
                val revenue = l.l_extendedprice * (1 - l.l_discount)
                rows.add(key to revenue)
            }
        }
    }

    val grouped = rows.groupBy({ it.first }, { it.second })
        .map { (k, list) ->
            k to list.sum()
        }
        .sortedByDescending { it.second }
        .map { (k, rev) ->
            Result(k.c_custkey, k.c_name, rev, k.c_acctbal, k.n_name, k.c_address, k.c_phone, k.c_comment)
        }
    println(grouped)
}
