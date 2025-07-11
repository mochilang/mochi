fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items
data class Customer(var c_custkey: Int, var c_name: String, var c_acctbal: Double, var c_nationkey: Int, var c_address: String, var c_phone: String, var c_comment: String)

data class Order(var o_orderkey: Int, var o_custkey: Int, var o_orderdate: String)

data class Lineitem(var l_orderkey: Int, var l_returnflag: String, var l_extendedprice: Double, var l_discount: Double)

data class Result(var c_custkey: Any?, var c_name: Any?, var revenue: Int, var c_acctbal: Any?, var n_name: Any?, var c_address: Any?, var c_phone: Any?, var c_comment: Any?)

data class Nation(var n_nationkey: Int, var n_name: String)

val nation = mutableListOf(Nation(n_nationkey = 1, n_name = "BRAZIL"))

val customer = mutableListOf(Customer(c_custkey = 1, c_name = "Alice", c_acctbal = 100, c_nationkey = 1, c_address = "123 St", c_phone = "123-456", c_comment = "Loyal"))

val orders = mutableListOf(Order(o_orderkey = 1000, o_custkey = 1, o_orderdate = "1993-10-15"), Order(o_orderkey = 2000, o_custkey = 1, o_orderdate = "1994-01-02"))

val lineitem = mutableListOf(Lineitem(l_orderkey = 1000, l_returnflag = "R", l_extendedprice = 1000, l_discount = 0.1), Lineitem(l_orderkey = 2000, l_returnflag = "N", l_extendedprice = 500, l_discount = 0))

val start_date = "1993-10-01"

val end_date = "1994-01-01"

val result = run {
    val __groups = mutableMapOf<Any?, Group>()
    val __order = mutableListOf<Any?>()
    for (c in customer) {
        for (o in orders) {
            if (o.o_custkey == c.c_custkey) {
                for (l in lineitem) {
                    if (l.l_orderkey == o.o_orderkey) {
                        for (n in nation) {
                            if (n.n_nationkey == c.c_nationkey) {
                                if (o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R") {
                                    val __k = mutableMapOf("c_custkey" to c.c_custkey, "c_name" to c.c_name, "c_acctbal" to c.c_acctbal, "c_address" to c.c_address, "c_phone" to c.c_phone, "c_comment" to c.c_comment, "n_name" to n.n_name)
                                    var __g = __groups[__k]
                                    if (__g == null) {
                                        __g = Group(__k, mutableListOf())
                                        __groups[__k] = __g
                                        __order.add(__k)
                                    }
                                    __g.add(mutableMapOf("c" to c, "o" to o, "l" to l, "n" to n) as MutableMap<Any?, Any?>)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    val __res = mutableListOf<Result>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(Result(c_custkey = g.key.c_custkey, c_name = g.key.c_name, revenue = sum(run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (x in g) {
        __res.add((toDouble((x as MutableMap<*, *>)["l"]["l_extendedprice"]) * toDouble((1 - toInt((x as MutableMap<*, *>)["l"]["l_discount"]))) as MutableMap<String, Any?>))
    }
    __res
}), c_acctbal = g.key.c_acctbal, n_name = g.key.n_name, c_address = g.key.c_address, c_phone = g.key.c_phone, c_comment = g.key.c_comment))
    }
    __res
}.sortedByDescending { sum(run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (x in it) {
        __res.add((toDouble((x as MutableMap<*, *>)["it"]["l_extendedprice"]) * toDouble((1 - toInt((x as MutableMap<*, *>)["it"]["l_discount"]))) as MutableMap<String, Any?>))
    }
    __res
}) as Comparable<Any> }

fun main() {
    println(result)
}
