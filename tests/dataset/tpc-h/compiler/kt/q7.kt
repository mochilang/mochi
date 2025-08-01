// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:06:09Z
data class Customer(var c_custkey: Int, var c_nationkey: Int)

data class Lineitem(var l_orderkey: Int, var l_suppkey: Int, var l_extendedprice: Double, var l_discount: Double, var l_shipdate: String)

data class Nation(var n_nationkey: Int, var n_name: String)

data class Order(var o_orderkey: Int, var o_custkey: Int)

data class Row0(var l: Lineitem, var o: Order, var c: Customer, var s: Supplier, var n1: Nation, var n2: Nation)

data class Supplier(var s_suppkey: Int, var s_nationkey: Int)

fun sum(list: List<Any?>): Number {
    var s = 0.0
    var allInt = true
    for (n in list) {
        val d = toDouble(n)
        if (d % 1.0 != 0.0) allInt = false
        s += d
    }
    return if (allInt) s.toInt() else s
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}

fun json(v: Any?) {
    println(toJson(v))
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

class Group<K, T>(val key: K, val items: MutableList<T>) : MutableList<T> by items
// Code generated from q7.mochi

val nation = mutableListOf(Nation(n_nationkey = 1, n_name = "FRANCE"), Nation(n_nationkey = 2, n_name = "GERMANY"))

val supplier = mutableListOf(Supplier(s_suppkey = 100, s_nationkey = 1))

val customer = mutableListOf(Customer(c_custkey = 200, c_nationkey = 2))

val orders = mutableListOf(Order(o_orderkey = 1000, o_custkey = 200))

val lineitem = mutableListOf(Lineitem(l_orderkey = 1000, l_suppkey = 100, l_extendedprice = 1000.0, l_discount = 0.1, l_shipdate = "1995-06-15"), Lineitem(l_orderkey = 1000, l_suppkey = 100, l_extendedprice = 800.0, l_discount = 0.05, l_shipdate = "1997-01-01"))

val start_date = "1995-01-01"

val end_date = "1996-12-31"

val nation1 = "FRANCE"

val nation2 = "GERMANY"

val result = run {
    val __groups = mutableMapOf<MutableMap<String, String>, Group<MutableMap<String, String>, Row0>>()
    val __order = mutableListOf<MutableMap<String, String>>()
    for (l in lineitem) {
        for (o in orders) {
            if (o.o_orderkey == l.l_orderkey) {
                for (c in customer) {
                    if (c.c_custkey == o.o_custkey) {
                        for (s in supplier) {
                            if (s.s_suppkey == l.l_suppkey) {
                                for (n1 in nation) {
                                    if (n1.n_nationkey == s.s_nationkey) {
                                        for (n2 in nation) {
                                            if (n2.n_nationkey == c.c_nationkey) {
                                                if ((l.l_shipdate >= start_date && l.l_shipdate <= end_date && (n1.n_name == nation1 && n2.n_name == nation2) || (n1.n_name == nation2 && n2.n_name == nation1))) {
                                                    val __k = (mutableMapOf("supp_nation" to (n1.n_name), "cust_nation" to (n2.n_name), "l_year" to (l.l_shipdate.substring(0, 4))) as MutableMap<String, String>)
                                                    var __g = __groups[__k]
                                                    if (__g == null) {
                                                        __g = Group(__k, mutableListOf<Row0>())
                                                        __groups[__k] = __g
                                                        __order.add(__k)
                                                    }
                                                    __g.add(Row0(l = l, o = o, c = c, s = s, n1 = n1, n2 = n2))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    __order.sortBy { k ->
        val g = __groups[k]!!
        mutableListOf(supp_nation, cust_nation, l_year) as Comparable<Any>
    }
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("supp_nation" to ((g.key as MutableMap<String, String>)["supp_nation"]), "cust_nation" to ((g.key as MutableMap<String, String>)["cust_nation"]), "l_year" to ((g.key as MutableMap<String, String>)["l_year"]), "revenue" to (sum(run {
    val __res = mutableListOf<Double>()
    for (x in g) {
        __res.add(x.l.l_extendedprice * ((1).toDouble() - x.l.l_discount))
    }
    __res
}))) as MutableMap<String, Any?>))
    }
    __res
}

fun main() {
    json(result)
}
