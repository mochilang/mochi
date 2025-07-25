// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:06:27Z
data class Lineitem(var l_orderkey: Int, var l_shipmode: String, var l_commitdate: String, var l_receiptdate: String, var l_shipdate: String)

data class Order(var o_orderkey: Int, var o_orderpriority: String)

data class Row0(var l: Lineitem, var o: Order)

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
// Code generated from q12.mochi

val orders = mutableListOf(Order(o_orderkey = 1, o_orderpriority = "1-URGENT"), Order(o_orderkey = 2, o_orderpriority = "3-MEDIUM"))

val lineitem = mutableListOf(Lineitem(l_orderkey = 1, l_shipmode = "MAIL", l_commitdate = "1994-02-10", l_receiptdate = "1994-02-15", l_shipdate = "1994-02-05"), Lineitem(l_orderkey = 2, l_shipmode = "SHIP", l_commitdate = "1994-03-01", l_receiptdate = "1994-02-28", l_shipdate = "1994-02-27"))

val result = run {
    val __groups = mutableMapOf<String, Group<String, Row0>>()
    val __order = mutableListOf<String>()
    for (l in lineitem) {
        for (o in orders) {
            if (o.o_orderkey == l.l_orderkey) {
                if ((l.l_shipmode in mutableListOf("MAIL", "SHIP")) && (l.l_commitdate < l.l_receiptdate) && (l.l_shipdate < l.l_commitdate) && (l.l_receiptdate >= "1994-01-01") && (l.l_receiptdate < "1995-01-01")) {
                    val __k = l.l_shipmode
                    var __g = __groups[__k]
                    if (__g == null) {
                        __g = Group(__k, mutableListOf<Row0>())
                        __groups[__k] = __g
                        __order.add(__k)
                    }
                    __g.add(Row0(l = l, o = o))
                }
            }
        }
    }
    __order.sortBy { k ->
        val g = __groups[k]!!
        g.key as String
    }
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add((mutableMapOf("l_shipmode" to (g.key), "high_line_count" to (sum(run {
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add(if (x.o.o_orderpriority in mutableListOf("1-URGENT", "2-HIGH")) 1 else 0)
    }
    __res
})), "low_line_count" to (sum(run {
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add(if (!(x.o.o_orderpriority in mutableListOf("1-URGENT", "2-HIGH"))) 1 else 0)
    }
    __res
}))) as MutableMap<String, Any?>))
    }
    __res
}

fun main() {
    json(result)
}
