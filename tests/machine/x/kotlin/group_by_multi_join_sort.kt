
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}

fun avg(list: List<Number>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += n.toDouble()
    return s / list.size
}

fun count(list: Collection<Any?>): Int = list.size

fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}

fun max(list: List<Int>): Int {
    var m = Int.MIN_VALUE
    for (n in list) if (n > m) m = n
    return if (m == Int.MIN_VALUE) 0 else m
}

fun min(list: List<Int>): Int {
    var m = Int.MAX_VALUE
    for (n in list) if (n < m) m = n
    return if (m == Int.MAX_VALUE) 0 else m
}

fun sum(list: List<Int>): Int = list.sum()

fun str(v: Any?): String = v.toString()

fun substring(s: String, start: Int, end: Int): String = s.substring(start, end)

fun <T> union(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = a.toMutableList()
    for (x in b) if (!res.contains(x)) res.add(x)
    return res
}

fun <T> except(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (!b.contains(x)) res.add(x)
    return res
}

fun <T> intersect(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (b.contains(x)) res.add(x)
    return res
}


val nation = mutableListOf(mutableMapOf("n_nationkey" to 1, "n_name" to "BRAZIL"))

val customer = mutableListOf(mutableMapOf("c_custkey" to 1, "c_name" to "Alice", "c_acctbal" to 100, "c_nationkey" to 1, "c_address" to "123 St", "c_phone" to "123-456", "c_comment" to "Loyal"))

val orders = mutableListOf(mutableMapOf("o_orderkey" to 1000, "o_custkey" to 1, "o_orderdate" to "1993-10-15"), mutableMapOf("o_orderkey" to 2000, "o_custkey" to 1, "o_orderdate" to "1994-01-02"))

val lineitem = mutableListOf(mutableMapOf("l_orderkey" to 1000, "l_returnflag" to "R", "l_extendedprice" to 1000, "l_discount" to 0.1), mutableMapOf("l_orderkey" to 2000, "l_returnflag" to "N", "l_extendedprice" to 500, "l_discount" to 0))

val start_date = "1993-10-01"

val end_date = "1994-01-01"

val result = run {
    val __res = mutableListOf<Any>()
    for (c in customer) {
        if (o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R") {
            __res.add(mutableMapOf("c_custkey" to g.key.c_custkey, "c_name" to g.key.c_name, "revenue" to sum(run {
    val __res = mutableListOf<Any>()
    for (x in g) {
        __res.add(x.l.l_extendedprice * (1 - x.l.l_discount))
    }
    __res
}), "c_acctbal" to g.key.c_acctbal, "n_name" to g.key.n_name, "c_address" to g.key.c_address, "c_phone" to g.key.c_phone, "c_comment" to g.key.c_comment))
        }
    }
    __res
}.sortedByDescending { sum(run {
    val __res = mutableListOf<Any>()
    for (x in g) {
        __res.add(x.l.l_extendedprice * (1 - x.l.l_discount))
    }
    __res
}) }

fun main() {
    println(result)
}
