
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


val customers = mutableListOf(mutableMapOf("id" to 1, "name" to "Alice"), mutableMapOf("id" to 2, "name" to "Bob"))

val orders = mutableListOf(mutableMapOf("id" to 100, "customerId" to 1, "total" to 250), mutableMapOf("id" to 101, "customerId" to 3, "total" to 80))

val result = run {
    val __res = mutableListOf<Any>()
    for (o in orders) {
        __res.add(mutableMapOf("orderId" to o["id"], "customer" to c, "total" to o["total"]))
    }
    __res
}

fun main() {
    println("--- Left Join ---")
    for (entry in result) {
        println(listOf("Order", entry.orderId, "customer", entry.customer, "total", entry.total).joinToString(" "))
    }
}
