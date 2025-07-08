
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


val nations = mutableListOf(mutableMapOf("id" to 1, "name" to "A"), mutableMapOf("id" to 2, "name" to "B"))

val suppliers = mutableListOf(mutableMapOf("id" to 1, "nation" to 1), mutableMapOf("id" to 2, "nation" to 2))

val partsupp = mutableListOf(mutableMapOf("part" to 100, "supplier" to 1, "cost" to 10, "qty" to 2), mutableMapOf("part" to 100, "supplier" to 2, "cost" to 20, "qty" to 1), mutableMapOf("part" to 200, "supplier" to 1, "cost" to 5, "qty" to 3))

val filtered = run {
    val __res = mutableListOf<Any>()
    for (ps in partsupp) {
        if (n.name == "A") {
            __res.add(mutableMapOf("part" to ps["part"], "value" to ps["cost"] * ps["qty"]))
        }
    }
    __res
}

val grouped = run {
    val __res = mutableListOf<Any>()
    for (x in filtered) {
        __res.add(mutableMapOf("part" to g.key, "total" to sum(run {
    val __res = mutableListOf<Any>()
    for (r in g) {
        __res.add(r.value)
    }
    __res
})))
    }
    __res
}

fun main() {
    println(grouped)
}
