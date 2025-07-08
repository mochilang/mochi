
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


val items = mutableListOf(mutableMapOf("cat" to "a", "val" to 3), mutableMapOf("cat" to "a", "val" to 1), mutableMapOf("cat" to "b", "val" to 5), mutableMapOf("cat" to "b", "val" to 2))

val grouped = run {
    val __res = mutableListOf<Any>()
    for (i in items) {
        __res.add(mutableMapOf("cat" to g.key, "total" to sum(run {
    val __res = mutableListOf<Any>()
    for (x in g) {
        __res.add(x.val)
    }
    __res
})))
    }
    __res
}.sortedByDescending { sum(run {
    val __res = mutableListOf<Any>()
    for (x in g) {
        __res.add(x.val)
    }
    __res
}) }

fun main() {
    println(grouped)
}
