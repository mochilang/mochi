
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


val people = mutableListOf(mutableMapOf("name" to "Alice", "age" to 30, "city" to "Paris"), mutableMapOf("name" to "Bob", "age" to 15, "city" to "Hanoi"), mutableMapOf("name" to "Charlie", "age" to 65, "city" to "Paris"), mutableMapOf("name" to "Diana", "age" to 45, "city" to "Hanoi"), mutableMapOf("name" to "Eve", "age" to 70, "city" to "Paris"), mutableMapOf("name" to "Frank", "age" to 22, "city" to "Hanoi"))

val stats = run {
    val __res = mutableListOf<Any>()
    for (person in people) {
        __res.add(mutableMapOf("city" to g.key, "count" to count(g), "avg_age" to avg(run {
    val __res = mutableListOf<Any>()
    for (p in g) {
        __res.add(p.age)
    }
    __res
})))
    }
    __res
}

fun main() {
    println("--- People grouped by city ---")
    for (s in stats) {
        println(listOf(s.city, ": count =", s.count, ", avg_age =", s.avg_age).joinToString(" "))
    }
}
