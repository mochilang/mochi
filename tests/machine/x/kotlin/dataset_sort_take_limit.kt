
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


val products = mutableListOf(mutableMapOf("name" to "Laptop", "price" to 1500), mutableMapOf("name" to "Smartphone", "price" to 900), mutableMapOf("name" to "Tablet", "price" to 600), mutableMapOf("name" to "Monitor", "price" to 300), mutableMapOf("name" to "Keyboard", "price" to 100), mutableMapOf("name" to "Mouse", "price" to 50), mutableMapOf("name" to "Headphones", "price" to 200))

val expensive = run {
    val __res = mutableListOf<Any>()
    for (p in products) {
        __res.add(p)
    }
    __res
}.sortedByDescending { p["price"] }.drop(1).take(3)

fun main() {
    println("--- Top products (excluding most expensive) ---")
    for (item in expensive) {
        println(listOf(item.name, "costs $", item.price).joinToString(" "))
    }
}
