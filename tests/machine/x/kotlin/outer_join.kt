// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:47:35Z
data class Customer(var id: Int, var name: String)

data class Order(var id: Int, var customerId: Int, var total: Int)

data class Row0(var order: Order?, var customer: Customer?)

data class Row1(var o: Order?, var c: Customer?)

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
// Code generated from outer_join.mochi

val customers = mutableListOf(Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"), Customer(id = 4, name = "Diana"))

val orders = mutableListOf(Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 5, total = 80))

val result = run {
    val __res = mutableListOf<Row0>()
    val __matched = mutableSetOf<Any?>()
    for (o in orders) {
        val __tmp = mutableListOf<Customer?>()
        for (c in customers) {
            if ((o as MutableMap<*, *>)["customerId"] == (c as MutableMap<*, *>)["id"]) {
                __tmp.add(c)
                __matched.add(c)
            }
        }
        if (__tmp.isEmpty()) __tmp.add(null)
        for (c in __tmp) {
            __res.add(Row0(order = o, customer = c))
        }
    }
    for (c in customers) {
        if (!__matched.contains(c)) {
            val o = null
            __res.add(Row0(order = o, customer = c))
        }
    }
    __res
}

fun main() {
    println("--- Outer Join using syntax ---")
    for (row in result) {
        if (toBool(row.order)) {
            if (toBool(row.customer)) {
                println(listOf("Order", (row.order as MutableMap<*, *>)["id"], "by", (row.customer as MutableMap<*, *>)["name"], "- $", (row.order as MutableMap<*, *>)["total"]).joinToString(" "))
            }
            else {
                println(listOf("Order", (row.order as MutableMap<*, *>)["id"], "by", "Unknown", "- $", (row.order as MutableMap<*, *>)["total"]).joinToString(" "))
            }
        }
        else {
            println(listOf("Customer", (row.customer as MutableMap<*, *>)["name"], "has no orders").joinToString(" "))
        }
    }
}

