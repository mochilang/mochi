// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:23:32Z
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

fun String.starts_with(prefix: String): Boolean = this.startsWith(prefix)

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
// Code generated from q42.mochi

data class Date_dim(var d_date_sk: Int, var d_year: Int, var d_moy: Int)

data class Item(var i_item_sk: Int, var i_manager_id: Int, var i_category_id: Int, var i_category: String)

data class Store_sale(var sold_date_sk: Int, var item_sk: Int, var ext_sales_price: Double)

val store_sales = mutableListOf(Store_sale(sold_date_sk = 1, item_sk = 1, ext_sales_price = 10.0), Store_sale(sold_date_sk = 1, item_sk = 2, ext_sales_price = 20.0), Store_sale(sold_date_sk = 2, item_sk = 1, ext_sales_price = 15.0))

val item = mutableListOf(Item(i_item_sk = 1, i_manager_id = 1, i_category_id = 100, i_category = "CatA"), Item(i_item_sk = 2, i_manager_id = 1, i_category_id = 200, i_category = "CatB"))

val date_dim = mutableListOf(Date_dim(d_date_sk = 1, d_year = 2020, d_moy = 5), Date_dim(d_date_sk = 2, d_year = 2021, d_moy = 5))

val month = 5

val year = 2020

val records = run {
    val __res = mutableListOf<Any?>()
    for (dt in date_dim) {
        for (ss in store_sales) {
            if (ss.sold_date_sk == dt.d_date_sk) {
                for (it in item) {
                    if (ss.item_sk == it.i_item_sk) {
                        if (it.i_manager_id == 1 && dt.d_moy == month && dt.d_year == year) {
                            __res.add(mutableMapOf("d_year" to dt.d_year, "i_category_id" to it.i_category_id, "i_category" to it.i_category, "price" to ss.ext_sales_price))
                        }
                    }
                }
            }
        }
    }
    __res
}

val grouped = run {
    val __groups = mutableMapOf<MutableMap<String, Any?>, Group<MutableMap<String, Any?>, MutableMap<String, Int>>>()
    val __order = mutableListOf<MutableMap<String, Any?>>()
    for (r in records) {
        val __k = (mutableMapOf("d_year" to (r as MutableMap<String, Int>)["d_year"], "i_category_id" to (r as MutableMap<String, Int>)["i_category_id"], "i_category" to (r as MutableMap<String, Int>)["i_category"]) as MutableMap<String, Any?>)
        var __g = __groups[__k]
        if (__g == null) {
            __g = Group(__k, mutableListOf<MutableMap<String, Int>>())
            __groups[__k] = __g
            __order.add(__k)
        }
        __g.add(r)
    }
    val __res = mutableListOf<Any?>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(mutableMapOf("d_year" to (g.key as MutableMap<String, Any?>)["d_year"], "i_category_id" to (g.key as MutableMap<String, Any?>)["i_category_id"], "i_category" to (g.key as MutableMap<String, Any?>)["i_category"], "sum_ss_ext_sales_price" to sum(run {
    val __res = mutableListOf<Int>()
    for (x in g) {
        __res.add((x as MutableMap<String, Int>)["price"])
    }
    __res
})))
    }
    __res
}

val base = run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (g in grouped) {
        __res.add((g as MutableMap<String, Any?>))
    }
    __res
}.sortedByDescending { (it as MutableMap<String, Any?>)["sum_ss_ext_sales_price"] as Comparable<Any> }

val result = base

fun main() {
    json(result)
}
