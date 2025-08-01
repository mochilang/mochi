// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:21:58Z
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
// Code generated from q14.mochi

data class StoreSale(var ss_item_sk: Int, var ss_list_price: Double, var ss_quantity: Int, var ss_sold_date_sk: Int)

data class CatalogSale(var cs_item_sk: Int, var cs_list_price: Double, var cs_quantity: Int, var cs_sold_date_sk: Int)

data class WebSale(var ws_item_sk: Int, var ws_list_price: Double, var ws_quantity: Int, var ws_sold_date_sk: Int)

data class Item(var i_item_sk: Int, var i_brand_id: Int, var i_class_id: Int, var i_category_id: Int)

data class DateDim(var d_date_sk: Int, var d_year: Int, var d_moy: Int)

data class Catalog_sale(var cs_item_sk: Int, var cs_list_price: Double, var cs_quantity: Int, var cs_sold_date_sk: Int)

data class Cross_item(var ss_item_sk: Int)

data class Date_dim(var d_date_sk: Int, var d_year: Int, var d_moy: Int)

data class Item(var i_item_sk: Int, var i_brand_id: Int, var i_class_id: Int, var i_category_id: Int)

data class Store_sale(var ss_item_sk: Int, var ss_list_price: Double, var ss_quantity: Int, var ss_sold_date_sk: Int)

data class Web_sale(var ws_item_sk: Int, var ws_list_price: Double, var ws_quantity: Int, var ws_sold_date_sk: Int)

val store_sales = mutableListOf(Store_sale(ss_item_sk = 1, ss_list_price = 10.0, ss_quantity = 2, ss_sold_date_sk = 1), Store_sale(ss_item_sk = 1, ss_list_price = 20.0, ss_quantity = 3, ss_sold_date_sk = 2))

val catalog_sales = mutableListOf(Catalog_sale(cs_item_sk = 1, cs_list_price = 10.0, cs_quantity = 2, cs_sold_date_sk = 1))

val web_sales = mutableListOf(Web_sale(ws_item_sk = 1, ws_list_price = 30.0, ws_quantity = 1, ws_sold_date_sk = 1))

val item = mutableListOf(Item(i_item_sk = 1, i_brand_id = 1, i_class_id = 1, i_category_id = 1))

val date_dim = mutableListOf(Date_dim(d_date_sk = 1, d_year = 2000, d_moy = 12), Date_dim(d_date_sk = 2, d_year = 2002, d_moy = 11))

val cross_items = mutableListOf(Cross_item(ss_item_sk = 1))

val avg_sales = run { val r = mutableListOf(20.0, 20.0, 30.0).map{ toDouble(it) }.average(); if (r % 1.0 == 0.0) r.toInt() else r }

val store_filtered = run {
    val __groups = mutableMapOf<MutableMap<String, Any?>, Group<MutableMap<String, Any?>, MutableMap<String, Any?>>>()
    val __order = mutableListOf<MutableMap<String, Any?>>()
    for (ss in store_sales) {
        for (d in date_dim) {
            if (ss.ss_sold_date_sk == d.d_date_sk && d.d_year == 2002 && d.d_moy == 11) {
                if (ss.ss_item_sk in (run {
    val __res = mutableListOf<Int>()
    for (ci in cross_items) {
        __res.add(ci.ss_item_sk)
    }
    __res
})) {
                    val __k = (mutableMapOf("brand_id" to 1, "class_id" to 1, "category_id" to 1) as MutableMap<String, Any?>)
                    var __g = __groups[__k]
                    if (__g == null) {
                        __g = Group(__k, mutableListOf<MutableMap<String, Any?>>())
                        __groups[__k] = __g
                        __order.add(__k)
                    }
                    __g.add(mutableMapOf("ss" to ss, "d" to d) as MutableMap<String, Any?>)
                }
            }
        }
    }
    val __res = mutableListOf<Any?>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(mutableMapOf("channel" to "store", "sales" to sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add(toDouble((x as MutableMap<String, Any?>)["ss_quantity"]) * toDouble((x as MutableMap<String, Any?>)["ss_list_price"]))
    }
    __res
}), "number_sales" to run {
    val __res = mutableListOf<MutableMap<String, Any?>>()
    for (_ in g) {
        __res.add((_ as MutableMap<String, Any?>))
    }
    __res
}.size))
    }
    __res
}

val result = run {
    val __res = mutableListOf<Any?>()
    for (r in store_filtered) {
        if (toDouble((r as MutableMap<String, Any?>)["sales"]) > toDouble(avg_sales)) {
            __res.add(mutableMapOf("channel" to (r as MutableMap<String, Any?>)["channel"], "i_brand_id" to 1, "i_class_id" to 1, "i_category_id" to 1, "sales" to (r as MutableMap<String, Any?>)["sales"], "number_sales" to (r as MutableMap<String, Any?>)["number_sales"]))
        }
    }
    __res
}

fun main() {
    json(result)
}
