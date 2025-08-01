// Generated by Mochi compiler v0.10.26 on 2025-07-15T07:22:36Z
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
// Code generated from q25.mochi

data class StoreSale(var ss_sold_date_sk: Int, var ss_item_sk: Int, var ss_store_sk: Int, var ss_customer_sk: Int, var ss_net_profit: Double, var ss_ticket_number: Int)

data class StoreReturn(var sr_returned_date_sk: Int, var sr_item_sk: Int, var sr_customer_sk: Int, var sr_ticket_number: Int, var sr_net_loss: Double)

data class CatalogSale(var cs_sold_date_sk: Int, var cs_item_sk: Int, var cs_bill_customer_sk: Int, var cs_net_profit: Double)

data class DateDim(var d_date_sk: Int, var d_moy: Int, var d_year: Int)

data class Store(var s_store_sk: Int, var s_store_id: String, var s_store_name: String)

data class Item(var i_item_sk: Int, var i_item_id: String, var i_item_desc: String)

data class Catalog_sale(var cs_sold_date_sk: Int, var cs_item_sk: Int, var cs_bill_customer_sk: Int, var cs_net_profit: Double)

data class Date_dim(var d_date_sk: Int, var d_moy: Int, var d_year: Int)

data class Item(var i_item_sk: Int, var i_item_id: String, var i_item_desc: String)

data class Store(var s_store_sk: Int, var s_store_id: String, var s_store_name: String)

data class Store_return(var sr_returned_date_sk: Int, var sr_item_sk: Int, var sr_customer_sk: Int, var sr_ticket_number: Int, var sr_net_loss: Double)

data class Store_sale(var ss_sold_date_sk: Int, var ss_item_sk: Int, var ss_store_sk: Int, var ss_customer_sk: Int, var ss_net_profit: Double, var ss_ticket_number: Int)

val store_sales = mutableListOf(Store_sale(ss_sold_date_sk = 1, ss_item_sk = 1, ss_store_sk = 1, ss_customer_sk = 1, ss_net_profit = 50.0, ss_ticket_number = 1), Store_sale(ss_sold_date_sk = 1, ss_item_sk = 2, ss_store_sk = 1, ss_customer_sk = 2, ss_net_profit = 20.0, ss_ticket_number = 2))

val store_returns = mutableListOf(Store_return(sr_returned_date_sk = 2, sr_item_sk = 1, sr_customer_sk = 1, sr_ticket_number = 1, sr_net_loss = 10.0), Store_return(sr_returned_date_sk = 2, sr_item_sk = 2, sr_customer_sk = 2, sr_ticket_number = 2, sr_net_loss = 5.0))

val catalog_sales = mutableListOf(Catalog_sale(cs_sold_date_sk = 3, cs_item_sk = 1, cs_bill_customer_sk = 1, cs_net_profit = 30.0), Catalog_sale(cs_sold_date_sk = 3, cs_item_sk = 2, cs_bill_customer_sk = 2, cs_net_profit = 15.0))

val date_dim = mutableListOf(Date_dim(d_date_sk = 1, d_moy = 4, d_year = 2000), Date_dim(d_date_sk = 2, d_moy = 5, d_year = 2000), Date_dim(d_date_sk = 3, d_moy = 6, d_year = 2000))

val store = mutableListOf(Store(s_store_sk = 1, s_store_id = "S1", s_store_name = "Store1"))

val item = mutableListOf(Item(i_item_sk = 1, i_item_id = "ITEM1", i_item_desc = "Desc1"), Item(i_item_sk = 2, i_item_id = "ITEM2", i_item_desc = "Desc2"))

val result = run {
    val __groups = mutableMapOf<MutableMap<String, Any?>, Group<MutableMap<String, Any?>, MutableMap<String, Any?>>>()
    val __order = mutableListOf<MutableMap<String, Any?>>()
    for (ss in store_sales) {
        for (sr in store_returns) {
            if (ss.ss_ticket_number == sr.sr_ticket_number && ss.ss_item_sk == sr.sr_item_sk) {
                for (cs in catalog_sales) {
                    if (sr.sr_customer_sk == cs.cs_bill_customer_sk && sr.sr_item_sk == cs.cs_item_sk) {
                        for (d1 in date_dim) {
                            if (d1.d_date_sk == ss.ss_sold_date_sk) {
                                for (d2 in date_dim) {
                                    if (d2.d_date_sk == sr.sr_returned_date_sk) {
                                        for (d3 in date_dim) {
                                            if (d3.d_date_sk == cs.cs_sold_date_sk) {
                                                for (s in store) {
                                                    if (s.s_store_sk == ss.ss_store_sk) {
                                                        for (i in item) {
                                                            if (i.i_item_sk == ss.ss_item_sk) {
                                                                if (d1.d_moy == 4 && d1.d_year == 2000 && d2.d_moy >= 4 && d2.d_moy <= 10 && d3.d_moy >= 4 && d3.d_moy <= 10) {
                                                                    val __k = (mutableMapOf("item_id" to i.i_item_id, "item_desc" to i.i_item_desc, "s_store_id" to s.s_store_id, "s_store_name" to s.s_store_name) as MutableMap<String, Any?>)
                                                                    var __g = __groups[__k]
                                                                    if (__g == null) {
                                                                        __g = Group(__k, mutableListOf<MutableMap<String, Any?>>())
                                                                        __groups[__k] = __g
                                                                        __order.add(__k)
                                                                    }
                                                                    __g.add(mutableMapOf("ss" to ss, "sr" to sr, "cs" to cs, "d1" to d1, "d2" to d2, "d3" to d3, "s" to s, "i" to i) as MutableMap<String, Any?>)
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    val __res = mutableListOf<Any?>()
    for (k in __order) {
        val g = __groups[k]!!
        __res.add(mutableMapOf("i_item_id" to (g.key as MutableMap<String, Any?>)["item_id"], "i_item_desc" to (g.key as MutableMap<String, Any?>)["item_desc"], "s_store_id" to (g.key as MutableMap<String, Any?>)["s_store_id"], "s_store_name" to (g.key as MutableMap<String, Any?>)["s_store_name"], "store_sales_profit" to sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add((x as MutableMap<String, Any?>)["ss_net_profit"])
    }
    __res
}), "store_returns_loss" to sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add((x as MutableMap<String, Any?>)["sr_net_loss"])
    }
    __res
}), "catalog_sales_profit" to sum(run {
    val __res = mutableListOf<Any?>()
    for (x in g) {
        __res.add((x as MutableMap<String, Any?>)["cs_net_profit"])
    }
    __res
})))
    }
    __res
}

fun main() {
    json(result)
}
