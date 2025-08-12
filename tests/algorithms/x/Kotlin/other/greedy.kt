import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class Thing(var name: String = "", var value: Double = 0.0, var weight: Double = 0.0)
data class GreedyResult(var items: MutableList<Thing> = mutableListOf<Thing>(), var total_value: Double = 0.0)
var food: MutableList<String> = mutableListOf("Burger", "Pizza", "Coca Cola", "Rice", "Sambhar", "Chicken", "Fries", "Milk")
var value: MutableList<Double> = mutableListOf(80.0, 100.0, 60.0, 70.0, 50.0, 110.0, 90.0, 60.0)
var weight: MutableList<Double> = mutableListOf(40.0, 60.0, 40.0, 70.0, 100.0, 85.0, 55.0, 70.0)
var foods: MutableList<Thing> = build_menu(food, value, weight)
fun get_value(t: Thing): Double {
    return t.value
}

fun get_weight(t: Thing): Double {
    return t.weight
}

fun get_name(t: Thing): String {
    return t.name
}

fun value_weight(t: Thing): Double {
    return t.value / t.weight
}

fun build_menu(names: MutableList<String>, values: MutableList<Double>, weights: MutableList<Double>): MutableList<Thing> {
    var menu: MutableList<Thing> = mutableListOf<Thing>()
    var i: Int = (0).toInt()
    while ((((i < values.size) && (i < names.size) as Boolean)) && (i < weights.size)) {
        menu = run { val _tmp = menu.toMutableList(); _tmp.add(Thing(name = names[i]!!, value = values[i]!!, weight = weights[i]!!)); _tmp }
        i = i + 1
    }
    return menu
}

fun sort_desc(items: MutableList<Thing>, key_func: (Thing) -> Double): MutableList<Thing> {
    var arr: MutableList<Thing> = mutableListOf<Thing>()
    var i: Int = (0).toInt()
    while (i < items.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(items[i]!!); _tmp }
        i = i + 1
    }
    var j: Int = (1).toInt()
    while (j < arr.size) {
        var key_item: Thing = arr[j]!!
        var key_val: Double = ((key_func(key_item)).toDouble())
        var k: BigInteger = ((j - 1).toBigInteger())
        while ((k.compareTo((0).toBigInteger()) >= 0) && (key_func(arr[(k).toInt()]!!) < key_val)) {
            _listSet(arr, (k.add((1).toBigInteger())).toInt(), arr[(k).toInt()]!!)
            k = k.subtract((1).toBigInteger())
        }
        _listSet(arr, (k.add((1).toBigInteger())).toInt(), key_item)
        j = j + 1
    }
    return arr
}

fun greedy(items: MutableList<Thing>, max_cost: Double, key_func: (Thing) -> Double): GreedyResult {
    var items_copy: MutableList<Thing> = sort_desc(items, key_func)
    var result: MutableList<Thing> = mutableListOf<Thing>()
    var total_value: Double = 0.0
    var total_cost: Double = 0.0
    var i: Int = (0).toInt()
    while (i < items_copy.size) {
        var it: Thing = items_copy[i]!!
        var w: Double = get_weight(it)
        if ((total_cost + w) <= max_cost) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(it); _tmp }
            total_cost = total_cost + w
            total_value = total_value + get_value(it)
        }
        i = i + 1
    }
    return GreedyResult(items = result, total_value = total_value)
}

fun thing_to_string(t: Thing): String {
    return ((((("Thing(" + t.name) + ", ") + t.value.toString()) + ", ") + t.weight.toString()) + ")"
}

fun list_to_string(ts: MutableList<Thing>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < ts.size) {
        s = s + thing_to_string(ts[i]!!)
        if (i < (ts.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(list_to_string(foods))
        var res: GreedyResult = greedy(foods, 500.0, ::get_value)
        println(list_to_string(res.items))
        println(res.total_value.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
