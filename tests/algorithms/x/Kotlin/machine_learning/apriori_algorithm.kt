import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

data class Itemset(var items: MutableList<String> = mutableListOf<String>(), var support: Int = 0)
var frequent_itemsets: MutableList<Itemset> = apriori(load_data(), 2)
fun load_data(): MutableList<MutableList<String>> {
    return mutableListOf(mutableListOf("milk"), mutableListOf("milk", "butter"), mutableListOf("milk", "bread"), mutableListOf("milk", "bread", "chips"))
}

fun contains_string(xs: MutableList<String>, s: String): Boolean {
    for (v in xs) {
        if (v == s) {
            return true
        }
    }
    return false
}

fun is_subset(candidate: MutableList<String>, transaction: MutableList<String>): Boolean {
    for (it in candidate) {
        if (!contains_string(transaction, it)) {
            return false
        }
    }
    return true
}

fun lists_equal(a: MutableList<String>, b: MutableList<String>): Boolean {
    if (a.size != b.size) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < a.size) {
        if (a[i]!! != b[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun contains_list(itemset: MutableList<MutableList<String>>, item: MutableList<String>): Boolean {
    for (l in itemset) {
        if (((lists_equal(l, item)) as Boolean)) {
            return true
        }
    }
    return false
}

fun count_list(itemset: MutableList<MutableList<String>>, item: MutableList<String>): Int {
    var c: Int = (0).toInt()
    for (l in itemset) {
        if (((lists_equal(l, item)) as Boolean)) {
            c = c + 1
        }
    }
    return c
}

fun slice_list(xs: MutableList<MutableList<String>>, start: Int): MutableList<MutableList<String>> {
    var res: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var i: Int = (start).toInt()
    while (i < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun combinations_lists(xs: MutableList<MutableList<String>>, k: Int): MutableList<MutableList<MutableList<String>>> {
    var result: MutableList<MutableList<MutableList<String>>> = mutableListOf<MutableList<MutableList<String>>>()
    if (k == 0) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(mutableListOf<MutableList<String>>()); _tmp }
        return result
    }
    var i: Int = (0).toInt()
    while (i < xs.size) {
        var head: MutableList<String> = xs[i]!!
        var tail: MutableList<MutableList<String>> = slice_list(xs, i + 1)
        var tail_combos: MutableList<MutableList<MutableList<String>>> = combinations_lists(tail, k - 1)
        for (combo in tail_combos) {
            var new_combo: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
            new_combo = run { val _tmp = new_combo.toMutableList(); _tmp.add(head); _tmp }
            for (c in combo) {
                new_combo = run { val _tmp = new_combo.toMutableList(); _tmp.add(c); _tmp }
            }
            result = run { val _tmp = result.toMutableList(); _tmp.add(new_combo); _tmp }
        }
        i = i + 1
    }
    return result
}

fun prune(itemset: MutableList<MutableList<String>>, candidates: MutableList<MutableList<MutableList<String>>>, length: Int): MutableList<MutableList<String>> {
    var pruned: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    for (candidate in candidates) {
        var is_subsequence: Boolean = true
        for (item in candidate) {
            if ((!contains_list(itemset, item) as Boolean) || (count_list(itemset, item) < (length - 1))) {
                is_subsequence = false
                break
            }
        }
        if ((is_subsequence as Boolean)) {
            var merged: MutableList<String> = mutableListOf<String>()
            for (item in candidate) {
                for (s in item) {
                    if (!contains_string(merged, s)) {
                        merged = run { val _tmp = merged.toMutableList(); _tmp.add(s); _tmp }
                    }
                }
            }
            pruned = run { val _tmp = pruned.toMutableList(); _tmp.add(merged); _tmp }
        }
    }
    return pruned
}

fun sort_strings(xs: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    for (s in xs) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(s); _tmp }
    }
    var i: Int = (0).toInt()
    while (i < res.size) {
        var j: Int = (i + 1).toInt()
        while (j < res.size) {
            if (res[j]!! < res[i]!!) {
                var tmp: String = res[i]!!
                _listSet(res, i, res[j]!!)
                _listSet(res, j, tmp)
            }
            j = j + 1
        }
        i = i + 1
    }
    return res
}

fun itemset_to_string(xs: MutableList<String>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i > 0) {
            s = s + ", "
        }
        s = ((s + "'") + xs[i]!!) + "'"
        i = i + 1
    }
    s = s + "]"
    return s
}

fun apriori(data: MutableList<MutableList<String>>, min_support: Int): MutableList<Itemset> {
    var itemset: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    for (transaction in data) {
        var t: MutableList<String> = mutableListOf<String>()
        for (v in transaction) {
            t = run { val _tmp = t.toMutableList(); _tmp.add(v); _tmp }
        }
        itemset = run { val _tmp = itemset.toMutableList(); _tmp.add(t); _tmp }
    }
    var frequent: MutableList<Itemset> = mutableListOf<Itemset>()
    var length: Int = (1).toInt()
    while (itemset.size > 0) {
        var counts: MutableList<Int> = mutableListOf<Int>()
        var idx: Int = (0).toInt()
        while (idx < itemset.size) {
            counts = run { val _tmp = counts.toMutableList(); _tmp.add(0); _tmp }
            idx = idx + 1
        }
        for (transaction in data) {
            var j: Int = (0).toInt()
            while (j < itemset.size) {
                var candidate: MutableList<String> = itemset[j]!!
                if (((is_subset(candidate, transaction)) as Boolean)) {
                    _listSet(counts, j, counts[j]!! + 1)
                }
                j = j + 1
            }
        }
        var new_itemset: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
        var k: Int = (0).toInt()
        while (k < itemset.size) {
            if (counts[k]!! >= min_support) {
                new_itemset = run { val _tmp = new_itemset.toMutableList(); _tmp.add(itemset[k]!!); _tmp }
            }
            k = k + 1
        }
        itemset = new_itemset
        var m: Int = (0).toInt()
        while (m < itemset.size) {
            var sorted_item: MutableList<String> = sort_strings(itemset[m]!!)
            frequent = run { val _tmp = frequent.toMutableList(); _tmp.add(Itemset(items = sorted_item, support = counts[m]!!)); _tmp }
            m = m + 1
        }
        length = length + 1
        var combos: MutableList<MutableList<MutableList<String>>> = combinations_lists(itemset, length)
        itemset = prune(itemset, combos, length)
    }
    return frequent
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (fi in frequent_itemsets) {
            println((itemset_to_string(fi.items) + ": ") + _numToStr(fi.support))
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
