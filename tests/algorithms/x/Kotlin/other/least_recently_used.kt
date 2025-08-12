fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

data class LRUCache(var max_capacity: Int = 0, var store: MutableList<String> = mutableListOf<String>())
var lru: LRUCache = new_cache(4)
fun new_cache(n: Int): LRUCache {
    if (n < 0) {
        panic("n should be an integer greater than 0.")
    }
    var cap: Int = (if (n == 0) 2147483647 else n).toInt()
    return LRUCache(max_capacity = cap, store = mutableListOf<String>())
}

fun remove_element(xs: MutableList<String>, x: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var removed: Boolean = false
    var i: Int = (0).toInt()
    while (i < xs.size) {
        var v: String = xs[i]!!
        if ((removed == false) && (v == x)) {
            removed = true
        } else {
            res = (res + mutableListOf(v)).toMutableList()
        }
        i = i + 1
    }
    return res
}

fun refer(cache: LRUCache, x: String): LRUCache {
    var store: MutableList<String> = cache.store
    var exists: Boolean = false
    var i: Int = (0).toInt()
    while (i < store.size) {
        if (store[i]!! == x) {
            exists = true
        }
        i = i + 1
    }
    if ((exists as Boolean)) {
        store = remove_element(store, x)
    } else {
        if (store.size == cache.max_capacity) {
            var new_store: MutableList<String> = mutableListOf<String>()
            var j: Int = (0).toInt()
            while (j < (store.size - 1)) {
                new_store = (new_store + mutableListOf(store[j]!!)).toMutableList()
                j = j + 1
            }
            store = new_store
        }
    }
    store = (mutableListOf(x) + store).toMutableList()
    return LRUCache(max_capacity = cache.max_capacity, store = store)
}

fun display(cache: LRUCache): Unit {
    var i: Int = (0).toInt()
    while (i < (cache.store).size) {
        println((cache.store)[i]!!)
        i = i + 1
    }
}

fun repr_item(s: String): String {
    var all_digits: Boolean = true
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s[i].toString()
        if ((ch < "0") || (ch > "9")) {
            all_digits = false
        }
        i = i + 1
    }
    if ((all_digits as Boolean)) {
        return s
    }
    return ("'" + s) + "'"
}

fun cache_repr(cache: LRUCache): String {
    var res: String = ("LRUCache(" + cache.max_capacity.toString()) + ") => ["
    var i: Int = (0).toInt()
    while (i < (cache.store).size) {
        res = res + repr_item((cache.store)[i]!!)
        if (i < ((cache.store).size - 1)) {
            res = res + ", "
        }
        i = i + 1
    }
    res = res + "]"
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        lru = refer(lru, "A")
        lru = refer(lru, "2")
        lru = refer(lru, "3")
        lru = refer(lru, "A")
        lru = refer(lru, "4")
        lru = refer(lru, "5")
        var r: String = cache_repr(lru)
        println(r)
        if (r != "LRUCache(4) => [5, 4, 'A', 3]") {
            panic("Assertion error")
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
