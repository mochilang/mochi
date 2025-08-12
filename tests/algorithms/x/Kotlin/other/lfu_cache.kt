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

data class Entry(var key: Int = 0, var _val: Int = 0, var freq: Int = 0, var order: Int = 0)
data class LFUCache(var entries: MutableList<Entry> = mutableListOf<Entry>(), var capacity: Int = 0, var hits: Int = 0, var miss: Int = 0, var tick: Int = 0)
data class GetResult(var cache: LFUCache = LFUCache(entries = mutableListOf<Entry>(), capacity = 0, hits = 0, miss = 0, tick = 0), var value: Int = 0, var ok: Boolean = false)
fun lfu_new(cap: Int): LFUCache {
    return LFUCache(entries = mutableListOf<Entry>(), capacity = cap, hits = 0, miss = 0, tick = 0)
}

fun find_entry(entries: MutableList<Entry>, key: Int): Int {
    var i: Int = (0).toInt()
    while (i < entries.size) {
        var e: Entry = entries[i]!!
        if (e.key == key) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun lfu_get(cache: LFUCache, key: Int): GetResult {
    var idx: Int = (find_entry(cache.entries, key)).toInt()
    if (idx == (0 - 1)) {
        var new_cache: LFUCache = LFUCache(entries = cache.entries, capacity = cache.capacity, hits = cache.hits, miss = cache.miss + 1, tick = cache.tick)
        return GetResult(cache = new_cache, value = 0, ok = false)
    }
    var entries: MutableList<Entry> = cache.entries
    var e: Entry = entries[idx]!!
    e.freq = e.freq + 1
    var new_tick: Int = (cache.tick + 1).toInt()
    e.order = new_tick
    _listSet(entries, idx, e)
    var new_cache: LFUCache = LFUCache(entries = entries, capacity = cache.capacity, hits = cache.hits + 1, miss = cache.miss, tick = new_tick)
    return GetResult(cache = new_cache, value = e._val, ok = true)
}

fun remove_lfu(entries: MutableList<Entry>): MutableList<Entry> {
    if (entries.size == 0) {
        return entries
    }
    var min_idx: Int = (0).toInt()
    var i: Int = (1).toInt()
    while (i < entries.size) {
        var e: Entry = entries[i]!!
        var m: Entry = entries[min_idx]!!
        if ((e.freq < m.freq) || (((e.freq == m.freq) && (e.order < m.order) as Boolean))) {
            min_idx = i
        }
        i = i + 1
    }
    var res: MutableList<Entry> = mutableListOf<Entry>()
    var j: Int = (0).toInt()
    while (j < entries.size) {
        if (j != min_idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(entries[j]!!); _tmp }
        }
        j = j + 1
    }
    return res
}

fun lfu_put(cache: LFUCache, key: Int, value: Int): LFUCache {
    var entries: MutableList<Entry> = cache.entries
    var idx: Int = (find_entry(entries, key)).toInt()
    if (idx != (0 - 1)) {
        var e: Entry = entries[idx]!!
        e._val = value
        e.freq = e.freq + 1
        var new_tick: Int = (cache.tick + 1).toInt()
        e.order = new_tick
        _listSet(entries, idx, e)
        return LFUCache(entries = entries, capacity = cache.capacity, hits = cache.hits, miss = cache.miss, tick = new_tick)
    }
    if (entries.size >= cache.capacity) {
        entries = remove_lfu(entries)
    }
    var new_tick: Int = (cache.tick + 1).toInt()
    var new_entry: Entry = Entry(key = key, _val = value, freq = 1, order = new_tick)
    entries = run { val _tmp = entries.toMutableList(); _tmp.add(new_entry); _tmp }
    return LFUCache(entries = entries, capacity = cache.capacity, hits = cache.hits, miss = cache.miss, tick = new_tick)
}

fun cache_info(cache: LFUCache): String {
    return ((((((("CacheInfo(hits=" + cache.hits.toString()) + ", misses=") + cache.miss.toString()) + ", capacity=") + cache.capacity.toString()) + ", current_size=") + (cache.entries).size.toString()) + ")"
}

fun user_main(): Unit {
    var cache: LFUCache = lfu_new(2)
    cache = lfu_put(cache, 1, 1)
    cache = lfu_put(cache, 2, 2)
    var r: GetResult = lfu_get(cache, 1)
    cache = r.cache
    if (((r.ok) as Boolean)) {
        println(r.value.toString())
    } else {
        println("None")
    }
    cache = lfu_put(cache, 3, 3)
    r = lfu_get(cache, 2)
    cache = r.cache
    if (((r.ok) as Boolean)) {
        println(r.value.toString())
    } else {
        println("None")
    }
    cache = lfu_put(cache, 4, 4)
    r = lfu_get(cache, 1)
    cache = r.cache
    if (((r.ok) as Boolean)) {
        println(r.value.toString())
    } else {
        println("None")
    }
    r = lfu_get(cache, 3)
    cache = r.cache
    if (((r.ok) as Boolean)) {
        println(r.value.toString())
    } else {
        println("None")
    }
    r = lfu_get(cache, 4)
    cache = r.cache
    if (((r.ok) as Boolean)) {
        println(r.value.toString())
    } else {
        println("None")
    }
    println(cache_info(cache))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
