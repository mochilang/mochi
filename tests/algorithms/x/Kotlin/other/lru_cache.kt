import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class Node(var key: Int = 0, var value: Int = 0, var prev: Int = 0, var next: Int = 0)
data class DoubleLinkedList(var nodes: MutableList<Node> = mutableListOf<Node>(), var head: Int = 0, var tail: Int = 0)
data class LRUCache(var list: DoubleLinkedList = DoubleLinkedList(nodes = mutableListOf<Node>(), head = 0, tail = 0), var capacity: Int = 0, var num_keys: Int = 0, var hits: Int = 0, var misses: Int = 0, var cache: MutableMap<String, Int> = mutableMapOf<String, Int>())
data class GetResult(var cache: LRUCache = LRUCache(list = DoubleLinkedList(nodes = mutableListOf<Node>(), head = 0, tail = 0), capacity = 0, num_keys = 0, hits = 0, misses = 0, cache = mutableMapOf<String, Int>()), var value: Int = 0, var ok: Boolean = false)
fun new_list(): DoubleLinkedList {
    var nodes: MutableList<Node> = mutableListOf<Node>()
    var head: Node = Node(key = 0, value = 0, prev = 0 - 1, next = 1)
    var tail: Node = Node(key = 0, value = 0, prev = 0, next = 0 - 1)
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(head); _tmp }
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(tail); _tmp }
    return DoubleLinkedList(nodes = nodes, head = 0, tail = 1)
}

fun dll_add(lst: DoubleLinkedList, idx: Int): DoubleLinkedList {
    var nodes: MutableList<Node> = lst.nodes
    var tail_idx: Int = (lst.tail).toInt()
    var tail_node: Node = nodes[tail_idx]!!
    var prev_idx: Int = (tail_node.prev).toInt()
    var node: Node = nodes[idx]!!
    node.prev = prev_idx
    node.next = tail_idx
    _listSet(nodes, idx, node)
    var prev_node: Node = nodes[prev_idx]!!
    prev_node.next = idx
    _listSet(nodes, prev_idx, prev_node)
    tail_node.prev = idx
    _listSet(nodes, tail_idx, tail_node)
    lst.nodes = nodes
    return lst
}

fun dll_remove(lst: DoubleLinkedList, idx: Int): DoubleLinkedList {
    var nodes: MutableList<Node> = lst.nodes
    var node: Node = nodes[idx]!!
    var prev_idx: Int = (node.prev).toInt()
    var next_idx: Int = (node.next).toInt()
    if ((prev_idx == (0 - 1)) || (next_idx == (0 - 1))) {
        return lst
    }
    var prev_node: Node = nodes[prev_idx]!!
    prev_node.next = next_idx
    _listSet(nodes, prev_idx, prev_node)
    var next_node: Node = nodes[next_idx]!!
    next_node.prev = prev_idx
    _listSet(nodes, next_idx, next_node)
    node.prev = 0 - 1
    node.next = 0 - 1
    _listSet(nodes, idx, node)
    lst.nodes = nodes
    return lst
}

fun new_cache(cap: Int): LRUCache {
    var empty_map: MutableMap<String, Int> = mutableMapOf<String, Int>()
    return LRUCache(list = new_list(), capacity = cap, num_keys = 0, hits = 0, misses = 0, cache = empty_map)
}

fun lru_get(c: LRUCache, key: Int): GetResult {
    var cache: LRUCache = c
    var key_str: String = key.toString()
    if (key_str in cache.cache) {
        var idx: Int = ((cache.cache)[key_str] as Int).toInt()
        if (idx != (0 - 1)) {
            cache.hits = cache.hits + 1
            var node: Node = (cache.list.nodes)[idx]!!
            var value: Int = (node.value).toInt()
            cache.list = dll_remove(cache.list, idx)
            cache.list = dll_add(cache.list, idx)
            return GetResult(cache = cache, value = value, ok = true)
        }
    }
    cache.misses = cache.misses + 1
    return GetResult(cache = cache, value = 0, ok = false)
}

fun lru_put(c: LRUCache, key: Int, value: Int): LRUCache {
    var cache: LRUCache = c
    var key_str: String = key.toString()
    if (!(key_str in cache.cache)) {
        if (cache.num_keys >= cache.capacity) {
            var head_node: Node = (cache.list.nodes)[cache.list.head]!!
            var first_idx: Int = (head_node.next).toInt()
            var first_node: Node = (cache.list.nodes)[first_idx]!!
            var old_key: Int = (first_node.key).toInt()
            cache.list = dll_remove(cache.list, first_idx)
            var mdel: MutableMap<String, Int> = cache.cache
            (mdel)[old_key.toString()] = 0 - 1
            cache.cache = mdel
            cache.num_keys = cache.num_keys - 1
        }
        var nodes: MutableList<Node> = cache.list.nodes
        var new_node: Node = Node(key = key, value = value, prev = 0 - 1, next = 0 - 1)
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(new_node); _tmp }
        var idx: Int = (nodes.size - 1).toInt()
        cache.list.nodes = nodes
        cache.list = dll_add(cache.list, idx)
        var m: MutableMap<String, Int> = cache.cache
        (m)[key_str] = idx
        cache.cache = m
        cache.num_keys = cache.num_keys + 1
    } else {
        var m: MutableMap<String, Int> = cache.cache
        var idx: Int = ((m)[key_str] as Int).toInt()
        var nodes: MutableList<Node> = cache.list.nodes
        var node: Node = nodes[idx]!!
        node.value = value
        _listSet(nodes, idx, node)
        cache.list.nodes = nodes
        cache.list = dll_remove(cache.list, idx)
        cache.list = dll_add(cache.list, idx)
        cache.cache = m
    }
    return cache
}

fun cache_info(cache: LRUCache): String {
    return ((((((("CacheInfo(hits=" + cache.hits.toString()) + ", misses=") + cache.misses.toString()) + ", capacity=") + cache.capacity.toString()) + ", current size=") + cache.num_keys.toString()) + ")"
}

fun print_result(res: GetResult): Unit {
    if (((res.ok) as Boolean)) {
        println(res.value.toString())
    } else {
        println("None")
    }
}

fun user_main(): Unit {
    var cache: LRUCache = new_cache(2)
    cache = lru_put(cache, 1, 1)
    cache = lru_put(cache, 2, 2)
    var r1: GetResult = lru_get(cache, 1)
    cache = r1.cache
    print_result(r1)
    cache = lru_put(cache, 3, 3)
    var r2: GetResult = lru_get(cache, 2)
    cache = r2.cache
    print_result(r2)
    cache = lru_put(cache, 4, 4)
    var r3: GetResult = lru_get(cache, 1)
    cache = r3.cache
    print_result(r3)
    var r4: GetResult = lru_get(cache, 3)
    cache = r4.cache
    print_result(r4)
    var r5: GetResult = lru_get(cache, 4)
    cache = r5.cache
    print_result(r5)
    println(cache_info(cache))
}

fun main() {
    user_main()
}
