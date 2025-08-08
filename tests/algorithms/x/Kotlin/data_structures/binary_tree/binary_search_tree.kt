fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun create_node(value: Int): MutableList<Any?> {
    return mutableListOf<Any?>((value as Any?), (null as Any?), (null as Any?))
}

fun insert(node: MutableList<Any?>, value: Int): MutableList<Any?> {
    if (node == null) {
        return create_node(value)
    }
    if ((value).toInt() < node[0] as Int) {
        _listSet(node, 1, ((insert(((node[1] as Any?) as MutableList<Any?>), value)) as Any?))
    } else {
        if ((value).toInt() > node[0] as Int) {
            _listSet(node, 2, ((insert(((node[2] as Any?) as MutableList<Any?>), value)) as Any?))
        }
    }
    return node
}

fun search(node: MutableList<Any?>, value: Int): Boolean {
    if (node == null) {
        return false
    }
    if (value == node[0] as Any?) {
        return true
    }
    if ((value).toInt() < node[0] as Int) {
        return search(((node[1] as Any?) as MutableList<Any?>), value)
    }
    return search(((node[2] as Any?) as MutableList<Any?>), value)
}

fun inorder(node: MutableList<Any?>, acc: MutableList<Int>): MutableList<Int> {
    if (node == null) {
        return acc
    }
    var left_acc: MutableList<Int> = inorder(((node[1] as Any?) as MutableList<Any?>), acc)
    var with_node = run { val _tmp = left_acc.toMutableList(); _tmp.add(((node[0] as Any?) as Int)); _tmp }
    return inorder(((node[2] as Any?) as MutableList<Any?>), (with_node as MutableList<Int>))
}

fun find_min(node: MutableList<Any?>): Int {
    var current: MutableList<Any?> = node
    while (current[1] as Any? != null) {
        current = ((current[1] as Any?) as MutableList<Any?>)
    }
    return ((current[0] as Any?) as Int)
}

fun find_max(node: MutableList<Any?>): Int {
    var current: MutableList<Any?> = node
    while (current[2] as Any? != null) {
        current = ((current[2] as Any?) as MutableList<Any?>)
    }
    return ((current[0] as Any?) as Int)
}

fun delete(node: MutableList<Any?>, value: Int): MutableList<Any?> {
    if (node == null) {
        return (null as MutableList<Any?>)
    }
    if ((value).toInt() < node[0] as Int) {
        _listSet(node, 1, ((delete(((node[1] as Any?) as MutableList<Any?>), value)) as Any?))
    } else {
        if ((value).toInt() > node[0] as Int) {
            _listSet(node, 2, ((delete(((node[2] as Any?) as MutableList<Any?>), value)) as Any?))
        } else {
            if (node[1] as Any? == null) {
                return ((node[2] as Any?) as MutableList<Any?>)
            }
            if (node[2] as Any? == null) {
                return ((node[1] as Any?) as MutableList<Any?>)
            }
            var min_val: Int = (find_min(((node[2] as Any?) as MutableList<Any?>))).toInt()
            _listSet(node, 0, (min_val as Any?))
            _listSet(node, 2, ((delete(((node[2] as Any?) as MutableList<Any?>), min_val)) as Any?))
        }
    }
    return node
}

fun user_main(): Unit {
    var root: MutableList<Any?>? = null
    var nums: MutableList<Int> = mutableListOf(8, 3, 6, 1, 10, 14, 13, 4, 7)
    for (v in nums) {
        root = insert(root, v)
    }
    println(inorder(root, mutableListOf<Int>()).toString())
    println(search(root, 6))
    println(search(root, 20))
    println(find_min(root))
    println(find_max(root))
    root = delete(root, 6)
    println(inorder(root, mutableListOf<Int>()).toString())
}

fun main() {
    user_main()
}
