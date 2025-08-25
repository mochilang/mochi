val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class Node(var data: Int = 0, var next: Int = 0, var prev: Int = 0)
data class Stack(var nodes: MutableList<Node> = mutableListOf<Node>(), var head: Int = 0)
data class PopResult(var stack: Stack = Stack(nodes = mutableListOf<Node>(), head = 0), var value: Int = 0, var ok: Boolean = false)
data class TopResult(var value: Int = 0, var ok: Boolean = false)
fun empty_stack(): Stack {
    return Stack(nodes = mutableListOf<Node>(), head = 0 - 1)
}

fun push(stack: Stack, value: Int): Stack {
    var nodes: MutableList<Node> = stack.nodes
    var idx: Int = (nodes.size).toInt()
    var new_node: Node = Node(data = value, next = stack.head, prev = 0 - 1)
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(new_node); _tmp }
    if (stack.head != (0 - 1)) {
        var head_node: Node = nodes[stack.head]!!
        head_node.prev = idx
        _listSet(nodes, stack.head, head_node)
    }
    return Stack(nodes = nodes, head = idx)
}

fun pop(stack: Stack): PopResult {
    if (stack.head == (0 - 1)) {
        return PopResult(stack = stack, value = 0, ok = false)
    }
    var nodes: MutableList<Node> = stack.nodes
    var head_node: Node = nodes[stack.head]!!
    var value: Int = (head_node.data).toInt()
    var next_idx: Int = (head_node.next).toInt()
    if (next_idx != (0 - 1)) {
        var next_node: Node = nodes[next_idx]!!
        next_node.prev = 0 - 1
        _listSet(nodes, next_idx, next_node)
    }
    var new_stack: Stack = Stack(nodes = nodes, head = next_idx)
    return PopResult(stack = new_stack, value = value, ok = true)
}

fun top(stack: Stack): TopResult {
    if (stack.head == (0 - 1)) {
        return TopResult(value = 0, ok = false)
    }
    var node: Node = (stack.nodes)[stack.head]!!
    return TopResult(value = node.data, ok = true)
}

fun size(stack: Stack): Int {
    var count: Int = (0).toInt()
    var idx: Int = (stack.head).toInt()
    while (idx != (0 - 1)) {
        count = count + 1
        var node: Node = (stack.nodes)[idx]!!
        idx = node.next
    }
    return count
}

fun is_empty(stack: Stack): Boolean {
    return stack.head == (0 - 1)
}

fun print_stack(stack: Stack): Unit {
    println("stack elements are:")
    var idx: Int = (stack.head).toInt()
    var s: String = ""
    while (idx != (0 - 1)) {
        var node: Node = (stack.nodes)[idx]!!
        s = (s + _numToStr(node.data)) + "->"
        idx = node.next
    }
    if (s.length > 0) {
        println(s)
    }
}

fun user_main(): Unit {
    var stack: Stack = empty_stack()
    println("Stack operations using Doubly LinkedList")
    stack = push(stack, 4)
    stack = push(stack, 5)
    stack = push(stack, 6)
    stack = push(stack, 7)
    print_stack(stack)
    var t: TopResult = top(stack)
    if ((t.ok) as Boolean) {
        println("Top element is " + _numToStr(t.value))
    } else {
        println("Top element is None")
    }
    println("Size of the stack is " + _numToStr(size(stack)))
    var p: PopResult = pop(stack)
    stack = p.stack
    p = pop(stack)
    stack = p.stack
    print_stack(stack)
    println("stack is empty: " + is_empty(stack).toString())
}

fun main() {
    user_main()
}
