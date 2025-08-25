import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Node(var value: String = "", var next: Int = 0)
data class Stack(var nodes: MutableList<Node> = mutableListOf<Node>(), var top: Int = 0)
data class PopResult(var stack: Stack = Stack(nodes = mutableListOf<Node>(), top = 0), var value: String = "")
fun empty_stack(): Stack {
    return Stack(nodes = mutableListOf<Node>(), top = 0 - 1)
}

fun is_empty(stack: Stack): Boolean {
    return stack.top == (0 - 1)
}

fun push(stack: Stack, item: String): Stack {
    var new_node: Node = Node(value = item, next = stack.top)
    var new_nodes: MutableList<Node> = stack.nodes
    new_nodes = run { val _tmp = new_nodes.toMutableList(); _tmp.add(new_node); _tmp }
    var new_top: Int = (new_nodes.size - 1).toInt()
    return Stack(nodes = new_nodes, top = new_top)
}

fun pop(stack: Stack): PopResult {
    if (stack.top == (0 - 1)) {
        panic("pop from empty stack")
    }
    var node: Node = (stack.nodes)[stack.top]!!
    var new_top: Int = (node.next).toInt()
    var new_stack: Stack = Stack(nodes = stack.nodes, top = new_top)
    return PopResult(stack = new_stack, value = node.value)
}

fun peek(stack: Stack): String {
    if (stack.top == (0 - 1)) {
        panic("peek from empty stack")
    }
    var node: Node = (stack.nodes)[stack.top]!!
    return node.value
}

fun clear(stack: Stack): Stack {
    return Stack(nodes = mutableListOf<Node>(), top = 0 - 1)
}

fun user_main(): Unit {
    var stack: Stack = empty_stack()
    println(is_empty(stack))
    stack = push(stack, "5")
    stack = push(stack, "9")
    stack = push(stack, "python")
    println(is_empty(stack))
    var res: PopResult = pop(stack)
    stack = res.stack
    println(res.value)
    stack = push(stack, "algorithms")
    res = pop(stack)
    stack = res.stack
    println(res.value)
    res = pop(stack)
    stack = res.stack
    println(res.value)
    res = pop(stack)
    stack = res.stack
    println(res.value)
    println(is_empty(stack))
}

fun main() {
    user_main()
}
