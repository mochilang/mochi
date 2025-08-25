import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class Node(var data: Int = 0, var next: Int = 0)
var NIL: Int = (0 - 1).toInt()
var nodes: MutableList<Node> = mutableListOf<Node>()
fun make_linked_list(elements: MutableList<Int>): Int {
    if (elements.size == 0) {
        panic("The Elements List is empty")
    }
    nodes = mutableListOf<Node>()
    nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = elements[0]!!, next = NIL.toInt())); _tmp }
    var head: Int = (0).toInt()
    var current: Int = (head).toInt()
    var i: Int = (1).toInt()
    while (i < elements.size) {
        nodes = run { val _tmp = nodes.toMutableList(); _tmp.add(Node(data = elements[i]!!, next = NIL.toInt())); _tmp }
        nodes[current]!!.next = nodes.size - 1
        current = nodes.size - 1
        i = i + 1
    }
    return head
}

fun node_to_string(head: Int): String {
    var s: String = ""
    var index: Int = (head).toInt()
    while ((index).toBigInteger().compareTo((NIL)) != 0) {
        var node: Node = nodes[index]!!
        s = ((s + "<") + _numToStr(node.data)) + "> ---> "
        index = node.next
    }
    s = s + "<END>"
    return s
}

fun user_main(): Unit {
    var list_data: MutableList<Int> = mutableListOf(1, 3, 5, 32, 44, 12, 43)
    println("List: " + list_data.toString())
    println("Creating Linked List from List.")
    var head: Int = (make_linked_list(list_data)).toInt()
    println("Linked List:")
    println(node_to_string(head))
}

fun main() {
    user_main()
}
