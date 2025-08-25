val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

data class List(var data: MutableList<Int> = mutableListOf<Int>())
fun empty_list(): List {
    return List(data = mutableListOf<Int>())
}

fun push(lst: List, value: Int): List {
    var res: MutableList<Int> = mutableListOf(value)
    var i: Int = (0).toInt()
    while (i < (lst.data).size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((lst.data)[i]!!); _tmp }
        i = i + 1
    }
    return List(data = res)
}

fun middle_element(lst: List): Int {
    var n: Int = ((lst.data).size).toInt()
    if (n == 0) {
        println("No element found.")
        return 0
    }
    var slow: Int = (0).toInt()
    var fast: Int = (0).toInt()
    while ((fast + 1) < n) {
        fast = fast + 2
        slow = slow + 1
    }
    return (lst.data)[slow]!!
}

fun user_main(): Unit {
    var lst: List = empty_list()
    middle_element(lst)
    lst = push(lst, 5)
    println(5)
    lst = push(lst, 6)
    println(6)
    lst = push(lst, 8)
    println(8)
    lst = push(lst, 8)
    println(8)
    lst = push(lst, 10)
    println(10)
    lst = push(lst, 12)
    println(12)
    lst = push(lst, 17)
    println(17)
    lst = push(lst, 7)
    println(7)
    lst = push(lst, 3)
    println(3)
    lst = push(lst, 20)
    println(20)
    lst = push(lst, 0 - 20)
    println(0 - 20)
    println(middle_element(lst))
}

fun main() {
    user_main()
}
