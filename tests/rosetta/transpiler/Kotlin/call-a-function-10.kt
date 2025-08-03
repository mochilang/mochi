fun user_main(): Unit {
    var list: MutableList<Int> = mutableListOf<Int>()
    var a: Int = 1
    var d: Int = 2
    var e: Int = 3
    var i: Int = 4
    list = run { val _tmp = list.toMutableList(); _tmp.add(a); _tmp }
    list = run { val _tmp = list.toMutableList(); _tmp.add(d); _tmp }
    list = run { val _tmp = list.toMutableList(); _tmp.add(e); _tmp }
    list = run { val _tmp = list.toMutableList(); _tmp.add(i); _tmp }
    i = list.size
}

fun main() {
    user_main()
}
