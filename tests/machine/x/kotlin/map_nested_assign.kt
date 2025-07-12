// Code generated from tests/vm/valid/map_nested_assign.mochi

var data = mutableMapOf("outer" to mutableMapOf("inner" to 1))

fun main() {
    data["outer"]!!["inner"] = 2
    println((data["outer"] as MutableMap<String, Int>)["inner"])
}
