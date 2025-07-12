// Code generated from tests/vm/valid/map_assign.mochi

var scores = mutableMapOf("alice" to 1)

fun main() {
    scores["bob"] = 2
    println(scores["bob"])
}
