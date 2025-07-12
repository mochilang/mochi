// Code generated from tests/vm/valid/for_map_collection.mochi

var m = mutableMapOf("a" to 1, "b" to 2)

fun main() {
    for (k in m.keys) {
        println(k)
    }
}
