// Code generated from tests/vm/valid/map_literal_dynamic.mochi

var x = 3

var y = 4

var m = mutableMapOf("a" to x, "b" to y)

fun main() {
    println(listOf(m["a"], m["b"]).joinToString(" "))
}
