// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:07:44Z
fun String.starts_with(prefix: String): Boolean = this.startsWith(prefix)
// Code generated from values_builtin.mochi

val m = mutableMapOf("a" to 1, "b" to 2, "c" to 3)

fun main() {
    println(m.values.toMutableList())
}
