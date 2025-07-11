val prefix = "fore"

val s1 = "forest"

val s2 = "desert"

fun main() {
    println(s1.substring(0, prefix.length) == prefix)
    println(s2.substring(0, prefix.length) == prefix)
}
