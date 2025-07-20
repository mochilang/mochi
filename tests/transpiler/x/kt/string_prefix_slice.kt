val prefix: String = "fore"
val s1: String = "forest"
val s2: String = "desert"
fun main() {
    println(s1.substring(0, prefix.length) == prefix)
    println(s2.substring(0, prefix.length) == prefix)
}
