fun main() {
    val prefix: String = "fore"
    val s1: String = "forest"
    println(s1.substring(0, prefix.length) == prefix)
    val s2: String = "desert"
    println(s2.substring(0, prefix.length) == prefix)
}
