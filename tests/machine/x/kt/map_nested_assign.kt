fun main() {
    val data = mutableMapOf("outer" to mutableMapOf("inner" to 1))
    data["outer"]!!["inner"] = 2
    println(data["outer"]!!["inner"])
}
