fun main() {
    var data: MutableMap<String, MutableMap<String, Int>> = mutableMapOf("outer" to mutableMapOf("inner" to 1))
    data["outer"]!!["inner"] = 2
    println(data["outer"]!!["inner"])
}
