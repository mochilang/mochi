var partList: MutableList<String> = mutableListOf("A", "B", "C", "D")
var nAssemblies: Int = 3
fun main() {
    for (cycle in 1 until nAssemblies + 1) {
        println("begin assembly cycle " + cycle.toString())
        for (p in partList) {
            println(p + " worker begins part")
        }
        for (p in partList) {
            println(p + " worker completes part")
        }
        println(("assemble.  cycle " + cycle.toString()) + " complete")
    }
}
