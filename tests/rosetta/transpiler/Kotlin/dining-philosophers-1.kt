fun user_main(): Unit {
    var philosophers: MutableList<String> = mutableListOf("Aristotle", "Kant", "Spinoza", "Marx", "Russell")
    var hunger: Int = 3
    println("table empty")
    for (p in philosophers) {
        println(p + " seated")
    }
    var idx: Int = 0
    while (idx < philosophers.size) {
        var name: String = philosophers[idx]!!
        var h: Int = 0
        while (h < hunger) {
            println(name + " hungry")
            println(name + " eating")
            println(name + " thinking")
            h = h + 1
        }
        println(name + " satisfied")
        println(name + " left the table")
        idx = idx + 1
    }
    println("table empty")
}

fun main() {
    user_main()
}
