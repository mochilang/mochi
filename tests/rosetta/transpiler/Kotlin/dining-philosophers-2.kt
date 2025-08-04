fun user_main(): Unit {
    var philosophers: MutableList<String> = mutableListOf("Aristotle", "Kant", "Spinoza", "Marx", "Russell")
    var hunger: Int = 3
    println("table empty")
    for (p in philosophers) {
        println(p + " seated")
    }
    var i: Int = 0
    while (i < philosophers.size) {
        var name: String = philosophers[i]!!
        var h: Int = 0
        while (h < hunger) {
            println(name + " hungry")
            println(name + " eating")
            println(name + " thinking")
            h = h + 1
        }
        println(name + " satisfied")
        println(name + " left the table")
        i = i + 1
    }
    println("table empty")
}

fun main() {
    user_main()
}
