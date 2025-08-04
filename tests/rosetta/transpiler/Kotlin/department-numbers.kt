fun main() {
    println("Police  Sanitation  Fire")
    println("------  ----------  ----")
    var count: Int = 0
    var i: Int = 2
    while (i < 7) {
        var j: Int = 1
        while (j < 8) {
            if (j != i) {
                var k: Int = 1
                while (k < 8) {
                    if ((k != i) && (k != j)) {
                        if (((i + j) + k) == 12) {
                            println((((("  " + i.toString()) + "         ") + j.toString()) + "         ") + k.toString())
                            count = count + 1
                        }
                    }
                    k = k + 1
                }
            }
            j = j + 1
        }
        i = i + 2
    }
    println("")
    println(count.toString() + " valid combinations")
}
