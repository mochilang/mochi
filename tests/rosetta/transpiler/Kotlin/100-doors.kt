fun main() {
    var doors: MutableList<Any> = mutableListOf()
    for (i in 0 until 100) {
        doors = (doors + false).toMutableList()
    }
    for (pass in 1 until 101) {
        var idx: Int = pass - 1
        while (idx < 100) {
            doors[idx] = !((doors[idx]!!) as Boolean)
            idx = idx + pass
        }
    }
    for (row in 0 until 10) {
        var line: String = ""
        for (col in 0 until 10) {
            val idx: Int = (row * 10) + col
            if ((doors[idx]!!) as Boolean) {
                line = line + "1"
            } else {
                line = line + "0"
            }
            if (col < 9) {
                line = line + " "
            }
        }
        println(line)
    }
}
