fun main() {
    var door: Int = 1
    var incrementer: Int = 0
    for (current in 1 until 101) {
        var line: String = ("Door " + (current.toString()).toString()) + " "
        if (current == door) {
            line = line + "Open"
            incrementer = incrementer + 1
            door = (door + (2 * incrementer)) + 1
        } else {
            line = line + "Closed"
        }
        println(line)
    }
}
