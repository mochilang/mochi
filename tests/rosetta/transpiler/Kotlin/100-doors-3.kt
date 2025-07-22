fun main() {
    var result: String = ""
    for (i in 1 until 101) {
        var j: Int = 1
        while ((j * j) < i) {
            j = j + 1
        }
        if ((j * j) == i) {
            result = result + "O"
        } else {
            result = result + "-"
        }
    }
    println(result)
}
