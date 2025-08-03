data class Box(var Contents: String = "", var secret: Int = 0) {
    fun TellSecret(): Int {
        return (secret as Int)
    }
}
var box: Box = New()
fun New(): Box {
    var b: Box = Box(Contents = "rabbit", secret = 1)
    return b
}

fun main() {
    box.TellSecret()
}
