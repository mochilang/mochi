import java.math.BigInteger

val msg: String = "Hello World! "
var shift: Int = 0
var inc: Int = 1
var clicks: Int = 0
var frames: Int = 0
fun main() {
    while (clicks < 5) {
        var line: String = ""
        var i: Int = 0
        while (i < msg.length) {
            val idx: BigInteger = (shift + i) % msg.length
            line = line + msg.substring(idx, idx.add(1.toBigInteger()))
            i = i + 1
        }
        println(line)
        shift = (shift + inc) % msg.length
        frames = frames + 1
        if ((frames % msg.length) == 0) {
            inc = msg.length - inc
            clicks = clicks + 1
        }
    }
}
