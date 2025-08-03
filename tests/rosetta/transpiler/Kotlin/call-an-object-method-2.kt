data class Box(var Contents: String = "", var secret: Int = 0) {
    fun TellSecret(): Int {
        return (secret as Int)
    }
}
var funcs: MutableList<() -> Any> = newFactory()
var New: () -> Any = funcs[0] as () -> Any
var Count: () -> Any = funcs[1] as () -> Any
fun newFactory(): MutableList<() -> Any> {
    var sn: Int = 0
    fun New(): Box {
        sn = sn + 1
        var b: Box = Box(secret = sn)
        if (sn == 1) {
            b.Contents = "rabbit"
        } else {
            if (sn == 2) {
                b.Contents = "rock"
            }
        }
        return b
    }

    fun Count(): Int {
        return sn
    }

    return mutableListOf<() -> Any>((::New as () -> Any), (::Count as () -> Any))
}

fun main() {
}
