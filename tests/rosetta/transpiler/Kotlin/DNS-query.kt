val res: MutableList<Any?> = mutableListOf<Any?>(mutableListOf("210.155.141.200"), null) as MutableList<Any?>
val addrs: Any? = res[0]
val err: Any? = res[1]
fun main() {
    if (err == null) {
        println(addrs.toString())
    } else {
        println(err)
    }
}
