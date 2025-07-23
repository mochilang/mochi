val res: MutableList<Any> = mutableListOf<Any?>(mutableListOf("210.155.141.200"), null) as MutableList<Any>
val addrs = res[0]
val err = res[1]
fun main() {
    if (err == null) {
        println(addrs.toString())
    } else {
        println(err)
    }
}
