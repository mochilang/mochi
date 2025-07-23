fun accumulator(sum: Any): (Any) -> Any {
    var store: MutableList<Any> = mutableListOf(sum)
    fun add(nv: Any): Any {
        store[0] = (store[0] as Number).toDouble() + (nv as Number).toDouble()
        return store[0] as (Any) -> Any
    }

    return add as (Any) -> Any
}

fun user_main(): Unit {
    val x: (Any) -> Any = accumulator(1) as (Any) -> Any
    x(5)
    accumulator(3) as (Any) -> Any
    println(x(2.3).toString())
}

fun main() {
    user_main()
}
