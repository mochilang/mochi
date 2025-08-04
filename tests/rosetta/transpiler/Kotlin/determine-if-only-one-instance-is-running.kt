var lockExists: Boolean = false
fun startOnce(): Unit {
    if ((lockExists as Boolean)) {
        println("an instance is already running")
    } else {
        lockExists = true
        println("single instance started")
    }
}

fun user_main(): Unit {
    startOnce()
    startOnce()
}

fun main() {
    user_main()
}
