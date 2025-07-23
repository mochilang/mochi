sealed class Beast
data class Dog(val kind: String, val name: String) : Beast()
data class Cat(val kind: String, val name: String) : Beast()
fun beastKind(b: Beast): String {
    return when (b) {
    is Dog -> run {
    val k: String = (b as Dog).kind
    k
}
    is Cat -> run {
    val k: String = (b as Cat).kind
    k
}
} as String
}

fun beastName(b: Beast): String {
    return when (b) {
    is Dog -> run {
    val n: String = (b as Dog).name
    n
}
    is Cat -> run {
    val n: String = (b as Cat).name
    n
}
} as String
}

fun beastCry(b: Beast): String {
    return when (b) {
    is Dog -> run {
    "Woof"
}
    is Cat -> run {
    "Meow"
}
} as String
}

fun bprint(b: Beast): Unit {
    println(((((beastName(b) as String + ", who's a ") + beastKind(b) as String) + ", cries: \"") + beastCry(b) as String) + "\".")
}

fun user_main(): Unit {
    val d: Beast = Dog(kind = "labrador", name = "Max")
    val c: Beast = Cat(kind = "siamese", name = "Sammy")
    bprint(d)
    bprint(c)
}

fun main() {
    user_main()
}
