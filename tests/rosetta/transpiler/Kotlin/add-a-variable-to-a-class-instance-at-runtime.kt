fun input(): String = readLine() ?: ""

data class SomeStruct(val runtimeFields: MutableMap<String, String>)
fun user_main(): Unit {
    var ss: SomeStruct = SomeStruct(runtimeFields = mutableMapOf<Any, Any>())
    println("Create two fields at runtime: \n")
    var i: Int = 1
    while (i <= 2) {
        println(("  Field #" + i.toString()) + ":\n")
        println("       Enter name  : ")
        val name: String = input()
        println("       Enter value : ")
        val value: String = input()
        var fields = ss.runtimeFields
        (fields as MutableMap<String, Any?>)[name] = value
        ss.runtimeFields = fields
        println("\n")
        i = i + 1
    }
    while (true) {
        println("Which field do you want to inspect ? ")
        val name: String = input()
        if (name in ss.runtimeFields) {
            val value: String = (ss.runtimeFields)[name]!!
            println(("Its value is '" + value) + "'")
            return
        } else {
            println("There is no field of that name, try again\n")
        }
    }
}

fun main() {
    user_main()
}
