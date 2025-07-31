fun main() {
    val products: MutableList<MutableMap<String, Any>> = mutableListOf(mutableMapOf(user_type, user_type), mutableMapOf(user_type, user_type), mutableMapOf(user_type, user_type), mutableMapOf(user_type, user_type), mutableMapOf(user_type, user_type), mutableMapOf(user_type, user_type), mutableMapOf(user_type, user_type))
    val expensive: MutableList<MutableMap<String, Any>> = run().drop(1).take(3).toMutableList()
    println("--- Top products (excluding most expensive) ---")
    for (item in expensive) {
        println(listOf(unary_expression, "costs ", unary_expression).joinToString(" "))
    }
}
