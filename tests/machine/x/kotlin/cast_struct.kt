// Code generated from tests/vm/valid/cast_struct.mochi

data class Todo(var title: String)

val todo = Todo(title = "hi")

fun main() {
    println(todo.title)
}
