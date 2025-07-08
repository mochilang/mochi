case class Todo(title: String)

object cast_struct {
  def main(args: Array[String]): Unit = {
    case class Todo(title: String)
    val todo = Todo(title = "hi")
    println(todo.title)
  }
}
