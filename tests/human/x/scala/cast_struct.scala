case class Todo(title: String)

object cast_struct {
  def main(args: Array[String]): Unit = {
    val todo = Todo("hi")
    println(todo.title)
  }
}
