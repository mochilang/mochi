case class Todo(var title: String)

object cast_struct {
  def main(args: Array[String]): Unit = {
    val todo = Todo(title = "hi")
    println((todo.title))
  }
}
