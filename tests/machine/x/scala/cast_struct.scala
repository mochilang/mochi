case class Todo(var title: String)

object cast_struct {
  case class Todo(title: String)

  val todo = Todo(title = "hi")
  def main(args: Array[String]): Unit = {
    println((todo.title))
  }
}
