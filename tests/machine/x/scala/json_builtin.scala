object json_builtin {
  case class Auto1(a: Int, b: Int)

  val m = Auto1(a = 1, b = 2)
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(m).toString())
  }
}
