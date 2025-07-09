object json_builtin {
  val m = Map("a" -> (1), "b" -> (2))
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(m).toString())
  }
}
