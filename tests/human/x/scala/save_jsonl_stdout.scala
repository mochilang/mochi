object save_jsonl_stdout {
  def main(args: Array[String]): Unit = {
    val people = List(Map("name" -> "Alice", "age" -> 30), Map("name" -> "Bob", "age" -> 25))
    people.foreach(p => println(scala.util.parsing.json.JSONObject(p).toString()))
  }
}
