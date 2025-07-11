object save_jsonl_stdout {
  case class People(name: String, age: Int)

  def _save_jsonl(rows: List[Map[String, Any]], path: String): Unit = { val out = if(path == "-") Console.out else new java.io.PrintWriter(path); rows.foreach(r => out.println(scala.util.parsing.json.JSONObject(r).toString())); if(out ne Console.out) out.close() }

  val people = List[People](People(name = "Alice", age = 30), People(name = "Bob", age = 25))
  def main(args: Array[String]): Unit = {
    _save_jsonl(people, "-")
  }
}
