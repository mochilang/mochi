object save_jsonl_stdout {
  case class Auto1(name: String, age: Int)

  def _save_jsonl(rows: List[Map[String, Any]], path: String): Unit = { val out = if(path == "-") Console.out else new java.io.PrintWriter(path); rows.foreach(r => out.println(scala.util.parsing.json.JSONObject(r).toString())); if(out ne Console.out) out.close() }

  val people = List[Auto1](Auto1(name = "Alice", age = 30), Auto1(name = "Bob", age = 25))
  def main(args: Array[String]): Unit = {
    _save_jsonl(people, "-")
  }
}
