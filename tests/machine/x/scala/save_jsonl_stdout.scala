object save_jsonl_stdout {
  def _save_jsonl(rows: List[Map[String, Any]], path: String): Unit = { val out = if(path == "-") Console.out else new java.io.PrintWriter(path); rows.foreach(r => out.println(scala.util.parsing.json.JSONObject(r).toString())); if(out ne Console.out) out.close() }

  val people = List(Map("name" -> ("Alice"), "age" -> (30)), Map("name" -> ("Bob"), "age" -> (25)))
  def main(args: Array[String]): Unit = {
    _save_jsonl(people, "-")
  }
}
