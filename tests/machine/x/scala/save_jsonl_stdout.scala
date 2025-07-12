object save_jsonl_stdout {
  case class People(name: String, age: Int)

  def _save_jsonl(rows: Iterable[Any], path: String): Unit = {
    def toMap(v: Any): Map[String,Any] = v match {
      case m: Map[_, _] => m.asInstanceOf[Map[String,Any]]
      case p: Product => p.getClass.getDeclaredFields.map(_.getName).zip(p.productIterator.toList).toMap.asInstanceOf[Map[String,Any]]
      case x => Map("value" -> x)
    }
    val out: java.io.PrintWriter = if(path == "-") new java.io.PrintWriter(System.out) else new java.io.PrintWriter(path)
    rows.foreach(r => out.println(scala.util.parsing.json.JSONObject(toMap(r)).toString()))
    out.flush()
    if(out ne Console.out) out.close()
  }

  val people = List(People(name = "Alice", age = 30), People(name = "Bob", age = 25))
  def main(args: Array[String]): Unit = {
    _save_jsonl(people, "-")
  }
}
