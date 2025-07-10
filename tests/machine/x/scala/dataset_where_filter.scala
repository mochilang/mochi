object dataset_where_filter {
  def _truthy(v: Any): Boolean = v match {
    case null => false
    case b: Boolean => b
    case i: Int => i != 0
    case l: Long => l != 0L
    case d: Double => d != 0.0
    case s: String => s.nonEmpty
    case m: scala.collection.Map[_, _] => m.nonEmpty
    case it: Iterable[_] => it.nonEmpty
    case opt: Option[_] => opt.nonEmpty
    case _ => true
  }

  val people = List[Map[String, Any]](Map[String, Any]("name" -> ("Alice"), "age" -> (30)), Map[String, Any]("name" -> ("Bob"), "age" -> (15)), Map[String, Any]("name" -> ("Charlie"), "age" -> (65)), Map[String, Any]("name" -> ("Diana"), "age" -> (45)))
  val adults = for { person <- people; if person("age") >= 18 } yield Map[String, Any]("name" -> (person("name")), "age" -> (person("age")), "is_senior" -> (person("age") >= 60))
  def main(args: Array[String]): Unit = {
    println(("--- Adults ---"))
    for(person <- adults) {
      println((person("name")) + " " + ("is") + " " + (person("age")) + " " + (if (_truthy(person("is_senior"))) " (senior)" else ""))
    }
  }
}
