case class Person(var name: String, var age: Int, var email: String)

object load_yaml {
  case class Adult(name: String, email: String)

  def _load_yaml(path: String): List[Map[String,String]] = {
    val lines = scala.io.Source.fromFile(path).getLines().toList
    val buf = scala.collection.mutable.ListBuffer[Map[String,String]]()
    var cur = Map[String,String]()
    for(l <- lines) {
      val line = l.trim
      if(line.startsWith("-")) {
        if(cur.nonEmpty) buf += cur; cur = Map()
        val rest = line.dropWhile(_ == '-').trim
        if(rest.nonEmpty && rest.contains(":")) { val i = rest.indexOf(':'); cur += rest.take(i).trim -> rest.drop(i+1).trim }
      } else if(line.contains(":")) { val i = line.indexOf(':'); cur += line.take(i).trim -> line.drop(i+1).trim }
    }
    if(cur.nonEmpty) buf += cur
    buf.toList
  }

  val people = _load_yaml("tests/interpreter/valid/people.yaml").map(r => Person(name = r("name"), age = r("age").toInt, email = r("email")))
  val adults = for { p <- people; if p.age >= 18 } yield Adult(name = p.name, email = p.email)
  def main(args: Array[String]): Unit = {
    for(a <- adults) {
      println((a.name) + " " + (a.email))
    }
  }
}
