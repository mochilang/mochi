object load_yaml {
  case class Person(name: String, age: Int, email: String)

  def parse(path: String): List[Person] = {
    val lines = scala.io.Source.fromFile(path).getLines().toList
    lines.grouped(3).flatMap {
      case List(n, a, e) =>
        val name = n.split(':')(1).trim
        val age = a.split(':')(1).trim.toInt
        val email = e.split(':')(1).trim
        Some(Person(name, age, email))
      case _ => None
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val people = parse("../interpreter/valid/people.yaml")
    val adults = people.filter(_.age >= 18).map(p => (p.name, p.email))
    adults.foreach { case (n, e) => println(s"$n $e") }
  }
}
