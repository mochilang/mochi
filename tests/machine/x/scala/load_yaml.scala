case class Person(var name: String, var age: Int, var email: String)

object load_yaml {
  def _load_yaml(path: String): List[Map[String, String]] = { val lines = scala.io.Source.fromFile(path).getLines().toList; lines.grouped(3).flatMap { case List(n,a,e) => Some(Map("name"->n.split(':')(1).trim, "age"->a.split(':')(1).trim, "email"->e.split(':')(1).trim)); case _ => None }.toList }

  val people = _load_yaml("../interpreter/valid/people.yaml")
  val adults = for { p <- people; if p.age >= 18 } yield Map("name" -> (p.name), "email" -> (p.email))
  def main(args: Array[String]): Unit = {
    for(a <- adults) {
      println((a("name")) + " " + (a("email")))
    }
  }
}
