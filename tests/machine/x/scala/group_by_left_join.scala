object group_by_left_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int)
  case class Stat(c: Customer, o: Option[Order])
  case class Stat1(name: String, count: Int)

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

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"))
  val orders = List[Order](Order(id = 100, customerId = 1), Order(id = 101, customerId = 1), Order(id = 102, customerId = 2))
  val stats = ((for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == c.id) } yield (c.name, Stat(c = c, o = o))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Stat1(name = g.key, count = (for { r <- g._2; if _truthy(r.o) } yield r).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- Group Left Join ---"))
    for(s <- stats) {
      println((s.name) + " " + ("orders:") + " " + (s.count))
    }
  }
}
