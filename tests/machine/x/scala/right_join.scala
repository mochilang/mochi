object right_join {
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

  val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")), Map("id" -> (3), "name" -> ("Charlie")), Map("id" -> (4), "name" -> ("Diana")))
  val orders = List(Map("id" -> (100), "customerId" -> (1), "total" -> (250)), Map("id" -> (101), "customerId" -> (2), "total" -> (125)), Map("id" -> (102), "customerId" -> (1), "total" -> (300)))
  val result = for { c <- customers; o = orders.find(o => (o.customerId).asInstanceOf[Int] == (c.id).asInstanceOf[Int]) } yield Map("customerName" -> (c("name")), "order" -> (o))
  def main(args: Array[String]): Unit = {
    println(("--- Right Join using syntax ---"))
    for(entry <- result) {
      if (_truthy(entry("order"))) {
        println(("Customer") + " " + (entry("customerName")) + " " + ("has order") + " " + (entry("order")("id")) + " " + ("- $") + " " + (entry("order")("total")))
      } else {
        println(("Customer") + " " + (entry("customerName")) + " " + ("has no orders"))
      }
    }
  }
}
