object outer_join {
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

  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")), Map[String, Any]("id" -> (3), "name" -> ("Charlie")), Map[String, Any]("id" -> (4), "name" -> ("Diana")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1), "total" -> (250)), Map[String, Int]("id" -> (101), "customerId" -> (2), "total" -> (125)), Map[String, Int]("id" -> (102), "customerId" -> (1), "total" -> (300)), Map[String, Int]("id" -> (103), "customerId" -> (5), "total" -> (80)))
  val result = for { o <- orders; c = customers.find(c => o("customerId") == (c.id).asInstanceOf[Int]) } yield Map[String, Any]("order" -> (o), "customer" -> (c))
  def main(args: Array[String]): Unit = {
    println(("--- Outer Join using syntax ---"))
    for(row <- result) {
      if (_truthy(row("order"))) {
        if (_truthy(row("customer"))) {
          println(("Order") + " " + (row("order")("id")) + " " + ("by") + " " + (row("customer")("name")) + " " + ("- $") + " " + (row("order")("total")))
        } else {
          println(("Order") + " " + (row("order")("id")) + " " + ("by") + " " + ("Unknown") + " " + ("- $") + " " + (row("order")("total")))
        }
      } else {
        println(("Customer") + " " + (row("customer")("name")) + " " + ("has no orders"))
      }
    }
  }
}
