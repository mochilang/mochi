object outer_join {
  case class Customer(id: Int, name: String)
  case class Order(id: Int, customerId: Int, total: Int)
  case class Result(order: Option[Order], customer: Option[Customer])
  case class Result1(order: Order, customer: Customer)

  def _outer_join[A,B](a: List[A], b: List[B])(cond: (A,B) => Boolean): List[(Option[A], Option[B])] = { val left = _left_join(a,b)(cond).map{ case(x,y) => (Some(x), y) }; val right = _right_join(a,b)(cond).collect{ case(None, r) => (None, Some(r)) }; left ++ right }

  val customers = List[Customer](Customer(id = 1, name = "Alice"), Customer(id = 2, name = "Bob"), Customer(id = 3, name = "Charlie"), Customer(id = 4, name = "Diana"))
  val orders = List[Order](Order(id = 100, customerId = 1, total = 250), Order(id = 101, customerId = 2, total = 125), Order(id = 102, customerId = 1, total = 300), Order(id = 103, customerId = 5, total = 80))
  val result = for { _pair0 <- _outer_join(orders, customers)((o,c) => o.customerId == (c.id).asInstanceOf[Int]); o = _pair0._1; c = _pair0._2 } yield Result(order = o, customer = c)
  def main(args: Array[String]): Unit = {
    println(("--- Outer Join using syntax ---"))
    for(row <- result) {
      if (row.order != null) {
        if (row.customer != null) {
          println(("Order") + " " + (row.order.id) + " " + ("by") + " " + (row.customer.name) + " " + ("- $") + " " + (row.order.total))
        } else {
          println(("Order") + " " + (row.order.id) + " " + ("by") + " " + ("Unknown") + " " + ("- $") + " " + (row.order.total))
        }
      } else {
        println(("Customer") + " " + (row.customer.name) + " " + ("has no orders"))
      }
    }
  }
}
