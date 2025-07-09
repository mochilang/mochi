object left_join_multi {
  val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")))
  val orders = List(Map("id" -> (100), "customerId" -> (1)), Map("id" -> (101), "customerId" -> (2)))
  val items = List(Map("orderId" -> (100), "sku" -> ("a")))
  val result = for { o <- orders; c <- customers; if (o.customerId).asInstanceOf[Int] == (c.id).asInstanceOf[Int]; i = items.find(i => (o.id).asInstanceOf[Int] == (i.orderId).asInstanceOf[Int]) } yield Map("orderId" -> (o("id")), "name" -> (c("name")), "item" -> (i))
  def main(args: Array[String]): Unit = {
    println(("--- Left Join Multi ---"))
    for(r <- result) {
      println((r("orderId")) + " " + (r("name")) + " " + (r("item")))
    }
  }
}
