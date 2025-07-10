object left_join_multi {
  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1)), Map[String, Int]("id" -> (101), "customerId" -> (2)))
  val items = List[Map[String, Any]](Map[String, Any]("orderId" -> (100), "sku" -> ("a")))
  val result = for { o <- orders; c <- customers; if o("customerId") == (c.id).asInstanceOf[Int]; i = items.find(i => o("id") == (i.orderId).asInstanceOf[Int]) } yield Map[String, Any]("orderId" -> (o("id")), "name" -> (c("name")), "item" -> (i))
  def main(args: Array[String]): Unit = {
    println(("--- Left Join Multi ---"))
    for(r <- result) {
      println((r("orderId")) + " " + (r("name")) + " " + (r("item")))
    }
  }
}
