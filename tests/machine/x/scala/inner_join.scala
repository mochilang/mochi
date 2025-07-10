object inner_join {
  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")), Map[String, Any]("id" -> (3), "name" -> ("Charlie")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1), "total" -> (250)), Map[String, Int]("id" -> (101), "customerId" -> (2), "total" -> (125)), Map[String, Int]("id" -> (102), "customerId" -> (1), "total" -> (300)), Map[String, Int]("id" -> (103), "customerId" -> (4), "total" -> (80)))
  val result = for { o <- orders; c <- customers; if o("customerId") == (c.id).asInstanceOf[Int] } yield Map[String, Any]("orderId" -> (o("id")), "customerName" -> (c("name")), "total" -> (o("total")))
  def main(args: Array[String]): Unit = {
    println(("--- Orders with customer info ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry("orderId")) + " " + ("by") + " " + (entry("customerName")) + " " + ("- $") + " " + (entry("total")))
    }
  }
}
