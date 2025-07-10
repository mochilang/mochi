object inner_join {
  val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")), Map("id" -> (3), "name" -> ("Charlie")))
  val orders = List(Map("id" -> (100), "customerId" -> (1), "total" -> (250)), Map("id" -> (101), "customerId" -> (2), "total" -> (125)), Map("id" -> (102), "customerId" -> (1), "total" -> (300)), Map("id" -> (103), "customerId" -> (4), "total" -> (80)))
  val result = for { o <- orders; c <- customers; if o("customerId") == (c.id).asInstanceOf[Int] } yield Map("orderId" -> (o("id")), "customerName" -> (c("name")), "total" -> (o("total")))
  def main(args: Array[String]): Unit = {
    println(("--- Orders with customer info ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry("orderId")) + " " + ("by") + " " + (entry("customerName")) + " " + ("- $") + " " + (entry("total")))
    }
  }
}
