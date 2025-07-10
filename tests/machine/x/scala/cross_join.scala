object cross_join {
  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")), Map[String, Any]("id" -> (3), "name" -> ("Charlie")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1), "total" -> (250)), Map[String, Int]("id" -> (101), "customerId" -> (2), "total" -> (125)), Map[String, Int]("id" -> (102), "customerId" -> (1), "total" -> (300)))
  val result = for { o <- orders; c <- customers } yield Map[String, Any]("orderId" -> (o("id")), "orderCustomerId" -> (o("customerId")), "pairedCustomerName" -> (c("name")), "orderTotal" -> (o("total")))
  def main(args: Array[String]): Unit = {
    println(("--- Cross Join: All order-customer pairs ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry("orderId")) + " " + ("(customerId:") + " " + (entry("orderCustomerId")) + " " + (", total: $") + " " + (entry("orderTotal")) + " " + (") paired with") + " " + (entry("pairedCustomerName")))
    }
  }
}
