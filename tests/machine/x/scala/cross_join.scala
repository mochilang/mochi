object cross_join {
  def main(args: Array[String]): Unit = {
    val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")), Map("id" -> (3), "name" -> ("Charlie")))
    val orders = List(Map("id" -> (100), "customerId" -> (1), "total" -> (250)), Map("id" -> (101), "customerId" -> (2), "total" -> (125)), Map("id" -> (102), "customerId" -> (1), "total" -> (300)))
    val result = for { o <- orders; c <- customers } yield Map("orderId" -> (o("id")), "orderCustomerId" -> (o("customerId")), "pairedCustomerName" -> (c("name")), "orderTotal" -> (o("total")))
    println(("--- Cross Join: All order-customer pairs ---"))
    for(entry <- result) {
      println(("Order") + " " + (entry("orderId")) + " " + ("(customerId:") + " " + (entry("orderCustomerId")) + " " + (", total: $") + " " + (entry("orderTotal")) + " " + (") paired with") + " " + (entry("pairedCustomerName")))
    }
  }
}
