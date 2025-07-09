object join_multi {
  val customers = List(Map("id" -> (1), "name" -> ("Alice")), Map("id" -> (2), "name" -> ("Bob")))
  val orders = List(Map("id" -> (100), "customerId" -> (1)), Map("id" -> (101), "customerId" -> (2)))
  val items = List(Map("orderId" -> (100), "sku" -> ("a")), Map("orderId" -> (101), "sku" -> ("b")))
  val result = for { o <- orders; c <- customers; if (o.customerId).asInstanceOf[Int] == (c.id).asInstanceOf[Int]; i <- items; if (o.id).asInstanceOf[Int] == (i.orderId).asInstanceOf[Int] } yield Map("name" -> (c("name")), "sku" -> (i("sku")))
  def main(args: Array[String]): Unit = {
    println(("--- Multi Join ---"))
    for(r <- result) {
      println((r("name")) + " " + ("bought item") + " " + (r("sku")))
    }
  }
}
