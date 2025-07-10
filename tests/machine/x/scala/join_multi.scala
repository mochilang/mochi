object join_multi {
  val customers = List[Map[String, Any]](Map[String, Any]("id" -> (1), "name" -> ("Alice")), Map[String, Any]("id" -> (2), "name" -> ("Bob")))
  val orders = List[Map[String, Int]](Map[String, Int]("id" -> (100), "customerId" -> (1)), Map[String, Int]("id" -> (101), "customerId" -> (2)))
  val items = List[Map[String, Any]](Map[String, Any]("orderId" -> (100), "sku" -> ("a")), Map[String, Any]("orderId" -> (101), "sku" -> ("b")))
  val result = for { o <- orders; c <- customers; if o("customerId") == (c.id).asInstanceOf[Int]; i <- items; if o("id") == (i.orderId).asInstanceOf[Int] } yield Map[String, String]("name" -> (c("name")), "sku" -> (i("sku")))
  def main(args: Array[String]): Unit = {
    println(("--- Multi Join ---"))
    for(r <- result) {
      println((r("name")) + " " + ("bought item") + " " + (r("sku")))
    }
  }
}
