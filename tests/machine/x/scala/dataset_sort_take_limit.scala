object dataset_sort_take_limit {
  val products = List(Map("name" -> ("Laptop"), "price" -> (1500)), Map("name" -> ("Smartphone"), "price" -> (900)), Map("name" -> ("Tablet"), "price" -> (600)), Map("name" -> ("Monitor"), "price" -> (300)), Map("name" -> ("Keyboard"), "price" -> (100)), Map("name" -> ("Mouse"), "price" -> (50)), Map("name" -> ("Headphones"), "price" -> (200)))
  val expensive = (for { p <- products } yield p).sortBy(p => -p("price")).drop(1).take(3)
  def main(args: Array[String]): Unit = {
    println(("--- Top products (excluding most expensive) ---"))
    for(item <- expensive) {
      println((item("name")) + " " + ("costs $") + " " + (item("price")))
    }
  }
}
