object dataset_sort_take_limit {
  case class Auto1(name: String, price: Int)

  val products = List[Auto1](Auto1(name = "Laptop", price = 1500), Auto1(name = "Smartphone", price = 900), Auto1(name = "Tablet", price = 600), Auto1(name = "Monitor", price = 300), Auto1(name = "Keyboard", price = 100), Auto1(name = "Mouse", price = 50), Auto1(name = "Headphones", price = 200))
  val expensive = (for { p <- products } yield p).sortBy(p => -p.price).drop(1).take(3)
  def main(args: Array[String]): Unit = {
    println(("--- Top products (excluding most expensive) ---"))
    for(item <- expensive) {
      println((item.name) + " " + ("costs $") + " " + (item.price))
    }
  }
}
