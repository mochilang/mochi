object dataset_sort_take_limit {
  case class Product(name: String, price: Int)

  val products = List[Product](Product(name = "Laptop", price = 1500), Product(name = "Smartphone", price = 900), Product(name = "Tablet", price = 600), Product(name = "Monitor", price = 300), Product(name = "Keyboard", price = 100), Product(name = "Mouse", price = 50), Product(name = "Headphones", price = 200))
  val expensive = (for { p <- products } yield p).sortBy(p => -p.price).map(p => p).drop(1).take(3)
  def main(args: Array[String]): Unit = {
    println(("--- Top products (excluding most expensive) ---"))
    for(item <- expensive) {
      println((item.name) + " " + ("costs $") + " " + (item.price))
    }
  }
}
