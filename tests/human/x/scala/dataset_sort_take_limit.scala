object dataset_sort_take_limit {
  case class Product(name: String, price: Int)

  def main(args: Array[String]): Unit = {
    val products = List(
      Product("Laptop", 1500),
      Product("Smartphone", 900),
      Product("Tablet", 600),
      Product("Monitor", 300),
      Product("Keyboard", 100),
      Product("Mouse", 50),
      Product("Headphones", 200)
    )

    val sorted = products.sortBy(-_.price)
    val expensive = sorted.slice(1, 4)

    println("--- Top products (excluding most expensive) ---")
    expensive.foreach(p => println(s"${p.name} costs $${p.price}"))
  }
}

