object associative_array_merging {
  def merge(base: Map[String, any], update: Map[String, any]): Map[String, any] = {
    var result: Map[String, any] = scala.collection.mutable.Map()
    for((k, _) <- base) {
      result(k) = (base).apply(k)
    }
    for((k, _) <- update) {
      result(k) = (update).apply(k)
    }
    return result
  }
  
  def main() = {
    val base: Map[String, any] = Map("name" -> "Rocket Skates", "price" -> 12.75, "color" -> "yellow")
    val update: Map[String, any] = Map("price" -> 15.25, "color" -> "red", "year" -> 1974)
    val result = merge(base, update)
    println(result)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
