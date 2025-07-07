object for_map_collection {
  def main(args: Array[String]): Unit = {
    var m = Map("a" -> 1, "b" -> 2)
    for((k, _) <- m) {
      println(k)
    }
  }
}
