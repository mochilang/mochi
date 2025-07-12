object list_set_ops {
  def main(args: Array[String]): Unit = {
    println(((List(1, 2)) ++ (List(2, 3))).distinct)
    println(((List(1, 2, 3)).toSet diff (List(2)).toSet).toList)
    println(((List(1, 2, 3)).toSet intersect (List(2, 4)).toSet).toList)
    println((List(1, 2) ++ List(2, 3)).length)
  }
}
