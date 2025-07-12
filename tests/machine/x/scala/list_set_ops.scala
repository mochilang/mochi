object list_set_ops {
  def main(args: Array[String]): Unit = {
    println((((List[Int](1, 2)) ++ (List[Int](2, 3))).distinct))
    println((((List[Int](1, 2, 3)).toSet diff (List[Int](2)).toSet).toList))
    println((((List[Int](1, 2, 3)).toSet intersect (List[Int](2, 4)).toSet).toList))
    println(((List[Int](1, 2) ++ List[Int](2, 3)).length))
  }
}
