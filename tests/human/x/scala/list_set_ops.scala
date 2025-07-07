object list_set_ops {
  def main(args: Array[String]): Unit = {
    println((Set(1,2) union Set(2,3)).toList)
    println((Set(1,2,3) diff Set(2)).toList)
    println((Set(1,2,3) intersect Set(2,4)).toList)
    println((List(1,2) ++ List(2,3)).length)
  }
}
