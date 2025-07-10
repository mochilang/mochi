object list_set_ops {
  def _except[T](a: List[T], b: List[T]): List[T] = { val remove = b.toSet; a.filterNot(remove) }

  def _intersect[T](a: List[T], b: List[T]): List[T] = { val keep = b.toSet; val res = scala.collection.mutable.ListBuffer[T](); val seen = scala.collection.mutable.Set[T](); for(x <- a if keep(x) && !seen(x)) { seen += x; res += x }; res.toList }

  def _union[T](a: List[T], b: List[T]): List[T] = { val set = scala.collection.mutable.LinkedHashSet[T](); set ++= a; set ++= b; set.toList }

  def _union_all[T](a: List[T], b: List[T]): List[T] = a ++ b

  def main(args: Array[String]): Unit = {
    println((_union(List[Int](1, 2), List[Int](2, 3))))
    println((_except(List[Int](1, 2, 3), List[Int](2))))
    println((_intersect(List[Int](1, 2, 3), List[Int](2, 4))))
    println((_union_all(List[Int](1, 2), List[Int](2, 3)).length))
  }
}
