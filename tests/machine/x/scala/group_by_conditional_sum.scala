// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
object group_by_conditional_sum {
  case class Item(cat: String, `val`: Int, flag: Boolean)
  case class Result(cat: String, share: Double)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  def main(args: Array[String]): Unit = {
    val items = List(Item(cat = "a", `val` = 10, flag = true), Item(cat = "a", `val` = 5, flag = false), Item(cat = "b", `val` = 20, flag = true))
    val result = (((for { i <- items } yield (i.cat, i)).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).sortBy(g => g.key)).map{ g => Result(cat = g.key, share = (for { x <- g } yield if (x.flag) x.`val` else 0).sum / (for { x <- g } yield x.`val`).sum) }.toList
    println(result)
  }
}
