object group_by_conditional_sum {
  case class Auto1(cat: String, val: Int, flag: Boolean)
  case class Auto2(cat: Any, share: Double)

  def _truthy(v: Any): Boolean = v match {
    case null => false
    case b: Boolean => b
    case i: Int => i != 0
    case l: Long => l != 0L
    case d: Double => d != 0.0
    case s: String => s.nonEmpty
    case m: scala.collection.Map[_, _] => m.nonEmpty
    case it: Iterable[_] => it.nonEmpty
    case opt: Option[_] => opt.nonEmpty
    case _ => true
  }

  val items = List[Auto1](Auto1(cat = "a", val = 10, flag = true), Auto1(cat = "a", val = 5, flag = false), Auto1(cat = "b", val = 20, flag = true))
  val result = (((for { i <- items } yield (i.cat, (i))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).sortBy(g => g.key)).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto2(cat = g._1, share = (for { x <- g._2 } yield if (_truthy(x.flag)) x.val else 0).sum / (for { x <- g._2 } yield x.val).sum) } }.toList
  def main(args: Array[String]): Unit = {
    println((result))
  }
}
