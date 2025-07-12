object group_by_multi_join {
  case class Filtered(part: Int, value: Double)
  case class Grouped(part: Int, total: Int)
  case class Nation(id: Int, name: String)
  case class Partsupp(part: Int, supplier: Int, cost: Double, qty: Int)
  case class Supplier(id: Int, nation: Int)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  val nations = List[Nation](Nation(id = 1, name = "A"), Nation(id = 2, name = "B"))
  val suppliers = List[Supplier](Supplier(id = 1, nation = 1), Supplier(id = 2, nation = 2))
  val partsupp = List[Partsupp](Partsupp(part = 100, supplier = 1, cost = 10, qty = 2), Partsupp(part = 100, supplier = 2, cost = 20, qty = 1), Partsupp(part = 200, supplier = 1, cost = 5, qty = 3))
  val filtered = for { ps <- partsupp; s <- suppliers; if (s.id).asInstanceOf[Int] == ps.supplier; n <- nations; if (n.id).asInstanceOf[Int] == s.nation; if n.name == "A" } yield Filtered(part = ps.part, value = ps.cost * ps.qty)
  val grouped = ((for { x <- filtered } yield (x.part, x)).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).map{ g => Grouped(part = g.key, total = (for { r <- g } yield r.value).sum) }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
