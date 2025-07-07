object group_by_multi_join {
  case class Nation(id: Int, name: String)
  case class Supplier(id: Int, nation: Int)
  case class PartSupp(part: Int, supplier: Int, cost: Double, qty: Int)
  case class Result(part: Int, total: Double)

  def main(args: Array[String]): Unit = {
    val nations = List(Nation(1, "A"), Nation(2, "B"))
    val suppliers = List(Supplier(1, 1), Supplier(2, 2))
    val partsupp = List(
      PartSupp(100, 1, 10.0, 2),
      PartSupp(100, 2, 20.0, 1),
      PartSupp(200, 1, 5.0, 3)
    )

    val filtered = for {
      ps <- partsupp
      s <- suppliers if s.id == ps.supplier
      n <- nations if n.id == s.nation && n.name == "A"
    } yield (ps.part, ps.cost * ps.qty)

    val grouped = filtered.groupBy(_._1).map { case (part, list) =>
      Result(part, list.map(_._2).sum)
    }

    grouped.foreach(r => println(s"{part: ${r.part}, total: ${r.total}}"))
  }
}
