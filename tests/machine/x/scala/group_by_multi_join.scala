object group_by_multi_join {
  case class Auto1(id: Int, name: String)
  case class Auto2(id: Int, nation: Int)
  case class Auto3(part: Int, supplier: Int, cost: Double, qty: Int)
  case class Auto4(part: Int, value: Double)
  case class Auto5(part: Int, total: Int)

  val nations = List[Auto1](Auto1(id = 1, name = "A"), Auto1(id = 2, name = "B"))
  val suppliers = List[Auto2](Auto2(id = 1, nation = 1), Auto2(id = 2, nation = 2))
  val partsupp = List[Auto3](Auto3(part = 100, supplier = 1, cost = 10, qty = 2), Auto3(part = 100, supplier = 2, cost = 20, qty = 1), Auto3(part = 200, supplier = 1, cost = 5, qty = 3))
  val filtered = for { ps <- partsupp; s <- suppliers; if (s.id).asInstanceOf[Int] == ps.supplier; n <- nations; if (n.id).asInstanceOf[Int] == s.nation; if n.name == "A" } yield Auto4(part = ps.part, value = ps.cost * ps.qty)
  val grouped = ((for { x <- filtered } yield (x.part, (x))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto5(part = g._1, total = (for { r <- g._2 } yield r.value).sum) } }.toList
  def main(args: Array[String]): Unit = {
    println((grouped))
  }
}
