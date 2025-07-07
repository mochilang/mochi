object group_by_multi_join_sort {
  case class Nation(n_nationkey: Int, n_name: String)
  case class Customer(c_custkey: Int, c_name: String, c_acctbal: Double,
                      c_nationkey: Int, c_address: String, c_phone: String,
                      c_comment: String)
  case class Order(o_orderkey: Int, o_custkey: Int, o_orderdate: String)
  case class LineItem(l_orderkey: Int, l_returnflag: String,
                      l_extendedprice: Double, l_discount: Double)
  case class Key(c_custkey: Int, c_name: String, c_acctbal: Double,
                 c_address: String, c_phone: String, c_comment: String,
                 n_name: String)
  case class Result(key: Key, revenue: Double)

  def main(args: Array[String]): Unit = {
    val nation = List(Nation(1, "BRAZIL"))
    val customer = List(
      Customer(1, "Alice", 100.0, 1, "123 St", "123-456", "Loyal")
    )
    val orders = List(
      Order(1000, 1, "1993-10-15"),
      Order(2000, 1, "1994-01-02")
    )
    val lineitem = List(
      LineItem(1000, "R", 1000.0, 0.1),
      LineItem(2000, "N", 500.0, 0.0)
    )

    val startDate = "1993-10-01"
    val endDate = "1994-01-01"

    val joined = for {
      c <- customer
      o <- orders if o.o_custkey == c.c_custkey
      l <- lineitem if l.l_orderkey == o.o_orderkey
      n <- nation if n.n_nationkey == c.c_nationkey
      if o.o_orderdate >= startDate && o.o_orderdate < endDate &&
         l.l_returnflag == "R"
    } yield (
      Key(c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone,
          c.c_comment, n.n_name),
      l.l_extendedprice * (1 - l.l_discount)
    )

    val grouped = joined.groupBy(_._1).map { case (k, list) =>
      Result(k, list.map(_._2).sum)
    }.toList
      .sortBy(r => -r.revenue)

    grouped.foreach { r =>
      println(s"${r.key.c_name} revenue ${r.revenue}")
    }
  }
}
