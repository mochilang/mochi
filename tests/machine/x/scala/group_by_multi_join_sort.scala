object group_by_multi_join_sort {
  case class Customer(c_custkey: Int, c_name: String, c_acctbal: Double, c_nationkey: Int, c_address: String, c_phone: String, c_comment: String)
  case class Lineitem(l_orderkey: Int, l_returnflag: String, l_extendedprice: Double, l_discount: Double)
  case class Nation(n_nationkey: Int, n_name: String)
  case class Order(o_orderkey: Int, o_custkey: Int, o_orderdate: String)
  case class Result(c_custkey: Int, c_name: String, c_acctbal: Double, c_address: String, c_phone: String, c_comment: String, n_name: String)
  case class Result1(c: Customer, o: Order, l: Lineitem, n: Nation)
  case class Result2(c_custkey: Int, c_name: String, revenue: Int, c_acctbal: Double, n_name: String, c_address: String, c_phone: String, c_comment: String)

  case class _Group[K,T](key: K, items: List[T])

  val nation = List[Nation](Nation(n_nationkey = 1, n_name = "BRAZIL"))
  val customer = List[Customer](Customer(c_custkey = 1, c_name = "Alice", c_acctbal = 100, c_nationkey = 1, c_address = "123 St", c_phone = "123-456", c_comment = "Loyal"))
  val orders = List[Order](Order(o_orderkey = 1000, o_custkey = 1, o_orderdate = "1993-10-15"), Order(o_orderkey = 2000, o_custkey = 1, o_orderdate = "1994-01-02"))
  val lineitem = List[Lineitem](Lineitem(l_orderkey = 1000, l_returnflag = "R", l_extendedprice = 1000, l_discount = 0.1), Lineitem(l_orderkey = 2000, l_returnflag = "N", l_extendedprice = 500, l_discount = 0))
  val start_date = "1993-10-01"
  val end_date = "1994-01-01"
  val result = (((for { c <- customer; o <- orders; if (o.o_custkey).asInstanceOf[Int] == c.c_custkey; l <- lineitem; if (l.l_orderkey).asInstanceOf[Int] == o.o_orderkey; n <- nation; if (n.n_nationkey).asInstanceOf[Int] == c.c_nationkey; if o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == "R" } yield (Result(c_custkey = c.c_custkey, c_name = c.c_name, c_acctbal = c.c_acctbal, c_address = c.c_address, c_phone = c.c_phone, c_comment = c.c_comment, n_name = n.n_name), Result1(c = c, o = o, l = l, n = n))).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).sortBy(g => -(for { x <- g } yield x.l.l_extendedprice * (1 - x.l.l_discount)).sum)).map{ g => Result2(c_custkey = g.key.c_custkey, c_name = g.key.c_name, revenue = (for { x <- g } yield x.l.l_extendedprice * (1 - x.l.l_discount)).sum, c_acctbal = g.key.c_acctbal, n_name = g.key.n_name, c_address = g.key.c_address, c_phone = g.key.c_phone, c_comment = g.key.c_comment) }.toList
  def main(args: Array[String]): Unit = {
    println((result))
  }
}
