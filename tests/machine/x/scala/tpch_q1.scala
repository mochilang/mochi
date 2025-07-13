object tpch_q1 {
  case class Auto2(returnflag: String, linestatus: String, sum_qty: Int, sum_base_price: Int, sum_disc_price: Double, sum_charge: Double, avg_qty: Double, avg_price: Int, avg_disc: Double, count_order: Int)
  case class G(returnflag: String, linestatus: String)
  case class Lineitem(l_quantity: Int, l_extendedprice: Double, l_discount: Double, l_tax: Double, l_returnflag: String, l_linestatus: String, l_shipdate: String)
  case class Result(returnflag: Any, linestatus: Any, sum_qty: Int, sum_base_price: Int, sum_disc_price: Int, sum_charge: Int, avg_qty: Double, avg_price: Double, avg_disc: Double, count_order: Int)
  case class Result1(returnflag: String, linestatus: String, sum_qty: Int, sum_base_price: Int, sum_disc_price: Int, sum_charge: Int, avg_qty: Double, avg_price: Double, avg_disc: Double, count_order: Int)

  case class _Group[K,T](key: K, items: List[T]) extends Iterable[T] { def iterator: Iterator[T] = items.iterator }

  val lineitem = List(Lineitem(l_quantity = 17, l_extendedprice = 1000, l_discount = 0.05, l_tax = 0.07, l_returnflag = "N", l_linestatus = "O", l_shipdate = "1998-08-01"), Lineitem(l_quantity = 36, l_extendedprice = 2000, l_discount = 0.1, l_tax = 0.05, l_returnflag = "N", l_linestatus = "O", l_shipdate = "1998-09-01"), Lineitem(l_quantity = 25, l_extendedprice = 1500, l_discount = 0, l_tax = 0.08, l_returnflag = "R", l_linestatus = "F", l_shipdate = "1998-09-03"))
  val result = ((for { row <- lineitem; if row.l_shipdate <= "1998-09-02" } yield (Map("returnflag" -> row.l_returnflag, "linestatus" -> row.l_linestatus), row)).groupBy(_._1).map{ case(k,list) => _Group(k, list.map(_._2)) }.toList).map{ g => Map("returnflag" -> g.key.returnflag, "linestatus" -> g.key.linestatus, "sum_qty" -> (for { x <- g } yield x.l_quantity).sum, "sum_base_price" -> (for { x <- g } yield x.l_extendedprice).sum, "sum_disc_price" -> (for { x <- g } yield x.l_extendedprice * (1 - x.l_discount)).sum, "sum_charge" -> (for { x <- g } yield x.l_extendedprice * (1 - x.l_discount) * (1 + x.l_tax)).sum, "avg_qty" -> (for { x <- g } yield x.l_quantity).sum.toDouble / (for { x <- g } yield x.l_quantity).size, "avg_price" -> (for { x <- g } yield x.l_extendedprice).sum.toDouble / (for { x <- g } yield x.l_extendedprice).size, "avg_disc" -> (for { x <- g } yield x.l_discount).sum.toDouble / (for { x <- g } yield x.l_discount).size, "count_order" -> (g).size) }.toList
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(result).toString())
    assert(result == List(Auto2(returnflag = "N", linestatus = "O", sum_qty = 53, sum_base_price = 3000, sum_disc_price = 950 + 1800, sum_charge = (950 * 1.07) + (1800 * 1.05), avg_qty = 26.5, avg_price = 1500, avg_disc = 0.07500000000000001, count_order = 2)))
  }
}
