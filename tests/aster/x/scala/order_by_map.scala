// Generated by Mochi v0.10.35 on 2025-07-22 16:35:52 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
object Main {
  case class Item(a: Int, b: Int)
  def main(args: Array[String]): Unit = {
    val data: ArrayBuffer[Item] = ArrayBuffer(Item(1, 2), Item(1, 1), Item(0, 5))
    val sorted: ArrayBuffer[Item] = ({
      var _tmp = ArrayBuffer[(Map[String, Any], Item)]()
      for (x <- data) {
        _tmp.append((Map("a" -> x.a, "b" -> x.b), x))
      }
      var _res = _tmp.sortBy(() => (t._1("a").asInstanceOf[Int], t._1("b").asInstanceOf[Int])).map()
      _res
    })
    println(sorted)
  }
}
