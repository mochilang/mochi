// Generated by Mochi v0.10.35 on 2025-07-22 14:24:26 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.immutable.ListMap
object Main {
  def toJson(value: Any, indent: Int = 0): String = value match {
    case m: scala.collection.Map[_, _] =>
    val items = ListMap(m.toSeq.sortBy(_._1.toString): _*).toSeq.map{ case (k,v) => "  "*(indent+1)+"\""+k.toString+"\": "+toJson(v, indent+1) }
    "{\n"+items.mkString(",\n")+"\n"+"  "*indent+"}"
    case s: Seq[_] =>
    val items = s.map(x => "  "*(indent+1)+toJson(x, indent+1))
    "[\n"+items.mkString(",\n")+"\n"+"  "*indent+"]"
    case s: String => "\""+s+"\""
    case other => other.toString
  }
  
  case class Item(name: String, city: String)
  
  def main(args: Array[String]): Unit = {
    val people: ArrayBuffer[Item] = ArrayBuffer(Item("Alice", "Paris"), Item("Bob", "Hanoi"), Item("Charlie", "Paris"), Item("Diana", "Hanoi"), Item("Eve", "Paris"), Item("Frank", "Hanoi"), Item("George", "Paris"))
    val big: ArrayBuffer[Any] = ArrayBuffer.from((for (p <- people) yield (p.city, Map("p" -> p)))).groupBy(_._1).map{ case (k, arr) => Map("key" -> k, "items" -> ArrayBuffer(arr.map(_._2).toSeq: _*)) }.filter(g => g("items").size >= 4).map(g => Map("city" -> g("key"), "num" -> g("items").size)))
    println(toJson(big))
  }
}
