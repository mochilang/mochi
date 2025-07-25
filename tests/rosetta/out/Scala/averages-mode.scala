object averages_mode {
  def main(args: Array[String]): Unit = {
    var arr1 = scala.collection.mutable.ArrayBuffer(2, 7, 1, 8, 2)
    var counts1: Map[Int, Int] = scala.collection.mutable.Map()
    var keys1: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < arr1.length) {
      val v = (arr1).apply(i)
      if (counts1.contains(v)) {
        counts1(v) = (counts1).apply(v) + 1
      } else {
        counts1(v) = 1
        keys1 = keys1 :+ v
      }
      i += 1
    }
    var max1 = 0
    i = 0
    while (i < keys1.length) {
      val k = (keys1).apply(i)
      val c = (counts1).apply(k)
      if (c > max1) {
        max1 = c
      }
      i += 1
    }
    var modes1: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    i = 0
    while (i < keys1.length) {
      val k = (keys1).apply(i)
      if ((counts1).apply(k) == max1) {
        modes1 = modes1 :+ k
      }
      i += 1
    }
    println(modes1.toString)
    var arr2 = scala.collection.mutable.ArrayBuffer(2, 7, 1, 8, 2, 8)
    var counts2: Map[Int, Int] = scala.collection.mutable.Map()
    var keys2: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    i = 0
    while (i < arr2.length) {
      val v = (arr2).apply(i)
      if (counts2.contains(v)) {
        counts2(v) = (counts2).apply(v) + 1
      } else {
        counts2(v) = 1
        keys2 = keys2 :+ v
      }
      i += 1
    }
    var max2 = 0
    i = 0
    while (i < keys2.length) {
      val k = (keys2).apply(i)
      val c = (counts2).apply(k)
      if (c > max2) {
        max2 = c
      }
      i += 1
    }
    var modes2: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    i = 0
    while (i < keys2.length) {
      val k = (keys2).apply(i)
      if ((counts2).apply(k) == max2) {
        modes2 = modes2 :+ k
      }
      i += 1
    }
    println(modes2.toString)
  }
}
