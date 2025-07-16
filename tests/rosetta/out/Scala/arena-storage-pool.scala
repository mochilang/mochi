object arena_storage_pool {
  def poolPut(p: List[Int], x: Int): List[Int] = p :+ x
  
  def poolGet(p: List[Int]): Map[String, any] = {
    if (p.length == 0) {
      println("pool empty")
      return Map("pool" -> p, "val" -> 0)
    }
    val idx = p.length - 1
    val v = (p).apply(idx)
    p = p.slice(0, idx)
    return Map("pool" -> p, "val" -> v)
  }
  
  def clearPool(p: List[Int]): List[Int] = List()
  
  def main() = {
    var pool: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 1
    var j = 2
    println(i + j.toString)
    pool = poolPut(pool, i)
    pool = poolPut(pool, j)
    i = 0
    j = 0
    val res1 = poolGet(pool)
    pool = (res1).apply("pool").asInstanceOf[List[Int]]
    i = (res1).apply("val").toInt
    val res2 = poolGet(pool)
    pool = (res2).apply("pool").asInstanceOf[List[Int]]
    j = (res2).apply("val").toInt
    i = 4
    j = 5
    println(i + j.toString)
    pool = poolPut(pool, i)
    pool = poolPut(pool, j)
    i = 0
    j = 0
    pool = clearPool(pool)
    val res3 = poolGet(pool)
    pool = (res3).apply("pool").asInstanceOf[List[Int]]
    i = (res3).apply("val").toInt
    val res4 = poolGet(pool)
    pool = (res4).apply("pool").asInstanceOf[List[Int]]
    j = (res4).apply("val").toInt
    i = 7
    j = 8
    println(i + j.toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
