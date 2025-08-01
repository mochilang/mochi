object atomic_updates {
  def randOrder(seed: Int, n: Int): List[Int] = {
    val next = (seed * 1664525 + 1013904223) % 2147483647
    return List(next, next % n)
  }
  
  def randChaos(seed: Int, n: Int): List[Int] = {
    val next = (seed * 1103515245 + 12345) % 2147483647
    return List(next, next % n)
  }
  
  def main() = {
    val nBuckets = 10
    val initialSum = 1000
    var buckets: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    for(i <- 0 until nBuckets) {
      buckets = buckets :+ 0
    }
    var i = nBuckets
    var dist = initialSum
    while (i > 0) {
      val v = dist / i
      i -= 1
      buckets(i) = v
      dist -= v
    }
    var tc0 = 0
    var tc1 = 0
    var total = 0
    var nTicks = 0
    var seedOrder = 1
    var seedChaos = 2
    println("sum  ---updates---    mean  buckets")
    var t = 0
    while (t < 5) {
      var r = randOrder(seedOrder, nBuckets)
      seedOrder = (r).apply(0)
      var b1 = (r).apply(1)
      var b2 = (b1 + 1) % nBuckets
      val v1 = (buckets).apply(b1)
      val v2 = (buckets).apply(b2)
      if (v1 > v2) {
        var a = ((v1 - v2) / 2).toInt
        if (a > (buckets).apply(b1)) {
          a = (buckets).apply(b1)
        }
        buckets(b1) = (buckets).apply(b1) - a
        buckets(b2) = (buckets).apply(b2) + a
      } else {
        var a = ((v2 - v1) / 2).toInt
        if (a > (buckets).apply(b2)) {
          a = (buckets).apply(b2)
        }
        buckets(b2) = (buckets).apply(b2) - a
        buckets(b1) = (buckets).apply(b1) + a
      }
      tc0 += 1
      r = randChaos(seedChaos, nBuckets)
      seedChaos = (r).apply(0)
      b1 = (r).apply(1)
      b2 = (b1 + 1) % nBuckets
      r = randChaos(seedChaos, (buckets).apply(b1) + 1)
      seedChaos = (r).apply(0)
      var amt = (r).apply(1)
      if (amt > (buckets).apply(b1)) {
        amt = (buckets).apply(b1)
      }
      buckets(b1) = (buckets).apply(b1) - amt
      buckets(b2) = (buckets).apply(b2) + amt
      tc1 += 1
      var sum = 0
      var idx = 0
      while (idx < nBuckets) {
        sum += (buckets).apply(idx)
        idx += 1
      }
      total = total + tc0 + tc1
      nTicks += 1
      println(sum.toString + " " + tc0.toString + " " + tc1.toString + " " + total / nTicks.toString + "  " + buckets.toString)
      tc0 = 0
      tc1 = 0
      t += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
