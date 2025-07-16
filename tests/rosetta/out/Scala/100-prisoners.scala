object _100_prisoners {
  def shuffle(xs: List[Int]): List[Int] = {
    var arr = xs
    var i = 99
    while (i > 0) {
      val j = now() % (i + 1)
      val tmp = (arr).apply(i)
      arr(i) = (arr).apply(j)
      arr(j) = tmp
      i -= 1
    }
    return arr
  }
  
  def doTrials(trials: Int, np: Int, strategy: String) = {
    var pardoned = 0
    var t = 0
    while (t < trials) {
      var drawers: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
      var i = 0
      while (i < 100) {
        drawers = drawers :+ i
        i += 1
      }
      drawers = shuffle(drawers)
      var p = 0
      var success = true
      while (p < np) {
        var found = false
        if (strategy == "optimal") {
          var prev = p
          var d = 0
          while (d < 50) {
            val this = (drawers).apply(prev)
            if (this == p) {
              found = true
              return
            }
            prev = this
            d += 1
          }
        } else {
          var opened: List[Boolean] = scala.collection.mutable.ArrayBuffer[Any]()
          var k = 0
          while (k < 100) {
            opened = opened :+ false
            k += 1
          }
          var d = 0
          while (d < 50) {
            var n = now() % 100
            while ((opened).apply(n)) {
              n = now() % 100
            }
            opened(n) = true
            if ((drawers).apply(n) == p) {
              found = true
              return
            }
            d += 1
          }
        }
        if (!found) {
          success = false
          return
        }
        p += 1
      }
      if (success) {
        pardoned += 1
      }
      t += 1
    }
    val rf = (pardoned.toDouble) / (trials.toDouble) * 100
    println("  strategy = " + strategy + "  pardoned = " + pardoned.toString + " relative frequency = " + rf.toString + "%")
  }
  
  def main() = {
    val trials = 1000
    for(np <- List(10, 100)) {
      println("Results from " + trials.toString + " trials with " + np.toString + " prisoners:\n")
      for(strat <- List("random", "optimal")) {
        doTrials(trials, np, strat)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
