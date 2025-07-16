object bin_given_limits {
  def getBins(limits: List[Int], data: List[Int]): List[Int] = {
    var n = limits.length
    var bins: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < n + 1) {
      bins = bins :+ 0
      i += 1
    }
    var j = 0
    while (j < data.length) {
      val d = (data).apply(j)
      var index = 0
      while (index < limits.length) {
        if (d < (limits).apply(index)) {
          return
        }
        if (d == (limits).apply(index)) {
          index += 1
          return
        }
        index += 1
      }
      bins(index) = (bins).apply(index) + 1
      j += 1
    }
    return bins
  }
  
  def padLeft(n: Int, width: Int): String = {
    var s = n.toString
    var pad = width - s.length
    var out = ""
    var i = 0
    while (i < pad) {
      out += " "
      i += 1
    }
    return out + s
  }
  
  def printBins(limits: List[Int], bins: List[Int]) = {
    var n = limits.length
    println("           < " + padLeft((limits).apply(0), 3) + " = " + padLeft((bins).apply(0), 2))
    var i = 1
    while (i < n) {
      println(">= " + padLeft((limits).apply(i - 1), 3) + " and < " + padLeft((limits).apply(i), 3) + " = " + padLeft((bins).apply(i), 2))
      i += 1
    }
    println(">= " + padLeft((limits).apply(n - 1), 3) + "           = " + padLeft((bins).apply(n), 2))
    println("")
  }
  
  def main() = {
    val limitsList = List(List(23, 37, 43, 53, 67, 83), List(14, 18, 249, 312, 389, 392, 513, 591, 634, 720))
    val dataList = List(List(95, 21, 94, 12, 99, 4, 70, 75, 83, 93, 52, 80, 57, 5, 53, 86, 65, 17, 92, 83, 71, 61, 54, 58, 47, 16, 8, 9, 32, 84, 7, 87, 46, 19, 30, 37, 96, 6, 98, 40, 79, 97, 45, 64, 60, 29, 49, 36, 43, 55), List(445, 814, 519, 697, 700, 130, 255, 889, 481, 122, 932, 77, 323, 525, 570, 219, 367, 523, 442, 933, 416, 589, 930, 373, 202, 253, 775, 47, 731, 685, 293, 126, 133, 450, 545, 100, 741, 583, 763, 306, 655, 267, 248, 477, 549, 238, 62, 678, 98, 534, 622, 907, 406, 714, 184, 391, 913, 42, 560, 247, 346, 860, 56, 138, 546, 38, 985, 948, 58, 213, 799, 319, 390, 634, 458, 945, 733, 507, 916, 123, 345, 110, 720, 917, 313, 845, 426, 9, 457, 628, 410, 723, 354, 895, 881, 953, 677, 137, 397, 97, 854, 740, 83, 216, 421, 94, 517, 479, 292, 963, 376, 981, 480, 39, 257, 272, 157, 5, 316, 395, 787, 942, 456, 242, 759, 898, 576, 67, 298, 425, 894, 435, 831, 241, 989, 614, 987, 770, 384, 692, 698, 765, 331, 487, 251, 600, 879, 342, 982, 527, 736, 795, 585, 40, 54, 901, 408, 359, 577, 237, 605, 847, 353, 968, 832, 205, 838, 427, 876, 959, 686, 646, 835, 127, 621, 892, 443, 198, 988, 791, 466, 23, 707, 467, 33, 670, 921, 180, 991, 396, 160, 436, 717, 918, 8, 374, 101, 684, 727, 749))
    var i = 0
    while (i < limitsList.length) {
      println("Example " + i + 1.toString + "\n")
      val bins = getBins((limitsList).apply(i), (dataList).apply(i))
      printBins((limitsList).apply(i), bins)
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
