object bitcoin_address_validation {
  def indexOf(s: String, ch: String): Int = {
    var i = 0
    while (i < s.length) {
      if (s.substring(i, i + 1) == ch) {
        return i
      }
      i += 1
    }
    return -1
  }
  
  def set58(addr: String): List[Int] = {
    val tmpl = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    var a: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < 25) {
      a = a :+ 0
      i += 1
    }
    var idx = 0
    while (idx < addr.length) {
      val ch = addr.substring(idx, idx + 1)
      var c = indexOf(tmpl, ch)
      if (c < 0) {
        return List()
      }
      var j = 24
      while (j >= 0) {
        c = c + 58 * (a).apply(j)
        a(j) = c % 256
        c = (c / 256).toInt
        j -= 1
      }
      if (c > 0) {
        return List()
      }
      idx += 1
    }
    return a
  }
  
  def doubleSHA256(bs: List[Int]): List[Int] = {
    val first = sha256(bs)
    return sha256(first)
  }
  
  def computeChecksum(a: List[Int]): List[Int] = {
    val hash = doubleSHA256(a.slice(0, 21))
    return hash.slice(0, 4)
  }
  
  def validA58(addr: String): Boolean = {
    val a = set58(addr)
    if (a.length != 25) {
      return false
    }
    if ((a).apply(0) != 0) {
      return false
    }
    val sum = computeChecksum(a)
    var i = 0
    while (i < 4) {
      if ((a).apply(21 + i) != (sum).apply(i)) {
        return false
      }
      i += 1
    }
    return true
  }
  
  def main(args: Array[String]): Unit = {
    println(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i").toString)
    println(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j").toString)
  }
}
