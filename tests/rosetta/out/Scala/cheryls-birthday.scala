case class Birthday(var month: Int, var day: Int)

object cheryls_birthday {
  def monthUnique(b: Birthday, list: List[Birthday]): Boolean = {
    var c = 0
    for(x <- list) {
      if (x.month == b.month) {
        c += 1
      }
    }
    return c == 1
  }
  
  def dayUnique(b: Birthday, list: List[Birthday]): Boolean = {
    var c = 0
    for(x <- list) {
      if (x.day == b.day) {
        c += 1
      }
    }
    return c == 1
  }
  
  def monthWithUniqueDay(b: Birthday, list: List[Birthday]): Boolean = {
    for(x <- list) {
      if (x.month == b.month && dayUnique(x, list)) {
        return true
      }
    }
    return false
  }
  
  def bstr(b: Birthday): String = {
    val months = List("", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    return (months).apply(b.month) + " " + b.day.toString
  }
  
  def main(args: Array[String]): Unit = {
    var choices: List[Birthday] = scala.collection.mutable.ArrayBuffer(Birthday(month = 5, day = 15), Birthday(month = 5, day = 16), Birthday(month = 5, day = 19), Birthday(month = 6, day = 17), Birthday(month = 6, day = 18), Birthday(month = 7, day = 14), Birthday(month = 7, day = 16), Birthday(month = 8, day = 14), Birthday(month = 8, day = 15), Birthday(month = 8, day = 17))
    var filtered: List[Birthday] = scala.collection.mutable.ArrayBuffer[Any]()
    for(bd <- choices) {
      if (!monthUnique(bd, choices)) {
        filtered = filtered :+ bd
      }
    }
    var filtered2: List[Birthday] = scala.collection.mutable.ArrayBuffer[Any]()
    for(bd <- filtered) {
      if (!monthWithUniqueDay(bd, filtered)) {
        filtered2 = filtered2 :+ bd
      }
    }
    var filtered3: List[Birthday] = scala.collection.mutable.ArrayBuffer[Any]()
    for(bd <- filtered2) {
      if (dayUnique(bd, filtered2)) {
        filtered3 = filtered3 :+ bd
      }
    }
    var filtered4: List[Birthday] = scala.collection.mutable.ArrayBuffer[Any]()
    for(bd <- filtered3) {
      if (monthUnique(bd, filtered3)) {
        filtered4 = filtered4 :+ bd
      }
    }
    if (filtered4.length == 1) {
      println("Cheryl's birthday is " + bstr((filtered4).apply(0)))
    } else {
      println("Something went wrong!")
    }
  }
}
