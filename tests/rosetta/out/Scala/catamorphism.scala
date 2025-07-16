object catamorphism {
  def add(a: Int, b: Int): Int = a + b
  
  def sub(a: Int, b: Int): Int = a - b
  
  def mul(a: Int, b: Int): Int = a * b
  
  def fold(f: (Int, Int) => Int, xs: List[Int]): Int = {
    var r = (xs).apply(0)
    var i = 1
    while (i < xs.length) {
      r = f(r, (xs).apply(i))
      i += 1
    }
    return r
  }
  
  def main(args: Array[String]): Unit = {
    val n = List(1, 2, 3, 4, 5)
    println(fold((a: Int, b: Int) => add(a, b), n))
    println(fold((a: Int, b: Int) => sub(a, b), n))
    println(fold((a: Int, b: Int) => mul(a, b), n))
  }
}
