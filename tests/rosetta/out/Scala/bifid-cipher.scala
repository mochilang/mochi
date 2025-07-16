object bifid_cipher {
  def square_to_maps(square: List[List[String]]): Map[String, any] = {
    var emap: Map[String, List[Int]] = scala.collection.mutable.Map()
    var dmap: Map[String, String] = scala.collection.mutable.Map()
    var x = 0
    while (x < square.length) {
      val row = (square).apply(x)
      var y = 0
      while (y < row.length) {
        val ch = (row).apply(y)
        emap(ch) = List(x, y)
        dmap(x.toString + "," + y.toString) = ch
        y += 1
      }
      x += 1
    }
    return Map("e" -> emap, "d" -> dmap)
  }
  
  def remove_space(text: String, emap: Map[String, List[Int]]): String = {
    val s = upper(text)
    var out = ""
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (emap.contains(ch != " " && ch)) {
        out += ch
      }
      i += 1
    }
    return out
  }
  
  def encrypt(text: String, emap: Map[String, List[Int]], dmap: Map[String, String]): String = {
    text = remove_space(text, emap)
    var row0: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var row1: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < text.length) {
      val ch = text.substring(i, i + 1)
      val xy = (emap).apply(ch)
      row0 = row0 :+ (xy).apply(0)
      row1 = row1 :+ (xy).apply(1)
      i += 1
    }
    for(v <- row1) {
      row0 = row0 :+ v
    }
    var res = ""
    var j = 0
    while (j < row0.length) {
      val key = (row0).apply(j).toString + "," + (row0).apply(j + 1).toString
      res += (dmap).apply(key)
      j += 2
    }
    return res
  }
  
  def decrypt(text: String, emap: Map[String, List[Int]], dmap: Map[String, String]): String = {
    text = remove_space(text, emap)
    var coords: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < text.length) {
      val ch = text.substring(i, i + 1)
      val xy = (emap).apply(ch)
      coords = coords :+ (xy).apply(0)
      coords = coords :+ (xy).apply(1)
      i += 1
    }
    var half = coords.length / 2
    var k1: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var k2: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var idx = 0
    while (idx < half) {
      k1 = k1 :+ (coords).apply(idx)
      idx += 1
    }
    while (idx < coords.length) {
      k2 = k2 :+ (coords).apply(idx)
      idx += 1
    }
    var res = ""
    var j = 0
    while (j < half) {
      val key = (k1).apply(j).toString + "," + (k2).apply(j).toString
      res += (dmap).apply(key)
      j += 1
    }
    return res
  }
  
  def main() = {
    val squareRosetta = List(List("A", "B", "C", "D", "E"), List("F", "G", "H", "I", "K"), List("L", "M", "N", "O", "P"), List("Q", "R", "S", "T", "U"), List("V", "W", "X", "Y", "Z"), List("J", "1", "2", "3", "4"))
    val squareWikipedia = List(List("B", "G", "W", "K", "Z"), List("Q", "P", "N", "D", "S"), List("I", "O", "A", "X", "E"), List("F", "C", "L", "U", "M"), List("T", "H", "Y", "V", "R"), List("J", "1", "2", "3", "4"))
    val textRosetta = "0ATTACKATDAWN"
    val textWikipedia = "FLEEATONCE"
    val textTest = "The invasion will start on the first of January"
    var maps = square_to_maps(squareRosetta)
    var emap = (maps).apply("e")
    var dmap = (maps).apply("d")
    println("from Rosettacode")
    println("original:\t " + textRosetta)
    var s = encrypt(textRosetta, emap, dmap)
    println("codiert:\t " + s)
    s = decrypt(s, emap, dmap)
    println("and back:\t " + s)
    maps = square_to_maps(squareWikipedia)
    emap = (maps).apply("e")
    dmap = (maps).apply("d")
    println("from Wikipedia")
    println("original:\t " + textWikipedia)
    s = encrypt(textWikipedia, emap, dmap)
    println("codiert:\t " + s)
    s = decrypt(s, emap, dmap)
    println("and back:\t " + s)
    maps = square_to_maps(squareWikipedia)
    emap = (maps).apply("e")
    dmap = (maps).apply("d")
    println("from Rosettacode long part")
    println("original:\t " + textTest)
    s = encrypt(textTest, emap, dmap)
    println("codiert:\t " + s)
    s = decrypt(s, emap, dmap)
    println("and back:\t " + s)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
