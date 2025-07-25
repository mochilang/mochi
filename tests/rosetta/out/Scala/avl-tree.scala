object avl_tree {
  def Node(data: Int): Map[String, any] = Map("Data" -> data, "Balance" -> 0, "Link" -> List(null, null))
  
  def getLink(n: Map[String, any], dir: Int): any = (((n).apply("Link").asInstanceOf[List[any]])).apply(dir)
  
  def setLink(n: Map[String, any], dir: Int, v: any) = {
    var links = (n).apply("Link").asInstanceOf[List[any]]
    links(dir) = v
    n("Link") = links
  }
  
  def opp(dir: Int): Int = 1 - dir
  
  def single(root: Map[String, any], dir: Int): Map[String, any] = {
    var tmp = getLink(root, opp(dir))
    setLink(root, opp(dir), getLink(tmp, dir))
    setLink(tmp, dir, root)
    return tmp
  }
  
  def double(root: Map[String, any], dir: Int): Map[String, any] = {
    var tmp = getLink(getLink(root, opp(dir)), dir)
    setLink(getLink(root, opp(dir)), dir, getLink(tmp, opp(dir)))
    setLink(tmp, opp(dir), getLink(root, opp(dir)))
    setLink(root, opp(dir), tmp)
    tmp = getLink(root, opp(dir))
    setLink(root, opp(dir), getLink(tmp, dir))
    setLink(tmp, dir, root)
    return tmp
  }
  
  def adjustBalance(root: Map[String, any], dir: Int, bal: Int) = {
    var n = getLink(root, dir).asInstanceOf[Map[String, any]]
    var nn = getLink(n, opp(dir)).asInstanceOf[Map[String, any]]
    if (((nn).apply("Balance")).asInstanceOf[Int] == 0) {
      root("Balance") = 0
      n("Balance") = 0
    } else {
      if (((nn).apply("Balance")).asInstanceOf[Int] == bal) {
        root("Balance") = -bal
        n("Balance") = 0
      } else {
        root("Balance") = 0
        n("Balance") = bal
      }
    }
    nn("Balance") = 0
  }
  
  def insertBalance(root: Map[String, any], dir: Int): Map[String, any] = {
    var n = getLink(root, dir).asInstanceOf[Map[String, any]]
    var bal = 2 * dir - 1
    if (((n).apply("Balance")).asInstanceOf[Int] == bal) {
      root("Balance") = 0
      n("Balance") = 0
      return single(root, opp(dir))
    }
    adjustBalance(root, dir, bal)
    return double(root, opp(dir))
  }
  
  def insertR(root: any, data: Int): Map[String, any] = {
    if ((root).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
      return Map("node" -> Node(data), "done" -> false)
    }
    var node = root.asInstanceOf[Map[String, any]]
    var dir = 0
    if (((node).apply("Data").toInt) < data) {
      dir = 1
    }
    var r = insertR(getLink(node, dir), data)
    setLink(node, dir, (r).apply("node"))
    if ((r).apply("done") != null) {
      return Map("node" -> node, "done" -> true)
    }
    node("Balance") = ((node).apply("Balance").toInt) + (2 * dir - 1)
    if (((node).apply("Balance")).asInstanceOf[Int] == 0) {
      return Map("node" -> node, "done" -> true)
    }
    if (((node).apply("Balance")).asInstanceOf[Int] == 1 || ((node).apply("Balance")).asInstanceOf[Int] == (-1)) {
      return Map("node" -> node, "done" -> false)
    }
    return Map("node" -> insertBalance(node, dir), "done" -> true)
  }
  
  def Insert(tree: any, data: Int): any = {
    val r = insertR(tree, data)
    return (r).apply("node")
  }
  
  def removeBalance(root: Map[String, any], dir: Int): Map[String, any] = {
    var n = getLink(root, opp(dir)).asInstanceOf[Map[String, any]]
    var bal = 2 * dir - 1
    if (((n).apply("Balance")).asInstanceOf[Int] == (-bal)) {
      root("Balance") = 0
      n("Balance") = 0
      return Map("node" -> single(root, dir), "done" -> false)
    }
    if (((n).apply("Balance")).asInstanceOf[Int] == bal) {
      adjustBalance(root, opp(dir), (-bal))
      return Map("node" -> double(root, dir), "done" -> false)
    }
    root("Balance") = -bal
    n("Balance") = bal
    return Map("node" -> single(root, dir), "done" -> true)
  }
  
  def removeR(root: any, data: Int): Map[String, any] = {
    if ((root).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
      return Map("node" -> null, "done" -> false)
    }
    var node = root.asInstanceOf[Map[String, any]]
    if (((node).apply("Data").toInt) == data) {
      if ((getLink(node, 0)).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
        return Map("node" -> getLink(node, 1), "done" -> false)
      }
      if ((getLink(node, 1)).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
        return Map("node" -> getLink(node, 0), "done" -> false)
      }
      var heir = getLink(node, 0)
      while ((getLink(heir, 1)).asInstanceOf[Int] != (null).asInstanceOf[Int]) {
        heir = getLink(heir, 1)
      }
      node("Data") = (heir).apply("Data")
      data = (heir).apply("Data").toInt
    }
    var dir = 0
    if (((node).apply("Data").toInt) < data) {
      dir = 1
    }
    var r = removeR(getLink(node, dir), data)
    setLink(node, dir, (r).apply("node"))
    if ((r).apply("done") != null) {
      return Map("node" -> node, "done" -> true)
    }
    node("Balance") = ((node).apply("Balance").toInt) + 1 - 2 * dir
    if (((node).apply("Balance")).asInstanceOf[Int] == 1 || ((node).apply("Balance")).asInstanceOf[Int] == (-1)) {
      return Map("node" -> node, "done" -> true)
    }
    if (((node).apply("Balance")).asInstanceOf[Int] == 0) {
      return Map("node" -> node, "done" -> false)
    }
    return removeBalance(node, dir)
  }
  
  def Remove(tree: any, data: Int): any = {
    val r = removeR(tree, data)
    return (r).apply("node")
  }
  
  def indentStr(n: Int): String = {
    var s = ""
    var i = 0
    while (i < n) {
      s += " "
      i += 1
    }
    return s
  }
  
  def dumpNode(node: any, indent: Int, comma: Boolean) = {
    val sp = indentStr(indent)
    if ((node).asInstanceOf[Int] == (null).asInstanceOf[Int]) {
      var line = sp + "null"
      if (comma) {
        line += ","
      }
      println(line)
    } else {
      println(sp + "{")
      println(indentStr(indent + 3) + "\"Data\": " + (node).apply("Data").toString + ",")
      println(indentStr(indent + 3) + "\"Balance\": " + (node).apply("Balance").toString + ",")
      println(indentStr(indent + 3) + "\"Link\": [")
      dumpNode(getLink(node, 0), indent + 6, true)
      dumpNode(getLink(node, 1), indent + 6, false)
      println(indentStr(indent + 3) + "]")
      var end = sp + "}"
      if (comma) {
        end += ","
      }
      println(end)
    }
  }
  
  def dump(node: any, indent: Int) = {
    dumpNode(node, indent, false)
  }
  
  def main() = {
    var tree = null
    println("Empty tree:")
    dump(tree, 0)
    println("")
    println("Insert test:")
    tree = Insert(tree, 3)
    tree = Insert(tree, 1)
    tree = Insert(tree, 4)
    tree = Insert(tree, 1)
    tree = Insert(tree, 5)
    dump(tree, 0)
    println("")
    println("Remove test:")
    tree = Remove(tree, 3)
    tree = Remove(tree, 1)
    var t = tree.asInstanceOf[Map[String, any]]
    t("Balance") = 0
    tree = t
    dump(tree, 0)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
