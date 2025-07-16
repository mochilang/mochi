case class Foobar(var Exported: Int, var unexported: Int)

object break_oo_privacy {
  def examineAndModify(f: Foobar): Foobar = {
    println(" v: {" + f.Exported.toString + " " + f.unexported.toString + "} = {" + f.Exported.toString + " " + f.unexported.toString + "}")
    println("    Idx Name       Type CanSet")
    println("     0: Exported   int  true")
    println("     1: unexported int  false")
    f.Exported = 16
    f.unexported = 44
    println("  modified unexported field via unsafe")
    return f
  }
  
  def anotherExample() = {
    println("bufio.ReadByte returned error: unsafely injected error value into bufio inner workings")
  }
  
  def main(args: Array[String]): Unit = {
    var obj = Foobar(Exported = 12, unexported = 42)
    println("obj: {" + obj.Exported.toString + " " + obj.unexported.toString + "}")
    obj = examineAndModify(obj)
    println("obj: {" + obj.Exported.toString + " " + obj.unexported.toString + "}")
    anotherExample()
  }
}
