import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.BatchSourceFile
import scala.util.parsing.json.JSONObject
import scala.collection.mutable.ListBuffer

object ParserJson {
  def main(args: Array[String]): Unit = {
    val file = args(0)
    val code = scala.io.Source.fromFile(file).mkString
    val settings = new Settings
    settings.usejavacp.value = true
    val reporter = new StoreReporter
    val global = new Global(settings, reporter)
    val run = new global.Run
    val unit = new global.CompilationUnit(new BatchSourceFile(file, code))
    run.units += unit
    run.compileUnits(run.units, run.parserPhase)
    val trees = ListBuffer[JSONObject]()
    unit.body.children.foreach { t =>
      trees += toJson(t)(global)
    }
    val obj = JSONObject(Map("stats" -> trees.toList))
    println(obj.toString())
  }

  def toJson(t: global.Tree)(implicit g: Global): JSONObject = {
    import g._
    t match {
      case d: DefDef =>
        val params = d.vparamss.flatten.map { p =>
          JSONObject(Map("name" -> p.name.toString))
        }
        JSONObject(Map(
          "kind" -> "def",
          "name" -> d.name.toString,
          "params" -> params,
          "ret" -> d.tpt.toString,
          "body" -> d.rhs.toString
        ))
      case v: ValDef =>
        JSONObject(Map(
          "kind" -> "val",
          "name" -> v.name.toString,
          "ret" -> v.tpt.toString,
          "rhs" -> v.rhs.toString
        ))
      case c: ClassDef =>
        JSONObject(Map(
          "kind" -> "class",
          "name" -> c.name.toString
        ))
      case m: ModuleDef =>
        JSONObject(Map(
          "kind" -> "object",
          "name" -> m.name.toString
        ))
      case other =>
        JSONObject(Map("kind" -> "unknown", "syntax" -> other.toString))
    }
  }
}
