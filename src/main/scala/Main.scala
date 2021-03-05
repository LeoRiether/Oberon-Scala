import java.io.File
import java.nio.file.{Files, Paths}

import br.unb.cic.oberon.ast._
import br.unb.cic.oberon.parser._

object Main extends App {

  // https://stackoverflow.com/a/55032051
  def prettyPrint(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {

    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match { case _: Iterable[Any] => "" case obj: Product => obj.productPrefix case _ => obj.toString }

    println(s"$indent$prettyName$ptype")

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(prettyPrint(_, depth + 1))
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => prettyPrint(subObj, depth + 1, Some(paramName)) }
      case _ =>
    }
  }

  val module = ScalaParser.parseResource("test.oberon")
  prettyPrint(module)
}