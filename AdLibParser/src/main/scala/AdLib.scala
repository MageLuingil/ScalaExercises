import adlib.AdLibParser
import scala.io.Source

object AdLib {
  def main(args: Array[String]): Unit = {
    val parser = new AdLibParser

    args.foreach { arg =>
      val sourceFile = Source.fromFile(arg)
      val inputText = sourceFile.mkString
      sourceFile.close
      
      println(parser.parseText(inputText))
    }
  }
}
