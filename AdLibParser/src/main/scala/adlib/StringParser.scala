package adlib

import scala.util.Try

object StringParser {
  def parseInt(string: String) = Try(string.toInt).toOption
}
