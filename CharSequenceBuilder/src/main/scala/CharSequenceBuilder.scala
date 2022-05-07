object CharSequenceBuilder {
  // val charset = "abcdefghijklmnopqrstuvwxyz"
  val charset = "abcd"

  /**
   * This acts as a wrapper for the recursive fn, passing default args to it
   * It also allows us to call the object as if it were a function
   */
  def apply(len: Int): Iterable[String] = {
    build(len, "")
  }

  /**
   * This function does the following:
   *  - Loop over all characters in the charset
   *  - Use the character as the first character in the string
   *  - Recursively call itself with length - 1, to get all possible suffixes for the string
   *  - Flatten the resulting lists of strings into a single list
   */
  private def build(len: Int, prefix: String): Iterable[String] = {
    if (len == 1) {
      // Innermost recursive call: append all possible characters to the given prefix
      charset.map(prefix + _)
    } else {
      // Main recursive logic: using the prefix from the previous iteration (if any), generate a new collection of
      //  possible prefixes by appending all characters from the charset; then recursively generate all possible
      //  suffixes
      charset.flatMap { c => build(len - 1, prefix + c) }
    }
  }
}

object PrintChars {
  def main(args: Array[String]): Unit = {
    val strings = CharSequenceBuilder(3)
    println(strings)
  }
}
