package adlib

import net.sf.extjwnl.data.POS
import java.util.UUID
import scala.util.matching.Regex

/**
 * Object representation for a word replacement symbol
 * Word replacement symbols can be:
 *  - A part of speech (i.e. `<noun>`)
 *  - A POS and a category (i.e. `<noun-animal>`)
 *  - A POS and multiple categories (i.e. `<noun-creature-animal>`)
 *  - A POS and a unique numeric ID (i.e. `<noun1>`)
 *  - Any combination of the above (i.e. `<noun-animal1>`)
 *
 *  @param id A unique ID for this symbol. This allows re-using the same replacement word multiple times in the text
 *  @param pos Part of Speech
 *  @param category An optional category to search for a matching word.
 *  @param sense An optional sense to search within the category. The defines which sense of the category word to use
 */
case class WordReplacementSymbol(id: Option[Int], pos: POS, category: Option[String], sense: Option[String]) {
  /**
   * A unique identifier for this replacement symbol. If an ID is defined, it is used to generate a reusable slug,
   * otherwise a random UUID is used.
   */
  val slug = id.fold(UUID.randomUUID.toString) { _ =>
    List(Some(pos.getLabel), category, sense, id).flatten.mkString("/")
  }
}

object WordReplacementSymbol {
  private val WordTagRegex: Regex = "<(noun|verb|adjective|adverb)(?:-([A-Za-z]+))?(?:-([A-Za-z]+))?(\\d?)>".r

  implicit def fromRegexMatch(m: Regex.Match): WordReplacementSymbol = WordReplacementSymbol(
    id = Option(m.group(4)).flatMap(StringParser.parseInt),
    pos = POS.getPOSForLabel(m.group(1)),
    category = Option(m.group(2)),
    sense = Option(m.group(3)),
  )
  
  def replaceAllIn(text: String, replacer: WordReplacementSymbol => String): String = {
    WordTagRegex.replaceAllIn(text, replacer(_))
  }
}
