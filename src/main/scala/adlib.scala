import net.sf.extjwnl.data.{POS, PointerUtils}
import net.sf.extjwnl.dictionary.Dictionary

import scala.util.Random
import scala.util.matching.Regex

/**
 * Object representation for a word replacement symbol
 * Word replacement symbols can be:
 *  - A part of speech (i.e. `<noun>`)
 *  - A POS and a category (i.e. `<noun-animal>`)
 *  - A POS and a unique numeric ID (i.e. `<noun1>`)
 *  - All three (i.e. `<noun-animal1>`)
 *  
 *  @param id A unique ID for this symbol. This allows re-using the same replacement word multiple times in the text
 *  @param pos Part of Speech
 *  @param category An optional sub-category to search within the part of speech
 */
case class WordReplacementSymbol(id: String, pos: POS, category: Option[String])

object WordReplacementSymbol {
  /**
   * Matching groups:
   *  1: part of speech
   *  2: category (optional)
   *  3: id (optional)
   */
  val regex = "<(noun|verb|adjective|adverb)(?:-([A-Za-z]+))?(\\d?)>".r
  
  private var increment = 0
  
  implicit def fromMatch(m: Regex.Match): WordReplacementSymbol = {
    val id = Option(m.group(3)).getOrElse {
      increment += 1
      "anonymousSymbol" + increment
    }

    WordReplacementSymbol(
      id,
      POS.getPOSForLabel(m.group(1)),
      Option(m.group(2))
    )
  }
}

/**
 * Used to parse a string for word replacement symbols and fill in random words
 * 
 * @param dict The WordNet dictionary to use for word replacements
 */
class AdLibParser(val dict: Dictionary = Dictionary.getDefaultResourceInstance) {
  val debug = true
  
  /**
   * Given a word replacement symbol, generates a matching random word
   */
  def getReplacementForSymbol(symbol: WordReplacementSymbol): String = {
    symbol.category match {
      // If no category is defined, draw from all words with the given part-of-speech
      case None => dict.getRandomIndexWord(symbol.pos).getLemma
      // If a category is defined, draw from a list of hyponyms for the given category
      case Some(category) => {
        val hypernym = dict.getIndexWord(symbol.pos, category)
        if (debug) println(s" Finding hyponym for ${hypernym.getLemma}")
        val hyponymTree = PointerUtils.getHyponymTree(hypernym.getSenses.get(0)).toList
        val hyponymBranch = hyponymTree.get(Random.nextInt(hyponymTree.size))
        if (debug) hyponymBranch.print()
        val synonyms = hyponymBranch.getLast.getSynset.getWords
        synonyms.get(Random.nextInt(synonyms.size)).getLemma
      }
    }
  }
  
  /**
   * Parse an input string for replacement symbols, and replace them with random strings
   */
  def parseText(inputText: String): String = {
    WordReplacementSymbol.regex.replaceAllIn(inputText, getReplacementForSymbol(_))
  }
}

object AdLib extends App {
  val testString = "In a <noun-place> in the <noun-place> there lived a <noun-creature>."
  val parser = new AdLibParser
  println(parser.parseText(testString))
}
