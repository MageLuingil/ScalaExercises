import net.sf.extjwnl.data.{IndexWord, POS, PointerUtils, Word}
import net.sf.extjwnl.dictionary.Dictionary

import java.util.Random
import scala.util.matching.Regex

/**
 * A word replacement symbol looks like <noun-animal1>, where:
 *  "noun" is the part of speech
 *  "animal" is a category (optional)
 *  "1" is a unique ID, to allow reusing the same replacement word
 */
case class WordReplacementSymbol(id: String, pos: POS, category: Option[String])

object WordReplacementSymbol {
  /**
   * Matching groups:
   *  1: part of speech
   *  2: category (optional)
   *  3: id (optional)
   */
  val symbolRegex = "<(noun|verb|adjective|adverb)(?:-([A-Za-z]+))?(\\d?)>".r
  
  var increment = 0
  
  implicit def fromMatch(m: Regex.Match): WordReplacementSymbol = {
    val id = Option(m.group(3)).getOrElse({
      increment += 1
      "symbol" + increment
    })

    WordReplacementSymbol(
      id,
      POS.getPOSForLabel(m.group(1)),
      Option(m.group(2))
    )
  }
}

class AdLibParser(val dict: Dictionary = Dictionary.getDefaultResourceInstance) {
  val debug = true
  val random = new Random
  
  def getReplacementForSymbol(symbol: WordReplacementSymbol): String = {
    symbol.category match {
      // If no category is defined, draw from all words with the given part-of-speech
      case None => dict.getRandomIndexWord(symbol.pos).getLemma
      // If a category is defined, draw from a list of hyponyms for the given category
      case Some(category) => {
        val hypernym = dict.getIndexWord(symbol.pos, category)
        val hyponymTree = PointerUtils.getHyponymTree(hypernym.getSenses.get(0)).toList
        val hyponymBranch = hyponymTree.get(random.nextInt(hyponymTree.size))
        if (debug) hyponymBranch.print()
        val synonyms = hyponymBranch.getLast.getSynset.getWords
        synonyms.get(random.nextInt(synonyms.size)).getLemma
      }
    }
  }
  
  def parseText(inputText: String): String = {
    WordReplacementSymbol.symbolRegex.replaceAllIn(inputText, getReplacementForSymbol(_))
  }
}

object AdLib extends App {
  val testString = "In a <noun-object> in the <noun-object> there lived a <noun-creature>."
  val parser = new AdLibParser
  println(parser.parseText(testString))
}
