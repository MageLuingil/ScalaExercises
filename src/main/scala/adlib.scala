import net.sf.extjwnl.data.{POS, PointerUtils}
import net.sf.extjwnl.dictionary.Dictionary

import scala.collection.mutable.Map
import scala.language.postfixOps
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
    val idString = m.group(3)
    val pos = POS.getPOSForLabel(m.group(1))
    val category = Option(m.group(2))
    
    // Make sure ID will be unique for all possible matching symbols
    val id = if (idString == null || idString.isEmpty) {
      increment += 1
      s"anonymousSymbol$increment"
    } else {
      val categoryString = category match {
        case Some(s) => s"-$s"
        case None => ""
      }
      s"${pos.getLabel}${categoryString}${idString}"
    }
    
    WordReplacementSymbol(id, pos, category)
  }
}

/**
 * Used to parse a string for word replacement symbols and fill in random words
 * 
 * @param dict The WordNet dictionary to use for word replacements
 */
class AdLibParser(val dict: Dictionary = Dictionary.getDefaultResourceInstance) {
  val debug = true
  val symbolMap: Map[String, String] = Map()
  
  /**
   * Given a word replacement symbol, generates a matching random word
   */
  def getReplacementForSymbol(symbol: WordReplacementSymbol): String = {
    val replacement = symbol match {
      // If the symbol already exists in our symbol map, re-use the existing replacement
      case s if symbolMap.contains(s.id) => symbolMap.get(s.id).get
      // If no category is defined, draw from all words with the given part-of-speech
      case WordReplacementSymbol(_, _, None) => dict.getRandomIndexWord(symbol.pos).getLemma
      // If a category is defined, draw from a list of hyponyms for the given category
      case WordReplacementSymbol(_, _, Some(category)) => {
        val hypernym = dict.getIndexWord(symbol.pos, category)
        if (debug) println(s" Finding hyponym for ${hypernym.getLemma}")
        val hyponymTree = PointerUtils.getHyponymTree(hypernym.getSenses.get(0)).toList
        val hyponymBranch = hyponymTree.get(Random.nextInt(hyponymTree.size))
        if (debug) hyponymBranch.print()
        val synonyms = hyponymBranch.getLast.getSynset.getWords
        synonyms.get(Random.nextInt(synonyms.size)).getLemma
      }
    }
    symbolMap += (symbol.id -> replacement)
    replacement
  }
  
  /**
   * Parse an input string for replacement symbols, and replace them with random strings
   */
  def parseText(inputText: String): String = {
    WordReplacementSymbol.regex.replaceAllIn(inputText, getReplacementForSymbol(_))
  }
}

object AdLib extends App {
  val testString = "In a <noun-object1> in the <noun-object> there lived a <noun-creature1>. Not a <adjective>, " +
    "<adjective>, <adjective> <noun-object1>, filled with the ends of <noun-creature>s and a <adjective> smell, nor " +
    "yet a <adjective>, <adjective>, <adjective> <noun-object1> with nothing in it to <verb> on or to <verb>; " +
    "it was a <noun-creature1>-<noun-object1>, and that means <noun-attribute>."
  val parser = new AdLibParser
  println(parser.parseText(testString))
}
