import net.sf.extjwnl.data.relationship.RelationshipFinder
import net.sf.extjwnl.data.{POS, PointerType, PointerUtils, Synset}
import net.sf.extjwnl.dictionary.Dictionary

import scala.collection.mutable.Map
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Random
import scala.util.matching.Regex

/**
 * Allow using the Java API with scala collections
 */
implicit def convertSynsetList(synset: java.util.List[Synset]): List[Synset] =
  synset.asScala.toList
implicit def convertWordList(words: java.util.List[net.sf.extjwnl.data.Word]): List[net.sf.extjwnl.data.Word] =
  words.asScala.toList
implicit def convertWordArrayList(words: java.util.ArrayList[net.sf.extjwnl.data.Word]): List[net.sf.extjwnl.data.Word] =
  words.asScala.toList
implicit def convertPointerTargetTreeToList(tree: net.sf.extjwnl.data.list.PointerTargetTree): List[net.sf.extjwnl.data.list.PointerTargetNodeList] =
  tree.toList.asScala.toList
implicit def convertRelationshipList(relationship: java.util.ArrayList[net.sf.extjwnl.data.relationship.Relationship]): List[net.sf.extjwnl.data.relationship.Relationship] =
  relationship.asScala.toList

/**
 * Object representation for a word replacement symbol
 * Word replacement symbols can be:
 *  - A part of speech (i.e. `<noun>`)
 *  - A POS and a category (i.e. `<noun-animal>`)
 *  - A POS and multiple categories (i.e. `<noun-creature-animal>`)
 *  - A POS and a unique numeric ID (i.e. `<noun1>`)
 *  - All three (i.e. `<noun-animal1>`)
 *  
 *  @param id A unique ID for this symbol. This allows re-using the same replacement word multiple times in the text
 *  @param pos Part of Speech
 *  @param category An optional tuple containing the category to search. The first word is the category, and the second
 *                  is an optional sense to define which definition of the category word to use.
 */
case class WordReplacementSymbol(id: String, pos: POS, category: (Option[String], Option[String]))

object WordReplacementSymbol {
  /**
   * Matching groups:
   *  1: part of speech
   *  2: category (optional)
   *  3: category sense (optional)
   *  4: id (optional)
   */
  val regex = "<(noun|verb|adjective|adverb)(?:-([A-Za-z]+))?(?:-([A-Za-z]+))?(\\d?)>".r
  
  private var increment = 0
  
  implicit def fromMatch(m: Regex.Match): WordReplacementSymbol = {
    val idString = m.group(4)
    val posString = m.group(1)
    val catString = m.group(2)
    val senseString = m.group(3)
    
    val pos = POS.getPOSForLabel(posString)
    val category = (Option(catString), Option(senseString))
    
    // Make sure ID will be unique for all possible matching symbols
    val id = if (idString == null || idString.isEmpty) {
      increment += 1
      s"anonymousSymbol$increment"
    } else {
      val categoryLabel = category match {
        case (Some(cat), Some(sense)) => s"-$cat-$sense"
        case (Some(cat), None) => s"-$cat"
        case (None, _) => ""
      }
      s"${pos.getLabel}${categoryLabel}${idString}"
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
      case WordReplacementSymbol(_, _, (None, _)) => dict.getRandomIndexWord(symbol.pos).getLemma
      // If a category is defined, draw from a list of hyponyms for the given category
      case WordReplacementSymbol(_, pos, (Some(cat), sense)) => getWordForCategory(pos, cat, sense)
    }
    symbolMap += (symbol.id -> replacement)
    replacement
  }

  /**
   * Find a random word matching the given category
   * 
   * @param pos Part of speech
   * @param category The category word to search under
   * @param sense A word to define which sense of the category word to use. Default picks first sense.
   */
  private def getWordForCategory(pos: POS, category: String, sense: Option[String]): String = {
    if (debug) println(s" Finding hyponym for $category: $sense")
    // Since words can have multiple meanings, we get a collection of possible matches for the category word
    val possibleCategories: List[Synset] = dict.getIndexWord(pos, category).getSenses
    // Using a list of posible meanings for the category word, and a list of possible meanings for the sense word, find
    // a relationship between the two. The presence of a relationship should indicate we've found the correct sense of
    // the category word to use.
    val hypernym = sense.flatMap { senseStr =>
      val possibleSenses: List[Synset] = dict.getIndexWord(pos, senseStr).getSenses 
      possibleCategories.find { category =>
        possibleSenses.find { senseDef =>
          RelationshipFinder.findRelationships(category, senseDef, PointerType.HYPONYM).nonEmpty
        }.nonEmpty
      }
    }.getOrElse(possibleCategories(0))
    // Find a random value in the tree of hyponyms
    val hyponymTree = PointerUtils.getHyponymTree(hypernym)
    val hyponymBranch = hyponymTree(Random.nextInt(hyponymTree.size))
    if (debug) hyponymBranch.print()
    // Use a random synonym from the last node on the branch
    val synonyms = hyponymBranch.getLast.getSynset.getWords
    synonyms(Random.nextInt(synonyms.size)).getLemma
  }
  
  /**
   * Parse an input string for replacement symbols, and replace them with random strings
   */
  def parseText(inputText: String): String = {
    WordReplacementSymbol.regex.replaceAllIn(inputText, getReplacementForSymbol(_))
  }
}

object AdLib {
  def main(args: Array[String]) = {
    val parser = new AdLibParser

    args.foreach { arg =>
      val inputText = Source.fromFile(arg).mkString
      println(parser.parseText(inputText))
    }
  }
}
