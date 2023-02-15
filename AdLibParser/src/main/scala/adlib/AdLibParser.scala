package adlib

import net.sf.extjwnl.data.relationship.RelationshipFinder
import net.sf.extjwnl.data.{POS, PointerType, PointerUtils, Synset, Word}
import net.sf.extjwnl.dictionary.Dictionary
import scala.collection.mutable
import scala.util.Random

/**
 * Used to parse a string for word replacement symbols and fill in random words
 *
 * @param dict The WordNet dictionary to use for word replacements
 * @param debug Enabe extra debugging
 */
class AdLibParser(
  val dict: Dictionary = Dictionary.getDefaultResourceInstance,
  val debug: Boolean = true,
) {
  val symbolCache: mutable.Map[String, String] = mutable.Map.empty
  
  /**
   * Naive scientific notation pattern match
   * Used to remove scientific names, because they don't work well in the output
   */
  private val scientificNotationRegex = "^[A-Z][a-z]+ [a-z]+$".r
  
  /**
   * Given a word replacement symbol, generates a matching random word
   */
  def getReplacementForSymbol(symbol: WordReplacementSymbol): String = {
    val replacement = symbolCache.get(symbol.slug).fold {
      symbol match {
        case WordReplacementSymbol(_, pos, None, _) => dict.getRandomIndexWord(pos).getLemma
        case WordReplacementSymbol(_, pos, Some(cat), sense) => getWordForCategory(pos, cat, sense)
      }
    } (identity)
    
    // If the word tag has an ID defined, cache it for future re-use
    if (symbol.id.nonEmpty && !symbolCache.contains(symbol.slug)) {
      symbolCache += (symbol.slug -> replacement)
    }
    
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
    // Using a list of possible meanings for the category word, and a list of possible meanings for the sense word, find
    // a relationship between the two. The presence of a relationship should indicate we've found the correct sense of
    // the category word to use.
    val hypernym = sense.flatMap { senseStr =>
      val possibleSenses: List[Synset] = dict.getIndexWord(pos, senseStr).getSenses
      possibleCategories.find { category =>
        possibleSenses.exists { senseDef =>
          RelationshipFinder.findRelationships(category, senseDef, PointerType.HYPONYM).nonEmpty
        }
      }
    }.getOrElse(possibleCategories.head)
    // Find a random value in the tree of hyponyms
    val hyponymTree = PointerUtils.getHyponymTree(hypernym)
    val hyponymBranch = hyponymTree(Random.nextInt(hyponymTree.size))
    if (debug) hyponymBranch.print()
    // Find the last node on the branch that does *not* consist solely of a scientific name
    val synonyms = hyponymBranch.foldRight[List[Word]](List.empty) { (node, filteredWords) =>
      if (filteredWords.nonEmpty) {
        filteredWords
      } else {
        node.getSynset.getWords.filterNot { word => scientificNotationRegex.matches(word.getLemma) }
      }
    }
    synonyms(Random.nextInt(synonyms.size)).getLemma
  }
  
  /**
   * Parse an input string for replacement symbols, and replace them with random strings
   */
  def parseText(inputText: String): String = {
    WordReplacementSymbol.replaceAllIn(inputText, getReplacementForSymbol)
  }
}
