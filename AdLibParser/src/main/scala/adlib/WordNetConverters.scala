package adlib

import net.sf.extjwnl.data.list.{PointerTargetNode, PointerTargetNodeList, PointerTargetTree}
import net.sf.extjwnl.data.relationship.Relationship
import net.sf.extjwnl.data.{Synset, Word}
import scala.jdk.CollectionConverters.*

/**
 * Allow using WordNet's Java API with scala collections
 */
implicit def convertSynsetList(synset: java.util.List[Synset]): List[Synset] = synset.asScala.toList
implicit def convertWordList(words: java.util.List[Word]): List[Word] = words.asScala.toList
implicit def convertWordArrayList(words: java.util.ArrayList[Word]): List[Word] = words.asScala.toList
implicit def convertPointerTargetTree(tree: PointerTargetTree): List[PointerTargetNodeList] = tree.toList.asScala.toList
implicit def convertPointerTargetNodeList(branch: PointerTargetNodeList): Seq[PointerTargetNode] = branch.asScala.toSeq
implicit def convertRelationshipList(relationship: java.util.ArrayList[Relationship]): List[Relationship] =
  relationship.asScala.toList
