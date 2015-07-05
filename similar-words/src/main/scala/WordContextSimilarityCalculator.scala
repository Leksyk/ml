class WordSimilarity(val wordIndex: Int, val score: Double) {}

class WordContextSimilarityCalculator(
    private val indexedText: IndexedText,
    private val nGramExtractor: NGramExtractor) {

  private val nGramsSets = getNGramSetVectors(indexedText, nGramExtractor)

  def calcSimilarityTable(wordIndexes: Iterable[Int]): Map[Int, Vector[WordSimilarity]] = {
    var result = Map.empty[Int, Vector[WordSimilarity]]
    val parIterator = wordIndexes.par.map(wordIndex1 => (wordIndex1, calcTopSimilar(wordIndex1)))
    for ((wordInd, topSimilarityList) <- parIterator) {
      result = result updated(wordInd, topSimilarityList)
    }
    result
  }

  def calcTopSimilar(wordIndex1: Int): Vector[WordSimilarity] = {
    (0 until indexedText.numWords)
      .filter(ind => ind != wordIndex1)
      .par
      .map(wordIndex2 => new WordSimilarity(wordIndex2, calcWordSimilarity(wordIndex1, wordIndex2)))
      .toVector
      .sortBy(-_.score)
      .take(Flags.MAX_SIMILAR_WORDS)
  }

  def calcWordSimilarity(wordIndex1: Int, wordIndex2: Int): Double = {
    val sets1 = nGramsSets(wordIndex1)
    val sets2 = nGramsSets(wordIndex2)
    var sum = 0.0
    for (set1 <- sets1) {
      for (set2 <- sets2) {
        sum += calcNGramSetsSimilarity(set1, set2)
      }
    }
    sum / sets1.length * sets2.length
  }

//  def calcWordSimilarity(wordIndex1: Int, wordIndex2: Int): Double = {
//    val nGrams1 = nGramExtractor.getNGrams(wordIndex1)
//    val nGrams2 = nGramExtractor.getNGrams(wordIndex2)
//    var sum = 0.0
//    for (nGram1 <- nGrams1) {
//      for (nGram2 <- nGrams2) {
//        sum += calcNGramSimilarity(nGram1, nGram2)
//      }
//    }
//    sum / (nGrams1.length * nGrams2.length)
//  }

//  private def calcNGramSimilarity(ngram1: NGramRef, ngram2: NGramRef): Double = {
//    val set1 = getNGramWordIndexSet(ngram1)
//    val set2 = getNGramWordIndexSet(ngram2)
//    calcNGramSetsSimilarity(set1, set2)
//  }

  private def calcNGramSetsSimilarity(set1: Set[Int], set2: Set[Int]): Double = {
    val numCommon = (set1 & set2).count(_ => true)
    val numAll = (set1 ++ set2).count(_ => true)
    // TODO: can go over 1. Need to think/experiment on this.
    if (numCommon > 0) Math.pow(Flags.NGRAM_SET_SIMILARITY_POW_BASE, numCommon) / numAll else 0
  }

  private def getNGramWordIndexSet(ngram: NGramRef): Set[Int] = {
    var result = Set.empty[Int]
    var i = ngram.from
    while (i <= ngram.to) {
      if (i != ngram.center) {
        result = result + indexedText.text(i)
      }
      i = i + 1
    }
    result
  }

  private def getNGramSetVectors(indexedText: IndexedText, nGramExtractor: NGramExtractor): Vector[Vector[Set[Int]]] = {
    val result = Vector.newBuilder[Vector[Set[Int]]]
    result.sizeHint(indexedText.numWords)
    val seq = (0 until indexedText.numWords)
      .par
      .map(wordIndex => (wordIndex, nGramExtractor.getNGrams(wordIndex).map(getNGramWordIndexSet)))
      .toVector
    for ((wordIndex, ngrams) <- seq) {
      result.+=(ngrams)
    }
    println("ngram-set vectors prepared.")
    result result()
  }
}
