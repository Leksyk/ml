class WordSimilarity(val wordIndex: Int, val score: Double) {}

class WordContextSimilarityCalculator(
    private val indexedText: IndexedText,
    private val nGramExtractor: NGramExtractor) {

  private val nGrams = getNGramSetVectors(indexedText, nGramExtractor)
  println("ngrams precalculated.")

  def calcSimilarityTable(wordIndexes: Iterable[Int]): Map[Int, Vector[WordSimilarity]] = {
    val parIterator = wordIndexes
      .par
      .map(wordIndex1 => (wordIndex1, calcTopSimilar(wordIndex1)))
      .toVector
    var result = Map.newBuilder[Int, Vector[WordSimilarity]]
    for ((wordInd, topSimilarityList) <- parIterator) {
      result = result += ((wordInd, topSimilarityList))
    }
    result result()
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

  def calcWordSimilarity(sourceWordIndex: Int, candidateWordIndex: Int): Double = {
    val ngrams1 = nGrams(sourceWordIndex)
    val ngrams2 = nGrams(candidateWordIndex)
    var ngramOverlapsBuilder = Vector.newBuilder[Double]
    var zeroCount = 0
    for (ngram1 <- ngrams1) {
      for (ngram2 <- ngrams2) {
        // This check still lets their edges overlap.
        if (Math.abs(ngram1.center - ngram2.center) > Flags.MIN_NGRAM_DIST) {
          val score: Double = ngram1 & ngram2
          if (score != 0) {
            ngramOverlapsBuilder += score
          } else {
            zeroCount += 1
          }
        }
      }
    }
    val nGramOverlaps = ngramOverlapsBuilder result()
    val sum = nGramOverlaps.sum
    sum * Math.pow(Flags.WORD_SIMILARITY_PROMOTION_BASE, 1 + sum) / ngrams1.length / ngrams2.length / (zeroCount + 1)
  }

  private def getNGramSetVectors(indexedText: IndexedText, nGramExtractor: NGramExtractor): Vector[Vector[NGram]] = {
    val result = Vector.newBuilder[Vector[NGram]]
    result.sizeHint(indexedText.numWords)
    val seq = (0 until indexedText.numWords)
      .par
      .map(wordIndex => (wordIndex, nGramExtractor.getNGrams(wordIndex)))
      .toVector
    for ((wordIndex, ngrams) <- seq) {
      result.+=(ngrams)
    }
    result result()
  }
}
