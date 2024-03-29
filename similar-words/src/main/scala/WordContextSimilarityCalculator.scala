import Utils._

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

  def calcWordGroupTopSimilar(wordIndexes: Iterable[Int]): Vector[WordSimilarity] = {
    val wordIndexesSet: Set[Int] = wordIndexes.toSet
    (0 until indexedText.numWords)
      .filter(ind => !wordIndexesSet.contains(ind))
      .par
      .map(wordIndex2 => new WordSimilarity(wordIndex2, calcWordGroupSimilarity(wordIndexes, wordIndex2)))
      .toVector
      .sortBy(-_.score)
      .take(Flags.MAX_SIMILAR_WORDS)
  }

  def getWordSimilarityNGrams(sourceWordIndex: Int, candidateWordIndex: Int): Traversable[(NGram, NGram, Double)] = {
    val ngrams1 = nGrams(sourceWordIndex)
    val ngrams2 = nGrams(candidateWordIndex)
    var ngramOverlapsBuilder = Vector.newBuilder[Double]
    var zeroCount = 0
    (ngrams1 cartezianProduct ngrams2)
      .filter { case (ng1, ng2) => ng1.center - ng2.center > Flags.MIN_NGRAM_DIST }
      .map { case (ng1, ng2) => (ng1, ng2, ng1 & ng2) }
  }

  def calcWordSimilarity(sourceWordIndex: Int, candidateWordIndex: Int): Double = {
    val scores = getWordSimilarityNGrams(sourceWordIndex, candidateWordIndex)
    scores.map{case (ng1, ng2, score) => score}.sum
  }

  def calcWordGroupSimilarity(wordIndexes: Iterable[Int], candidateWordIndex: Int): Double = {
    val similaritiesVector = wordIndexes
      .par
      .map(wordIndex => calcWordSimilarity(wordIndex, candidateWordIndex))
    similaritiesVector.sum
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
