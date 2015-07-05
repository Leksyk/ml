
/** Range of indexes within indexedText */
class NGram(val center: Int, val divisorMap: Map[Int, Double]) {
  /** Score - level of intersection between two NGrams. Doesn't take into account word order. */
  def &(other: NGram): Double = {
    var sum = 0.0
    var matchCount = 0
    for ((wordIndex, divisor) <- divisorMap) {
      val maybeOtherDivisor = other.divisorMap.get(wordIndex)
      if (maybeOtherDivisor.isDefined) {
        matchCount = matchCount + 1
        sum = sum + 1.0 / (divisor * maybeOtherDivisor.get)
      }
    }
    // Promote for having many matches.
    val notMatched = Math.max(divisorMap.size, other.divisorMap.size) - matchCount
    sum * matchCount * Math.pow(Flags.NGRAM_SIMILARITY_NOT_MATCH_PENALTY_BASE, -notMatched)
  }
}

/**
 * @param n how many words to take on each side of the given one.
 * @param indexedText indexed text to extract NGrams from.
 */
class NGramExtractor(private val n: Int, private val indexedText: IndexedText) {
  val lastIndex = indexedText.text.length - 1

  def getNGrams(wordIndex: Int): Vector[NGram] = {
    val occurrences = indexedText.wordOccurrences(wordIndex)
    val result = Vector.newBuilder[NGram]
    result.sizeHint(occurrences.length)
    for (index <- occurrences) {
      val mapBuilder = Map.newBuilder[Int, Double]
      for (position <- Math.max(index - n, 0) to Math.min(index + n, lastIndex)) {
        if (position != index) {
          val wordIndex = indexedText.text(position)
          val divisor = Math.pow(Flags.NGRAM_DIST_FROM_CENTER_PENALTY_BASE, Math.abs(position - index - 1))
          mapBuilder += ((wordIndex, divisor))
        }
      }
      result += new NGram(index, mapBuilder result())
    }
    result result()
  }
}
