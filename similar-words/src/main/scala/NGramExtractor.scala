
/** Range of indexes within indexedText */
class NGram(val center: Int, val startPos: Int, val endPos: Int, val divisorMap: Map[Int, Double], val penaltyDivisor: Double) {
  /** Score - level of intersection between two NGrams. Doesn't take into account word order. */
  def &(other: NGram): Double = {
    var sum = 0.0
    var matchCount = 0
    for ((wordIndex, divisor) <- divisorMap) {
      val maybeOtherDivisor = other.divisorMap.get(wordIndex)
      if (maybeOtherDivisor.isDefined) {
        matchCount = matchCount + 1
        sum += 1.0 / (divisor * maybeOtherDivisor.get)
      }
    }
    // Promote for having many matches.
    sum * matchCount / penaltyDivisor / other.penaltyDivisor
  }

  def toString(indexedText: IndexedText): String = {
    val result = StringBuilder.newBuilder
    result ++= "%s(/%f) => ".format(indexedText.indexToWord(indexedText.text(center)), penaltyDivisor)
    result ++= (startPos to endPos)
      .map(pos => indexedText.text(pos))
      .map(wordIndex => (wordIndex, divisorMap.getOrElse(wordIndex, 0.0)))
      .map{ case (wordIndex, divisor) =>
        "%s/%f ".format(indexedText.indexToWord(wordIndex), divisor)
      }
      .mkString(" ")
    result result()
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
      val startPos = Math.max(index - n, 0)
      val endPos = Math.min(index + n, lastIndex)
      for (position <- startPos to endPos) {
        if (position != index) {
          val wordIndex = indexedText.text(position)
          val divisor = distancePenaltyDivisor(position, index) * popularityPenaltyDivisor(wordIndex)
          mapBuilder += ((wordIndex, divisor))
        }
      }
      result += new NGram(index, startPos, endPos, mapBuilder result(),
        popularityPenaltyDivisor(wordIndex))
    }
    result result()
  }

  def popularityPenaltyDivisor(wordIndex: Int): Double = {
    val popularity = indexedText.wordIndexToOccurrences(wordIndex).length
    Math.pow(Flags.NGRAM_WORD_POPULARITY_PENALTY_BASE, 100.0 * popularity.toDouble / indexedText.text.length)
  }

  def distancePenaltyDivisor(position: Int, center: Int): Double = {
    Math.pow(Flags.NGRAM_DIST_FROM_CENTER_PENALTY_BASE, Math.abs(position - center - 1))
  }
}
