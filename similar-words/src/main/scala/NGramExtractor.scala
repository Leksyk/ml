/** Range of indexes within indexedText */
class NGramRef(val center: Int, val from: Int, val to: Int)

/**
 * @param n how many words to take on each side of the given one.
 * @param indexedText indexed text to extract NGrams from.
 */
class NGramExtractor(private val n: Int, private val indexedText: IndexedText) {
  def getNGrams(wordIndex: Int): Vector[NGramRef] = {
    val lastIndex = indexedText.text.length - 1
    for (index <- indexedText.wordOccurrences(wordIndex))
      yield new NGramRef(index, Math.max(index - n, 0), Math.min(index + n, lastIndex))
  }
}
