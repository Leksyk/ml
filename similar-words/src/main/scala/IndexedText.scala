

class IndexedText(val text: Vector[Int],
                  private val indexToWord: Vector[String],
                  private val wordToIndex: Map[String, Int],
                  private val wordIndexToOccurrences: Vector[Vector[Int]]) {
  def numWords = indexToWord.length
  def uniqueWords = indexToWord
  def wordIndex(word: String) = wordToIndex(word)
  def wordFromIndex(index: Int) = indexToWord(index)
  def wordOccurrences(wordIndex: Int) = wordIndexToOccurrences(wordIndex)
  println("length (#word): " + text.length)
  println("# unique words: " + uniqueWords.length)
  println("most common words: " + wordIndexToOccurrences.sortBy(-_.length).take(300).map(inds => wordFromIndex(text(inds.head))).toString())
}

object TextIndexer {
  def index(text: String): IndexedText = {
    // Let's get rid of apostrophes.
    var cleanedText = """\'""".r replaceAllIn(text, "")
    cleanedText = """\[^A-Za-z]+""".r replaceAllIn(text, " ")
    // 2 or more sequential spaces will collapse to one.
    cleanedText = """\s{2}""".r replaceAllIn(cleanedText, " ")
    cleanedText = cleanedText toLowerCase
    // Now we have iterator over all indexToWord.
    val wordsIterator = """\s""".r split cleanedText
    var wordToIndex = Map.empty[String, Int]
    var indexToWord = Vector.empty[String]
    var textWordsIndexes = Vector.empty[Int]
    var wordIndexToOccurrences: Vector[Vector[Int]] = Vector.empty[Vector[Int]]
    var cursor: Int = 0
    for (word <- wordsIterator) {
      val indexOpt: Option[Int] = wordToIndex.get(word)
      val index: Int = if (indexOpt isEmpty) {
        val index = indexToWord.length
        wordToIndex = wordToIndex updated(word, index)
        indexToWord = indexToWord :+ word
        wordIndexToOccurrences = wordIndexToOccurrences :+ Vector.empty[Int]
        index
      } else {
        indexOpt get
      }
      textWordsIndexes = textWordsIndexes :+ index
      wordIndexToOccurrences = wordIndexToOccurrences updated (index, wordIndexToOccurrences(index) :+ cursor)
      cursor = cursor + 1
    }
    new IndexedText(textWordsIndexes, indexToWord, wordToIndex, wordIndexToOccurrences)
  }
}