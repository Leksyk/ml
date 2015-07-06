

class IndexedText(val text: Vector[Int],
                  val indexToWord: Vector[String],
                  val wordToIndex: Map[String, Int],
                  val wordIndexToOccurrences: Vector[Vector[Int]]) {
  def numWords = indexToWord.length
  def uniqueWords = indexToWord
  def wordIndex(word: String) = wordToIndex(word)
  def wordFromIndex(index: Int) = indexToWord(index)
  def wordOccurrences(wordIndex: Int) = wordIndexToOccurrences(wordIndex)
  println("length (#word): " + text.length)
  println("# unique words: " + uniqueWords.length)
  println("most common words: " + wordIndexToOccurrences
    .sortBy(-_.length)
    .take(300)
    .map(inds => wordFromIndex(text(inds.head)) + " (%d)".format(inds.length)))
}

object TextIndexer {
  def index(text: String): IndexedText = {
    val cleanedText = cleanUpText(text)
    // Now we have iterator over all words.
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

  def cleanUpText(text: String): String = {
    var cleanedText = text.toLowerCase
      .replaceAll("i\\'m", "i is")
      .replaceAll("\\bam\\b", "is")
      .replaceAll("\\bare\\b", "is")
      .replaceAll("\\bwere\\b", "is")
      .replaceAll("\\bwas\\b", "is")
      .replaceAll("it\'s", "it is")
      .replaceAll("\\'s", " has")
      .replaceAll("n\'t", " ")
      .replaceAll("\\bnot\\b", "")
      // Let's get rid of apostrophes.
      .replaceAll("\'", "")
    cleanedText = """[^a-z]+""".r replaceAllIn(cleanedText, " ")
    // 2 or more sequential spaces will collapse to one.
    cleanedText = """\s{2}""".r replaceAllIn(cleanedText, " ")
    cleanedText
  }
}
