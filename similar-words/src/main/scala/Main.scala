import Utils._

object Main extends App {
  val fileNames: Array[String] = args

  assert(fileNames.length == 1, "Only single file is supported now")
  val fileName = fileNames(0)

  findSimilarWords(readFile(fileName), "me", "love", "parent", "he", "you", "time", "michael", "steve", "jeffrey")

  def findSimilarWords(text: String, words: String*) {
    val indexedText = TextIndexer index text
    println("Text indexed.")
    val nGramExtractor = new NGramExtractor(Flags.NGRAM_HALF_LENGTH, indexedText)
    val wordSimilarityCalculator = new WordContextSimilarityCalculator(indexedText, nGramExtractor)
    val wordIndexes = words.map(indexedText.wordIndex)
    val similarityTable = wordSimilarityCalculator.calcSimilarityTable(wordIndexes)
    println("Similarities calculated.")
    val report = similarityTableToString(indexedText, similarityTable)
    println(report)
  }

  def similarityTableToString(indexedText: IndexedText, table: Map[Int, Vector[WordSimilarity]]): String = {
    var result = ""
    for ((wordIndex, similarWords) <- table) {
      result = result + "%s: " format indexedText.wordFromIndex(wordIndex)
      for (similarWord <- similarWords) {
        val word = indexedText.wordFromIndex(similarWord.wordIndex)
        result = result + "%s (%e) " format(word, similarWord.score)
      }
      result = result + "\r\n"
    }
    result
  }
}