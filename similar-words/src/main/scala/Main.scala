
object Main extends App {
  val fileNames: Array[String] = args

  assert(fileNames.length == 1, "Only single file is supported now")
  val fileName = fileNames(0)

  var text = readFile(fileName)
  val indexedText = TextIndexer index text
  val nGramExtractor = new NGramExtractor(Flags.NGRAM_HALF_LENGTH, indexedText)
  val wordSimilarityCalculator = new WordContextSimilarityCalculator(indexedText, nGramExtractor)
  val meIndex = indexedText.wordIndex("me")
  val larryIndex = indexedText.wordIndex("love")
  val parentIndex = indexedText.wordIndex("parent")
  val similarityTable = wordSimilarityCalculator.calcSimilarityTable(Vector.empty :+ larryIndex :+ parentIndex :+ meIndex)
  val report = similarityTableToString(indexedText, similarityTable)
  println("report: \r\n" + report)

  def readFile(name: String): String = {
    val source = scala.io.Source.fromFile(name)
    try source.mkString finally source.close()
  }

  def similarityTableToString(indexedText: IndexedText, table: Map[Int, Vector[WordSimilarity]]): String = {
    var result = ""
    for ((wordIndex, similarWords) <- table) {
      result = result + "%s: " format indexedText.wordFromIndex(wordIndex)
      for (similarWord <- similarWords) {
        val word = indexedText.wordFromIndex(similarWord.wordIndex)
        result = result + "%s (%6f) " format(word, similarWord.score)
      }
      result = result + "\r\n"
    }
    result
  }
}