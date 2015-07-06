import Utils._

object Main extends App {
  val fileNames: Array[String] = args

  assert(fileNames.length == 1, "Only single file is supported now")
  val fileName = fileNames(0)

  //explainWordDependencies(readFile(fileName), "love", "thanks")

  //findSimilarWords(readFile(fileName), "me", "love", "parent", "he", "you", "time", "michael", "steve", "jeffrey")
  findSimilarWordsToGroup(readFile(fileName),
    Vector("teacher", "daughter", "son", "mommy", "father", "child"),
    Vector("susie", "jonathan", "michael", "steve", "jeffrey", "paul"),
    Vector("see", "feel", "say", "think"),
    Vector("of", "on", "in", "at"))

  def explainWordDependencies(text: String, word1: String, word2: String) {
    val (indexedText, calculator) = indexAndPrepareCalculator(text)
    val ngrams = calculator.getWordSimilarityNGrams(
      indexedText.wordIndex(word1), indexedText.wordIndex(word2))
    println(Utils.similarityNGramsToString(indexedText, ngrams, 30))
  }

  def findSimilarWordsToGroup(text: String, wordGroups: Vector[String]*) {
    val (indexedText, wordSimilarityCalculator) = indexAndPrepareCalculator(text)

    def processGroup(group: Vector[String]) {
      val wordIndexes = group.map(indexedText.wordIndex)
      val similarityTable = wordSimilarityCalculator.calcWordGroupTopSimilar(wordIndexes)
      val report = similarityVectorToString(indexedText, similarityTable)
      println(group.toString + ": " + report)
    }

    wordGroups
      .par
      .foreach(processGroup)
  }

  def findSimilarWords(text: String, words: String*) {
    val (indexedText, wordSimilarityCalculator) = indexAndPrepareCalculator(text)
    val wordIndexes = words.map(indexedText.wordIndex)
    val similarityTable = wordSimilarityCalculator.calcSimilarityTable(wordIndexes)
    println("Similarities calculated.")
    val report = similarityTableToString(indexedText, similarityTable)
    println(report)
  }

  def indexAndPrepareCalculator(text: String): (IndexedText, WordContextSimilarityCalculator) = {
    val indexedText = TextIndexer index text
    println("Text indexed.")
    val nGramExtractor = new NGramExtractor(Flags.NGRAM_HALF_LENGTH, indexedText)
    val wordSimilarityCalculator = new WordContextSimilarityCalculator(indexedText, nGramExtractor)
    (indexedText, wordSimilarityCalculator)
  }

  def similarityVectorToString(indexedText: IndexedText, vector: Vector[WordSimilarity]): String = {
    val result = StringBuilder.newBuilder
    for (similarWord <- vector) {
      val word = indexedText.wordFromIndex(similarWord.wordIndex)
      result ++= ("%s (%e) " format(word, similarWord.score))
    }
    result result()


  }

  def similarityTableToString(indexedText: IndexedText, table: Map[Int, Vector[WordSimilarity]]): String = {
    val result = StringBuilder.newBuilder
    for ((wordIndex, similarWords) <- table) {
      result ++= ("%s: " format indexedText.wordFromIndex(wordIndex))
      result ++= similarityVectorToString(indexedText, similarWords)
      result ++= "\r\n"
    }
    result result()
  }
}