import java.io.FileWriter

import scala.io.Source

object Utils {
  implicit class Crossable[X](xs: Traversable[X]) {
    def cartezianProduct[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  def readFile(name: String): String = {
    val source = Source.fromFile(name)
    try source.mkString finally source.close()
  }

  def writeFile(name: String, content: String) = {
    val writer = new FileWriter(name)
    try writer.write(content) finally writer.close()
  }

  def harmonicMean(values: Seq[Double]): Double = {
    values.length / values.map(v => 1.0 / v).sum
  }

  def mean(values: Seq[Double]): Double = {
    if (values.nonEmpty) values.sum / values.length else 0
  }

  def similarityNGramsToString(indexedText: IndexedText, ngrams: Traversable[(NGram, NGram, Double)], limit: Int) = {
    ngrams.toVector.sortBy{case (_, _, score) => -score}
      .take(limit)
      .map{case (ng1, ng2, score) => "\t%s\n\t%s\nx=%f".format(
        ng1.toString(indexedText), ng2.toString(indexedText), score)}
      .mkString("\n")
  }
}
