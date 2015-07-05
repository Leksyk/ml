import java.io.FileWriter

import scala.io.Source

object Utils {
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
}
