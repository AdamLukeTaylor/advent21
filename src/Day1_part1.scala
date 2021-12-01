import scala.io.Source

object Day1_part1 extends App {
  val example = "data/day1_1_example"
  val myData = "data/day1_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList.map(_.toInt)
  println(s"${input}")
  val depthChanges = input.sliding(2).map { case Seq(x, y, _*) => y - x }.toList
  println(s"Increases ${depthChanges.count(_ > 0)} times ")
}
