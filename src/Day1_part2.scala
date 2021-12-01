import scala.io.Source

object Day1_part2 extends App {
  val example = "data/day1_1_example"
  val myData = "data/day1_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList.map(_.toInt)
  println(s"${input}")
  val smoothed = input.sliding(3).map { case Seq(x, y, z, _*) => x + y + z}.toList
  val depthChanges = smoothed.sliding(2).map { case Seq(x, y, _*) => y - x }.toList
  println(s"Increases ${depthChanges.count(_ > 0)} times ")
}
