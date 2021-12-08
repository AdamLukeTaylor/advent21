import scala.collection.mutable
import scala.io.Source

object Day7_part1 extends App {
  val example = "data/day7_1_example"
  val myData = "data/day7_1_data"
  val dataFile = myData
  var crabs = Source.fromFile(dataFile).getLines.toList.head.split(",").map(_.toInt).toSeq

  println(s"${crabs}")
  var fuel = mutable.Map.empty[Int, Int]
  for (a <- crabs.min until crabs.max) {
    val cost = crabs.foldLeft(0)((tot, crab) => tot + crabFuel(crab, a))
    fuel += (a -> cost)
  }

  def crabFuel(start: Int, end: Int): Int = {
    val dist = Math.abs(start - end)
    (0 to dist).sum
  }

  println(s"Cheapest = ${fuel.minBy(_._2)}")

}