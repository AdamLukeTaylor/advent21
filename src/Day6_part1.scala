import scala.io.Source

object Day6_part1 extends App {
  val example = "data/day6_1_example"
  val myData = "data/day6_1_data"
  val dataFile = myData
  var fish = Source.fromFile(dataFile).getLines.toList.head.split(",").map(_.toInt).toSeq

  for (a <- 1 to 81) {
    println(s"Fish (day ${a - 1}): ${fish.size}")
    fish = advanceFish(fish)
  }


  def advanceFish(fish: Seq[Int]): Seq[Int] = {
    val oldFish = fish.map(_ - 1)
    val rebornFish = oldFish.count(_ < 0)
    (oldFish.filter(_ >= 0) ++ Seq.fill(rebornFish)(6) ++ Seq.fill(rebornFish)(8))
  }


}