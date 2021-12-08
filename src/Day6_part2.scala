import scala.io.Source

object Day6_part2 extends App {
  val example = "data/day6_1_example"
  val myData = "data/day6_1_data"
  val dataFile = myData
  val fishRaw = Source.fromFile(dataFile).getLines.toList.head.split(",").map(_.toInt).toSeq.groupBy(identity)
  var fish = fishRaw.map(row => row._1 -> BigInt(row._2.size))

  for (a <- 1 to 257) {
    println(s"Fish (day ${a - 1}): ${fish.foldLeft(BigInt(0))(_ + _._2).toString()}")
    val newGen = fish.map(group => advanceFish(group._1, group._2)).flatten
    val sixes = Seq((6,newGen.filter(_._1 == 6).foldLeft(BigInt(0))(_ + _._2)))
    fish = (newGen.filterNot(_._1 == 6) ++ sixes).toMap
  }


  def advanceFish(fishAge: Int, fishCount: BigInt): Seq[(Int, BigInt)] = {
    val oldFish = fishAge - 1
    if(oldFish < 0) {
      Seq((6,  fishCount),(8,  fishCount))
    }
    else Seq((oldFish, fishCount))

  }


}