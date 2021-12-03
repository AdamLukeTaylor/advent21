import scala.io.Source

object Day3_part1 extends App {
  val example = "data/day3_1_example"
  val myData = "data/day3_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList.map(_.toCharArray.toList)

  var gammas = List.empty[Int]
  for (a <- 0 until input.head.length) {
    val row = input.map(_ (a)).map(_.toInt - '0'.toInt)
    val (ones, zeros) = row.partition(_ == 1)
    if (ones.length > zeros.length) gammas = gammas.appended(1) else gammas = gammas.appended(0)
  }
  val epsilons = gammas.map(num => if (num == 1) 0 else 1)
  val gamma = Integer.parseInt(gammas.mkString(""), 2)
  val epsilon = Integer.parseInt(epsilons.mkString(""), 2)

  println(s"Gammas at ${gamma} * ${epsilon} = ${gamma * epsilon}")

}