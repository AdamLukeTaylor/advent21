import scala.io.Source

object Day2_part1 extends App {
  val example = "data/day2_1_example"
  val myData = "data/day2_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList.map(Instruction.fromString)
  println(s"${input}")
  val start = Coord(0,0)
  val end = input.foldLeft(start)(_.move(_))
  println(s"Finish at ${end}, answer = ${end.answer}")
}
case class Coord(position: Int, depth: Int) {
  def move(instruction: Instruction): Coord = {
    instruction.direction match {
      case "forward" => Coord(position, depth + instruction.distance)
      case "down" => Coord(position + instruction.distance, depth)
      case "up" => Coord(position - instruction.distance, depth)
    }
  }

  def answer: Int = position * depth
}

case class Instruction(direction: String, distance: Int)
object Instruction {
  def fromString(input: String) = {
    val parts = input.split(" ")
    new Instruction(parts(0), parts(1).toInt)
  }
}