import scala.io.Source

object Day2_part2 extends App {
  val example = "data/day2_1_example"
  val myData = "data/day2_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList.map(Instruction.fromString)
  println(s"${input}")
  val start = Coord(0, 0, 0)
  val end = input.foldLeft(start)(_.move(_))
  println(s"Finish at ${end}, answer = ${end.answer}")


  case class Coord(position: Int, depth: Int, aim: Int) {
    def move(instruction: Instruction): Coord = {
      instruction.direction match {
        case "forward" => Coord(position + instruction.distance, depth + instruction.distance * aim, aim)
        case "down" => Coord(position, depth, aim + instruction.distance)
        case "up" => Coord(position, depth, aim - instruction.distance)
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
}