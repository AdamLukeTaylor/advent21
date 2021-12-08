import scala.io.Source

object Day5_part2 extends App {
  val example = "data/day5_1_example"
  val myData = "data/day5_1_data"
  val dataFile = myData
  val rawLines = Source.fromFile(dataFile).getLines.toList

  val lines = rawLines.map(Line.build)
  println(s"${lines}")

  var matrix = Seq.empty[Coord]
  lines.foreach { line =>
    val dx = math.signum(line.start.x - line.end.x)
    val dy = math.signum(line.start.y - line.end.y)
    def isDiagonal: Boolean = dx * dy != 0
    def points: Seq[Coord] = //if(!isDiagonal) {
      Range.inclusive(0, math.max(math.abs(line.start.x - line.end.x), math.abs(line.start.y - line.end.y)))
        .map(step => Coord(line.end.x + dx * step, line.end.y + dy * step))
    //} else Seq.empty[Coord]

    matrix = matrix ++ points
  }

  println(s"${matrix.groupBy(identity).values.count(_.size > 1)}")

  case class Line(start: Coord, end: Coord) {

  }

  object Line {
    def build(input: String): Line = {
      val parts = input.split("->").toList
      Line(start = Coord.build(parts.head), Coord.build(parts(1)))
    }
  }

  case class Coord(x: Int, y: Int)

  object Coord {
    def build(input: String) = {
      val parts = input.replaceAll("\\s", "").split(",")
      Coord(x = parts.head.toInt, y = parts(1).toInt)
    }
  }

  case class Count(coord: Coord, count: Int)

}