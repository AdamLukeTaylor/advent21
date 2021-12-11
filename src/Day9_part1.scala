import scala.io.Source

object Day9_part1 extends App {
  val example = "data/day9_1_example"
  val myData = "data/day9_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toSeq.map(Height.build)
  var heights = Array.fill[Height](input.length, input.head.length) {
    Height(-1, true)
  }


  for (a <- 0 until input.length) {
    for (b <- 0 until input.head.length) {
      heights(a)(b) = input(a)(b)
      if (a + 1 < input.length && input(a)(b).measure >= input(a + 1)(b).measure) {
        heights(a)(b) = input(a)(b).copy(lowest = false)
      }
      if (a - 1 >= 0 && input(a)(b).measure >= input(a - 1)(b).measure) {
        heights(a)(b) = input(a)(b).copy(lowest = false)
      }
      if (b - 1 >= 0 && input(a)(b).measure >= input(a)(b - 1).measure) {
        heights(a)(b) = input(a)(b).copy(lowest = false)
      }
      if (b + 1 < input.head.length && input(a)(b).measure >= input(a)(b + 1).measure) {
        heights(a)(b) = input(a)(b).copy(lowest = false)
      }
    }
  }
  var total = 0
  for (a <- 0 until input.length) {
    for (b <- 0 until input.head.length) {
      print(s"${heights(a)(b).measure}")
      if (heights(a)(b).lowest) {
        print("*")
        total += heights(a)(b).measure + 1
      }
    }
    println("")
  }
  println(s"Heights - ${total}")

  case class Height(measure: Int, lowest: Boolean = true)

  object Height {
    def build(str: String): List[Height] = str.toList.map(car => Height(car.toInt - '0'.toInt))
  }
}