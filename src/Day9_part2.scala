import scala.io.Source

object Day9_part2 extends App {
  val example = "data/day9_1_example"
  val myData = "data/day9_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toSeq.map(Height.build)
  var heights = Array.fill[Height](input.length, input.head.length) {
    Height(-1, true)
  }
  var lows = Array.fill[Height](input.length, input.head.length) {
    Height(-1, true)
  }


  for (a <- 0 until input.length) {
    for (b <- 0 until input.head.length) {
      heights(a)(b) = input(a)(b).copy(lowest = false)
      lows(a)(b) = input(a)(b)
      if (a + 1 < input.length && input(a)(b).measure >= input(a + 1)(b).measure) {
        lows(a)(b) = input(a)(b).copy(lowest = false)
      }
      if (a - 1 >= 0 && input(a)(b).measure >= input(a - 1)(b).measure) {
        lows(a)(b) = input(a)(b).copy(lowest = false)
      }
      if (b - 1 >= 0 && input(a)(b).measure >= input(a)(b - 1).measure) {
        lows(a)(b) = input(a)(b).copy(lowest = false)
      }
      if (b + 1 < input.head.length && input(a)(b).measure >= input(a)(b + 1).measure) {
        lows(a)(b) = input(a)(b).copy(lowest = false)
      }
    }
  }
  var sizes = Seq.empty[Int]
  var toCheck = List.empty[Coord]
  for (a <- 0 until input.length) {
    for (b <- 0 until input.head.length) {
      if (lows(a)(b).lowest) { //TODO = in basin? joined basins
        var total = 0
        var totalBasin = 0
        for (a <- 0 until input.length) {
          for (b <- 0 until input.head.length) {
            heights(a)(b) = heights(a)(b).copy(lowest = false, basin = false)
          }
        }
        toCheck = List(Coord(a, b))
        while (toCheck.nonEmpty) {
          var used = List.empty[Coord]
          val item = toCheck.head
          toCheck = toCheck.tail
          if (item.x + 1 < input.length && heights(item.x)(item.y).measure != 9 && heights(item.x)(item.y).measure < heights(item.x + 1)(item.y).measure) {
            heights(item.x)(item.y) = heights(item.x)(item.y).copy(basin = true)
            toCheck = toCheck :+ Coord(item.x + 1, item.y)
          }
          if (item.x - 1 >= 0 && heights(item.x)(item.y).measure != 9 && heights(item.x)(item.y).measure < heights(item.x - 1)(item.y).measure) {
            heights(item.x)(item.y) = heights(item.x)(item.y).copy(basin = true)
            toCheck = toCheck :+ Coord(item.x - 1, item.y)
          }
          if (item.y - 1 >= 0 && heights(item.x)(item.y).measure != 9 && heights(item.x)(item.y).measure < heights(item.x)(item.y - 1).measure) {
            heights(item.x)(item.y) = heights(item.x)(item.y).copy(basin = true)
            toCheck = toCheck :+ Coord(item.x, item.y - 1)
          }
          if (item.y + 1 < input.head.length && heights(item.x)(item.y).measure != 9 && heights(item.x)(item.y).measure < heights(item.x)(item.y + 1).measure) {
            heights(item.x)(item.y) = heights(item.x)(item.y).copy(basin = true)
            toCheck = toCheck :+ Coord(item.x, item.y + 1)
          }
          used = used :+ item
          toCheck = toCheck.filterNot(used.contains(_))
        }

        for (a <- 0 until input.length) {
          for (b <- 0 until input.head.length) {
            //print(s"${heights(a)(b).measure}")
            if (heights(a)(b).lowest) {
              //print("*")
              total += heights(a)(b).measure + 1
            } else if (heights(a)(b).basin) {
              //print("+")
              totalBasin += 1
            }
          }
          //println("")
        }
        println(s"Low points - ${total}")
        println(s"Basin - ${totalBasin}")
        sizes = sizes :+ totalBasin
      }
    }
  }
  println(s"${sizes.sorted.reverse.take(3).product}")


  case class Coord(x: Int, y: Int)

  case class Height(measure: Int, lowest: Boolean = true, basin: Boolean = false) {

  }

  object Height {
    def build(str: String): List[Height] = str.toList.map(car => Height(car.toInt - '0'.toInt))
  }
}