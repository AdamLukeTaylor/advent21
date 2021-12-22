import scala.io.Source

object Day11_part1 extends App {
  val example = "data/day11_1_example"
  val myData = "data/day11_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList

  var octopuses = List.empty[Octopus]
  octopuses = input.zipWithIndex.flatMap { case (line, row) => line.toList.zipWithIndex.map { case (level, col) => Octopus(level.toInt - '0'.toInt, col, row) } }

  println(s"pre")
  for (b <- 0 to 9) {
    println(s"${octopuses.filter(_.row == b).map { oct => oct.level.toString.concat(if (oct.hasFlashed) "*" else "-") }.mkString("")}")
  }
  var flashCount = 0
  for (a <- 1 to 400) {
    octopuses = octopuses.map { octo => octo.charge }
    // println("charged " + octopuses.take(5))
    while (octopuses.count(_.willFlash) > 0) {
      //println(s"to flash ${octopuses.count(_.willFlash)}")
      val flasher = octopuses.find(_.willFlash).get
      val (flashees, unaffected) = octopuses.partition { octo =>
        octo.row >= flasher.row - 1 && octo.row <= flasher.row + 1 &&
          octo.col >= flasher.col - 1 && octo.col <= flasher.col + 1
      }
      octopuses = unaffected ++ {
        flashees.filterNot(_ == flasher)
      }.map(_.charge) ++ List(flasher.flash)
//      println(s"flashed - ${a}")
//      for (b <- 0 to 9) {
//        for (c <- 0 to 9) {
//          print(s"${octopuses.filter{oct => oct.row == b && oct.col == c}.map { oct => oct.level.toString.concat(if (oct.hasFlashed) "*" else "-") }.mkString("")}")
//        }
//        println("")
//      }
    }
    if(octopuses.count(_.hasFlashed) == octopuses.size) println(s"${a}")
    octopuses = octopuses.map(_.reset)
//    println(s"post - ${a}")
//    for (b <- 0 to 9) {
//      for (c <- 0 to 9) {
//        print(s"${octopuses.filter{oct => oct.row == b && oct.col == c}.map { oct => oct.level.toString.concat(if (oct.hasFlashed) "*" else "-") }.mkString("")}")
//      }
//      println("")
//    }

  }
  println(s"flashes ${flashCount}")

  case class Octopus(level: Int, col: Int, row: Int, hasFlashed: Boolean = false) {
    def charge: Octopus = if(hasFlashed) this else Octopus(level + 1, col, row)

    def flash: Octopus = {
      flashCount = flashCount + 1
      Octopus(0, col, row, true)
    }

    def willFlash: Boolean = level > 9 && !hasFlashed

    def reset: Octopus = if(hasFlashed) Octopus(0, col, row, false) else Octopus(level, col, row, false)
  }
}

//5483143223
//2745854711
//5264556173
//6141336146
//6357385478
//4167524645
//2176841721
//6882881134
//4846848554
//5283751526