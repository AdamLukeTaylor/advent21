import scala.io.Source

object Day10_part1 extends App {
  val example = "data/day10_1_example"
  val myData = "data/day10_1_data"
  val dataFile = example
  val input = Source.fromFile(dataFile).getLines.toSeq//.filter(line => isNotValid(line.head, line.tail))
  println(input.head)
  println(removePair(input.head))
  println(removePair(removePair(input.head)))
  println(removePair(removePair(removePair(input.head))))

  def isNotValid(input: String): Boolean = {
    println(s"$input")
    while (removePair(input).nonEmpty) {


    }
    false
  }

  def removePair(str: String): String = {
    val target = str.head match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }
    val selection = str.toList.zipWithIndex.filter {
      case (bracket, place) => bracket == str.head || bracket == target
    }
      val closer = selection.zipWithIndex.find{case ((bracket, innerPlace), place) => bracket == target}
    if(closer.isEmpty) ""
    else {
      val closerLocation = closer.get._1._2
      val openerLocation = selection(closer.get._2 - 1)._2
      str.substring(0,openerLocation ) + str.substring(openerLocation + 1, closerLocation) + str.substring(closerLocation +1)
    }
  }
}