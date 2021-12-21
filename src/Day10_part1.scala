import scala.collection.mutable
import scala.io.Source

object Day10_part1 extends App {
  val example = "data/day10_1_example"
  val myData = "data/day10_1_data"
  val dataFile = example
  val input = Source.fromFile(dataFile).getLines.toSeq //.filter(line => isNotValid(line.head, line.tail))
  println(input.head)

  input.foreach { lineIn =>
    var line = lineIn
    println(s"Running ${line}")

    var open = mutable.Stack[(Char, Int)]()
    open = open :+ (line.toList.zipWithIndex.head)
    while (open.nonEmpty) {
      val toTest: (Char, Int) = open.pop()
      open.push(toTest)
      try {
        for (toCompare <- line.toList.zipWithIndex) {
          if (toCompare._2 > toTest._2) {
            println(s"Comparing ${toTest} and ${toCompare} ${line.toList(toTest._2)} ${line.toList(toCompare._2)}")
            if (matchPair(line.toList(toTest._2), line.toList(toCompare._2))) {
              line = crossOut(line, toTest, toCompare)
            }
            else {
              if(line.toList(toTest._2) !='X') {
                if (newCloser(toCompare._1)) throw NewCloser(toCompare)
                if (newOpener(toCompare._1)) throw NewOpener(toCompare)
              }
              else{
                if(open.nonEmpty) open.pop()
                throw Matched(toTest)
              }
            }
          }
        }
      } catch {
        case NewOpener(a) => open.push(a)
        case Matched(a) => {
          println(s"Step ${line}")
          line = crossOut(line, toTest, a)
        }
        case NewCloser(a) => {
          println(s"Error ${a}")
          open.clear()
        }
      }
    }
    println(s"Finished ${containsClosed(line)} ${line}")
  }

  def containsClosed(str: String): Boolean = {
    str.contains(']') || str.contains(')') || str.contains('}') || str.contains('>')
  }

  def newOpener(test: Char): Boolean = {
    test match {
      case '(' => true
      case '[' => true
      case '{' => true
      case '<' => true
      case _ => false
    }
  }

  def newCloser(test: Char): Boolean = {
    test match {
      case ')' => true
      case ']' => true
      case '}' => true
      case '>' => true
      case _ => false
    }
  }

  def matchPair(source: Char, test: Char): Boolean = {
    val target = source match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case _ => 'X'
    }
    target == test
  }

  def crossOut(input: String, open: (Char, Int), close: (Char, Int)): String = {
    (input.substring(0, open._2) + "X" + input.substring(open._2 + 1)).substring(0, close._2) + "X" + input.substring(close._2 + 1)
  }

  case class Matched(toAdd: (Char, Int)) extends Throwable

  case class NewOpener(toAdd: (Char, Int)) extends Throwable

  case class NewCloser(toAdd: (Char, Int)) extends Throwable
}