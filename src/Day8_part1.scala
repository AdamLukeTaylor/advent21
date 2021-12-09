import scala.collection.mutable
import scala.io.Source

object Day8_part1 extends App {
  val example = "data/day8_1_example"
  val myData = "data/day8_1_data"
  val dataFile = myData
  var displays = Source.fromFile(dataFile).getLines.toList.map(Input.build)
  //1 - 2 4 - 4 7 - 3 8 - 7
  println(s"${displays}")
  println(s"1, 4, 7, 8 count ${displays.map(display => display.partOne).sum}")

  case class Input(input: Seq[String], output: Seq[String]){
    def partOne: Int = {
      def isUniqueNum(input:String): Int = if(Seq(2, 4, 3, 7).contains(input.length)) 1 else 0
      output.foldLeft(0)((tot, item) => tot + isUniqueNum(item))

    }
  }

  object Input {
    def build(input: String): Input = {
      val parts = input.split("#")
      Input(input = parts(0).split(" ").toSeq, output = parts(1).split(" ").toSeq)
    }
  }

}