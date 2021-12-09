import scala.io.Source

object Day8_part2 extends App {
  val example = "data/day8_1_example"
  val example2 = "data/day8_2_example"
  val myData = "data/day8_1_data"
  val dataFile = myData
  var displays = Source.fromFile(dataFile).getLines.toList.map(Input.build)
  //1 - 2 4 - 4 7 - 3 8 - 7
  println(s"${displays}")
  //println(s"1, 4, 7, 8 count ${displays.map(display => display.partOne).sum}")
  println(s"part 2 ${displays.map(_.partTwo).sum}")

  case class Input(input: Seq[String], output: Seq[String]) {
    def partOne: Int = {
      def isUniqueNum(input: String): Int = if (Seq(2, 4, 3, 7).contains(input.length)) 1 else 0

      output.foldLeft(0)((tot, item) => tot + isUniqueNum(item))
    }

    def partTwo: Int = {
      def translate(str: String): Int = {
        // known by length
        // 1 - cf
        // 4 - bcdf
        // 7 - acf
        // 8 - abcdefg

        // unknown cell configurations
        // 0 - abcefg
        // 2 - acdeg
        // 3 - acdfg
        // 5 - abdfg
        // 6 - abdefg
        // 9 - abcdfg

        // a = 7 - 1
        // bd = 4 - 1
        // fg = len5 (with abd) - abd
        // f = fg and 1
        // c = 1 - f
        // g = fg - f
        // d = 3 - acfg (fg + 7)
        // b = bd - d
        // e = 8 - abcefg (4 + ag)
        val one = input.find(_.length == 2).get
        val four = input.find(_.length == 4).get
        val seven = input.find(_.length == 3).get
        val eight = input.find(_.length == 7).get
        val a = seven.toList.filter(!one.toList.contains(_))
        val bd = four.toList.filter(!one.toList.contains(_))
        val fg = input.filter(_.length == 5).filter(_.contains(a.head.toString)).filter(_.contains(bd.head.toString)).filter(_.contains(bd.reverse.head.toString)).head.filter(!a.toList.contains(_)).filter(!bd.toList.contains(_)) //TODO filter not find
        val f = fg.toList.filter(one.toList.contains(_))
        val c = one.toList.filter(!f.toList.contains(_))
        val g = fg.toList.filter(!f.toList.contains(_))
        val three = input.filter(_.length == 5).filter(_.contains(c.head.toString)).filter(_.contains(f.head.toString)).head
        val d = three.toList.filter(!fg.toList.contains(_)).filter(!seven.toList.contains(_))
        val b = bd.toList.filter(!d.toList.contains(_))
        val e = eight.toList.filter(!four.toList.contains(_)).filter(!a.toList.contains(_)).filter(!g.toList.contains(_))
        val zero = List(a, b, c, e, f, g).flatten
        val two = List(a, c, d, e, g).flatten
        val five = List(a, b, d, f, g).flatten
        val six = List(a, b, d, e, f, g).flatten
        val nine = List(a, b, c, d, f, g).flatten

        str.sorted match {
          case thing if (thing == zero.sorted.mkString("")) => 0
          case thing if (thing == one.sorted.mkString("")) => 1
          case thing if (thing == two.sorted.mkString("")) => 2
          case thing if (thing == three.sorted.mkString("")) => 3
          case thing if (thing == four.sorted.mkString("")) => 4
          case thing if (thing == five.sorted.mkString("")) => 5
          case thing if (thing == six.sorted.mkString("")) => 6
          case thing if (thing == seven.sorted.mkString("")) => 7
          case thing if (thing == eight.sorted.mkString("")) => 8
          case thing if (thing == nine.sorted.mkString("")) => 9
          case thing => {
            println(s"oh no - ${thing}")
            0
          }
        }
      }

      output.foldLeft(0)((tot, item) => tot * 10 + translate(item))

    }
  }

  object Input {
    def build(input: String): Input = {
      val parts = input.split("#")
      Input(input = parts(0).split(" ").toSeq.filter(_.length > 1), output = parts(1).split(" ").toSeq.filter(_.length > 1))
    }
  }
}

//println(s"one = $one")
//println(s"four = $four")
//println(s"seven = $seven")
//println(s"eight = $eight")
//println(s"a = $a")
//println(s"bd = $bd")
//println(s"fg = $fg")
//println(s"f = $f")
//println(s"c = $c")
//println(s"g = $g")
//println(s"three = $three")
//println(s"d = $d")
//println(s"b = $b")
//println(s"e = $e")