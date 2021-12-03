import scala.io.Source

object Day3_part2 extends App {
  val example = "data/day3_1_example"
  val myData = "data/day3_1_data"
  val dataFile = myData
  val input = Source.fromFile(dataFile).getLines.toList.map(_.toCharArray.toList)

  var oxygen = input
  try {
    for (a <- input.head.indices) {

      var gammas = List.empty[Int]
      for (a <- oxygen.head.indices) {
        val row = oxygen.map(_ (a)).map(_.toInt - '0'.toInt)
        val (ones, zeros) = row.partition(_ == 1)
        println(ones.length + " " + zeros.length)
        if (ones.length > zeros.length) gammas = gammas.appended(1) else if (ones.length == zeros.length) gammas = gammas.appended(1) else gammas = gammas.appended(0)
      }
      val epsilons = gammas.map(num => if (num == 1) 0 else 1)
      println(s"gamma ${gammas}, epeslon ${epsilons}")


      oxygen = oxygen.filter(row => row(a).toInt - '0'.toInt == gammas(a))
      if (oxygen.length == 1) throw AllDone
    }
  }
  catch {
    case AllDone => println(s"Oxygen = ${oxygen} ${Integer.parseInt(oxygen.flatten.mkString(""), 2)}")
  }
  var co2 = input
  try {
    for (a <- input.head.indices) {
      var gammas = List.empty[Int]
      for (a <- co2.head.indices) {
        val row = co2.map(_ (a)).map(_.toInt - '0'.toInt)
        val (ones, zeros) = row.partition(_ == 1)
        println(ones.length + " " + zeros.length)
        if (ones.length > zeros.length) gammas = gammas.appended(1) else if (ones.length == zeros.length) gammas = gammas.appended(1) else gammas = gammas.appended(0)
      }
      val epsilons = gammas.map(num => if (num == 1) 0 else 1)
      println(s"gamma ${gammas}, EPSILON ${epsilons}")


      co2 = co2.filter(row => row(a).toInt - '0'.toInt == epsilons(a))
      if (co2.length == 1) throw AllDone
    }
  }
  catch {
    case AllDone => {
      println(s"co2 = ${co2} ${Integer.parseInt(co2.flatten.mkString(""), 2)}")
    }
  }


  object AllDone extends Throwable

  println(s"Answer = ${Integer.parseInt(co2.flatten.mkString(""), 2) * Integer.parseInt(oxygen.flatten.mkString(""), 2)}")
}