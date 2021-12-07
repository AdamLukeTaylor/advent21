import jdk.internal.util.xml.impl.Input

import scala.io.Source

object Day4_part1 extends App {
  val example = "data/day4_1_example"
  val myData = "data/day4_1_data"
  val dataFile = myData
  val drawRaw :: rawBoards = Source.fromFile(dataFile).getLines.toList
  val draw = drawRaw.split(",").map(_.toInt).toList
  println(s"Draw: ${draw}")
  println(s"Boards: ${rawBoards}")
  var boards = Board.createBoards(rawBoards)
  println(boards)

  // feed in numbers and terminate
  try {
    draw.foreach { num =>
      boards = boards.map(_.mark(num))
      boards.zipWithIndex.foreach(
        boardWithIndex =>
          if (boardWithIndex._1.check) {
          println(s"Board ${boardWithIndex._2} is done ${boardWithIndex._1.sum} * ${num}= ${boardWithIndex._1.sum * num}")
          throw Done
        }
      )
      println(s"Drawn ${num}")
    }
  } catch {
    case Done => println("Done!")
  }

  case class Cell(num: Int, marked: Boolean = false) {
    def sum: Int = if (marked) 0 else num
  }

  case class Row(list: List[Cell]) {
    def mark(number: Int): Row = Row(list.map(cell => if (cell.num == number) Cell(number, true) else cell))

    def sum: Int = list.foldLeft(0)(_ + _.sum)
  }

  case class Board(rows: List[Row]) {
    def size: Int = rows.size

    def sum: Int = rows.foldLeft(0)(_ + _.sum)

    def mark(number: Int): Board = {
      new Board(rows.map(row => row.mark(number)))
    }

    def check: Boolean = checkH || checkV

    def checkH: Boolean = {
      rows.map(row => row.list.map(_.marked).foldLeft(true)(_ && _)).foldLeft(false)(_ || _)
    }

    def checkV: Boolean = {
      var result = false
      for (a <- rows.indices) {
        val colDone = rows.map(row => row.list(a)).map(_.marked).foldLeft(true)(_ && _)
        result = result || colDone
      }
      result
    }
  }

  object Done extends Throwable

  object Board {
    def createBoards(input: List[String]): List[Board] = {
      var inputRows = input
      var boards = List.empty[Board]
      while (inputRows.nonEmpty) {
        val nextBoardEnd = inputRows.indexOf("") + 1
        val (head, tail) = inputRows.splitAt(nextBoardEnd)
        boards = boards.appended(createBoard(head))
        if (nextBoardEnd <= 0) inputRows = List.empty else inputRows = tail
      }
      boards.filter(_.size > 0)
    }

    def createBoard(value: List[String]): Board = {
      new Board(value.filter(_.nonEmpty).map(row => new Row(row.split(" ").filter(_.nonEmpty).map(num => new Cell(num.toInt)).toList)).toList)
    }
  }
}