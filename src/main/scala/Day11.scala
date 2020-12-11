package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day11 {

  sealed trait Tile extends Product with Serializable
  case object Floor extends Tile
  case object Free extends Tile
  case object Occupied extends Tile

  case class Matrix(seats: Vector[Vector[Tile]]) {

    def get(x: Int, y: Int): Option[Tile] =
      if (seats.isDefinedAt(y) && seats(y).isDefinedAt(x))
        Some(seats(y)(x))
      else
        None

    def adjacentTo(x: Int, y: Int): List[Tile] = {
      val up = (x, y + 1)
      val down = (x, y - 1)
      val left = (x - 1, y)
      val right = (x + 1, y)
      val upLeft = (x - 1, y + 1)
      val upRight = (x + 1, y + 1)
      val downLeft = (x - 1, y - 1)
      val downRight = (x + 1, y - 1)

      List(up, down, left, right, upLeft, upRight, downLeft, downRight).map { 
        case (x, y) => get(x, y) 
      }.flatMap(_.toList).filterNot(_ == Floor)
    }

    def occupied: Int = seats.flatMap(identity).count(_ == Occupied)

  }

  def parseLine(line: String): Vector[Tile] = 
    line.map(c => c match {
      case '.' => Floor
      case 'L' => Free
      case '#' => Occupied
    }).toVector

  def applyRules(matrix: Matrix): Matrix = {

    val after: Vector[Vector[Tile]] = matrix.seats.zipWithIndex.map { 
      case (row, y) => row.zipWithIndex.map {
        case (Floor, x) => Floor
        case (Occupied, x) => if (matrix.adjacentTo(x, y).count(_ == Occupied) > 3) Free else Occupied
        case (Free, x) => if (matrix.adjacentTo(x, y).forall(_ == Free) ) Occupied else Free
      } 
    }

    Matrix(after)
  }

  @tailrec
  def caos(matrix: Matrix): Matrix = {
    val m = applyRules(matrix)
    if (m == matrix) 
      m
    else
      caos(m)
  }
}

object Day11Part1 extends App {
  import Day11._

  val input = Source.fromResource("ferry.txt").getLines().map(parseLine).toVector

  println(caos(Matrix(input)).occupied)
}

object Day11Test extends App {
  import Day11._

  val input = """L.LL.LL.LL
                |LLLLLLL.LL
                |L.L.L..L..
                |LLLL.LL.LL
                |L.LL.LL.LL
                |L.LLLLL.LL
                |..L.L.....
                |LLLLLLLLLL
                |L.LLLLLL.L
                |L.LLLLL.LL""".stripMargin

  val map = input.linesIterator.map(parseLine).toVector

  val m = Matrix(map)

  assert(caos(m).occupied == 37)

  println("OK")
}