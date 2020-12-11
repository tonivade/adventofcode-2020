package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day11 {

  sealed trait Tile
  case object Floor extends Tile
  case object Free extends Tile
  case object Occupied extends Tile

  type Motion = (Int, Int)

  case class Position(x: Int, y: Int) {
    def move(motion: Motion): Position = 
      Position(x + motion._1, y + motion._2)
  }

  object Position {
    val up: Motion = (0, 1)
    val down: Motion = (0, -1)
    val left: Motion = (-1, 0)
    val right: Motion = (1, 0)
    val upLeft: Motion = (-1, 1)
    val upRight: Motion = (1, 1)
    val downLeft: Motion = (-1, -1)
    val downRight: Motion = (1, -1)

    val movements = 
      List(up, down, left, right, upLeft, upRight, downLeft, downRight)
  }

  case class Matrix(seats: Vector[Vector[Tile]]) {
    import Position.movements

    def map(f: (Position, Tile) => Tile): Matrix =
      Matrix(seats.zipWithIndex.map {
        case (row, y) => row.zipWithIndex.map {
          case (t, x) => f(Position(x, y), seats(y)(x))
        } 
      })

    def get(p: Position): Option[Tile] =
      if (seats.isDefinedAt(p.y) && seats(p.y).isDefinedAt(p.x))
        Some(seats(p.y)(p.x))
      else
        None

    def adjacentTo(p: Position): Int =
      movements.flatMap(move(p, _).toList).count(_ == Occupied)

    def visibleFrom(p: Position): Int =
      movements.flatMap(search(p, _)).size

    @tailrec
    final def search(p: Position, motion: Motion): List[Tile] = 
      move(p, motion) match {
        case Some(Occupied) => Occupied :: Nil
        case Some(Free) => search(p.move(motion), motion)
        case Some(Floor) => search(p.move(motion), motion)
        case None => Nil
      }

    def move(p: Position, motion: Motion): Option[Tile] = 
      get(p.move(motion))

    def occupied: Int = seats.flatMap(identity).count(_ == Occupied)

    def mkString: String = 
      seats.zipWithIndex.map {
        case (row, _) => row.zipWithIndex.map {
          case (Free, _) => 'L'
          case (Occupied, _) => '#'
          case (Floor, _) => '.'
        } 
      }.map(_.mkString).mkString("\n")
  }

  def parseLine(line: String): Vector[Tile] = 
    line.map(c => c match {
      case '.' => Floor
      case 'L' => Free
      case '#' => Occupied
    }).toVector

  def parseMatrix(input: String): Matrix = 
    Matrix(input.linesIterator.map(parseLine).toVector)

  def applyRules: Matrix => Matrix = m =>
    m.map {
      case (p, Floor) => Floor
      case (p, Occupied) => if (m.adjacentTo(p) >= 4) Free else Occupied
      case (p, Free) => if (m.adjacentTo(p) == 0) Occupied else Free
    } 

  def applyRules2: Matrix => Matrix = m =>
    m.map {
      case (p, Floor) => Floor
      case (p, Occupied) => if (m.visibleFrom(p) >= 5) Free else Occupied
      case (p, Free) => if (m.visibleFrom(p) == 0) Occupied else Free
    } 

  @tailrec
  def caos(matrix: Matrix, applyRules: Matrix => Matrix): Matrix = {
    val m = applyRules(matrix)
    if (m == matrix) 
      m
    else
      caos(m, applyRules)
  }

  val input = parseMatrix(Source.fromResource("ferry.txt").mkString)
}

object Day11Part1 extends App {
  import Day11._

  println(caos(input, applyRules).occupied)
}

object Day11Part2 extends App {
  import Day11._

  println(caos(input, applyRules2).occupied)
}

object Day11Test extends App {
  import Day11._

  val input1 = """L.LL.LL.LL
                 |LLLLLLL.LL
                 |L.L.L..L..
                 |LLLL.LL.LL
                 |L.LL.LL.LL
                 |L.LLLLL.LL
                 |..L.L.....
                 |LLLLLLLLLL
                 |L.LLLLLL.L
                 |L.LLLLL.LL""".stripMargin
  val map1 = parseMatrix(input1)
  assert(caos(map1, applyRules).occupied == 37)

  val input2 = """.......#.
                 |...#.....
                 |.#.......
                 |.........
                 |..#L....#
                 |....#....
                 |.........
                 |#........
                 |...#.....""".stripMargin
  val map2 = parseMatrix(input2)
  assert(map2.visibleFrom(Position(3, 4)) == 8)

  val input3 = """.##.##.
                 |#.#.#.#
                 |##...##
                 |...L...
                 |##...##
                 |#.#.#.#
                 |.##.##.""".stripMargin
  val map3 = parseMatrix(input3)
  assert(map3.visibleFrom(Position(3, 3)) == 0)

  val input4 = """.............
                 |.L.L.#.#.#.#.
                 |.............""".stripMargin
  val map4 = parseMatrix(input4)
  assert(map4.visibleFrom(Position(1, 1)) == 1)
  assert(map4.visibleFrom(Position(3, 1)) == 1)
  
  println(caos(map1, applyRules2).occupied == 26)

  println("OK")
}