package adventofcode

import scala.io.Source
import scala.annotation.tailrec

sealed trait Tile
case object Tree extends Tile
case object Empty extends Tile

case class Point(x: Int, y: Int) {
  def move(move: Point) = Point(x + move.x, y + move.y)
}

case class Row(tiles: IndexedSeq[Tile]) {
  def get(x: Int): Tile = tiles(x % tiles.length)

  def print = 
    println(tiles.map(t => t match { case Tree => '#' case Empty => '.'}).mkString)
}

case class Matrix(rows: IndexedSeq[Row]) {
  def get(point: Point): Tile = rows(point.y).get(point.x)

  def isTree(point: Point): Int = get(point) match {
    case Tree => 1
    case Empty => 0
  }

  @tailrec
  final def searchTrees(initial: Point, move: Point, current: Long = 0): Long =
    if (initial.y < rows.size)
      searchTrees(initial.move(move), move, current + isTree(initial))
    else current

  def print = rows.foreach(_.print)
}

object Matrix {
  
  def parseLine(line: String): Row =
    Row(line.map(x => if (x == '#') Tree else Empty))

  def test(): Matrix = {
    val string = """..##.......
                    |#...#...#..
                    |.#....#..#.
                    |..#.#...#.#
                    |.#...##..#.
                    |..#.##.....
                    |.#.#.#....#
                    |.#........#
                    |#.##...#...
                    |#...##....#
                    |.#..#...#.#""".stripMargin

    val rows = string.split('\n').map(parseLine).toIndexedSeq
    Matrix(rows)
  }

  def load(): Matrix = {
    val rows = Source.fromFile("src/main/resources/map.txt").getLines().map(parseLine).toIndexedSeq
    Matrix(rows)
  }
}

object Day3Part1 extends App {

  println("Day3 Part1")

  val matrix = Matrix.load()

  val initial = Point(0, 0)
  val move = Point(3, 1)
  println(s"${matrix.searchTrees(initial, move)} trees")
}

object Day3Part2 extends App {

  println("Day3 Part2")

  val matrix = Matrix.load()

  val initial = Point(0, 0)
  val moves = List(Point(1, 1), Point(3, 1), Point(5, 1), Point(7, 1), Point(1, 2))
  val result = moves.map(matrix.searchTrees(initial, _)).fold(1L)(_ * _)
  println(result)
}

object Day3Test extends App {

  println("Day3 Test")

  val matrix = Matrix.test()

  val initial = Point(0, 0)
  val moves = List(Point(1, 1), Point(3, 1), Point(5, 1), Point(7, 1), Point(1, 2))
  val result = moves.map(matrix.searchTrees(initial, _)).fold(1L)(_ * _)
  
  assert(result == 336L)
}