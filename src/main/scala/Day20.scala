package adventofcode

import scala.io.Source
import scala.collection.mutable
import scala.collection.SortedSet
import scala.collection.mutable.HashSet

object Day20 {

  implicit class ChainOps[A](val value: A) extends AnyVal {
    def |>[B](f: A => B): B = f(value)
  }

  case class Tile(id: Int, image: Seq[String]) {
    def top: String = image.head
    def bottom: String = image.last
    def left: String = image.map(_.head).mkString
    def right: String = image.map(_.last).mkString

    def rotate: Tile = Tile(id, rotate(image))
    def flip: Tile = Tile(id, image.map(_.reverse))
    def invert: Tile = Tile(id, invert(image))

    private def rotate(input: Seq[String]): Seq[String] =
      for {
        i <- 0 until input(0).size
      } yield (input.map(_(i)).mkString)

    private def invert(input: Seq[String]): Seq[String] =
      for {
        i <- (input.size - 1) to 0 by -1
      } yield (input(i))
  }

  case class Result(id: Int, top: Option[Int], botton: Option[Int], left: Option[Int], right: Option[Int]) {
    def matches: Seq[Int] = Seq(top, botton, left, right).flatMap(_.toList)
    def count: Int = matches.size
  }

  def parse(input: String): Seq[Tile] = 
    input.split("\n\n").map(parseTile).toSeq

  def parseTile(input: String): Tile = {
    val id :: image = input.split('\n').toList

    Tile(id.filter(_.isDigit).toInt, image)
  }

  def searchRight(tile: Tile, tiles: Seq[Tile]): Option[Int] = {
    tiles.filterNot(_.id == tile.id).find(_.left == tile.right).map(_.id)
  }

  def searchLeft(tile: Tile, tiles: Seq[Tile]): Option[Int] = {
    tiles.filterNot(_.id == tile.id).find(_.right == tile.left).map(_.id)
  }

  def searchBottom(tile: Tile, tiles: Seq[Tile]): Option[Int] = {
    tiles.filterNot(_.id == tile.id).find(_.top == tile.bottom).map(_.id)
  }

  def searchTop(tile: Tile, tiles: Seq[Tile]): Option[Int] = {
    tiles.filterNot(_.id == tile.id).find(_.bottom == tile.top).map(_.id)
  }

  def findMatches(tiles: Seq[Tile]): Map[Int, Result] = {
    val rotated = tiles.map(_.rotate)
    val rnf = rotated.map(_.flip)
    val flipped = tiles.map(_.flip)
    val fnr = flipped.map(_.rotate)
    val inverted = tiles.map(_.invert)
    val inf = inverted.map(_.flip)
    val infnr = inf.map(_.rotate)

    val all = tiles ++ rotated ++ flipped ++ inverted ++ rnf ++ fnr ++ inf ++ infnr

    val result = all.map { t => Result(
      t.id,
      searchTop(t, all), 
      searchBottom(t, all), 
      searchLeft(t, all),
      searchRight(t, all))
    }

    result.groupBy(_.id)
      .map(x => (x._1, x._2.maxBy(_.count)))
      .toMap
  }

  def part1(input: Map[Int, Result]): Long =
    input.values.filter(_.count == 2)
      .map(_.id).foldLeft(1L)(_ * _)

  sealed trait Move {
    def turn: Move
    def reverse: Move
    def next(input: Result): Option[Int]
  }
  case object Top extends Move {
    override def turn: Move = Right
    override def reverse: Move = Bottom
    override def next(input: Result): Option[Int] = input.top
  }
  case object Bottom extends Move {
    override def turn: Move = Left
    override def reverse: Move = Top
    override def next(input: Result): Option[Int] = input.botton
  }
  case object Left extends Move {
    override def turn: Move = Top
    override def reverse: Move = Right
    override def next(input: Result): Option[Int] = input.left
  }
  case object Right extends Move {
    override def turn: Move = Bottom
    override def reverse: Move = Left
    override def next(input: Result): Option[Int] = input.right
  }

  def search(input: Result, matches: Map[Int, Result]): (Int, Set[List[Int]]) = {

    val size = Math.sqrt(matches.size).toInt

    val memory = HashSet.empty[List[Int]]

    def follow(move: Move)(input: Int, path: List[Int]): List[Int] = {
      matches.get(input).flatMap(move.next) match {
        case Some(value) => 
          if (path.contains(value))
            // loop detected
            follow(move.turn)(value, path.takeWhile(_ != value))
          else
            follow(move)(value, path :+ value)
        case None => 
          if (path.size < size)
            // not completed
            follow(move.turn)(input, path)
          else if (path.size == size)
            // success
            path
          else
            Nil
        }
    }

    val paths = Seq(Top, Bottom, Left, Right)
      .map(follow(_)(input.id, List(input.id))).filterNot(_.isEmpty)
      .toSet

    (input.id, paths)
  }

  val input = Source.fromResource("tiles.txt").mkString
}

object Day20Part1 extends App {
  import Day20._

  val result = parse(input) |> findMatches |> part1
  
  println(result)
}

object Day20Part2 extends App {
  import Day20._
  
  val tiles = parse(input)

  val matches = findMatches(tiles)

  val Seq(corner1, corner2, corner3, corner4) = 
    matches.values.filter(_.count == 2).toSeq

  println(search(corner1, matches))
//  println(search(corner2, matches))
//  println(search(corner3, matches))
//  println(search(corner4, matches))
  
  matches.values.filter(_.count == 3).foreach(println)
}

object Day20Test extends App {
  import Day20._

  val x = Tile(1, List("TTT","...","BBB"))
  val y = Tile(1, List("L.R","L.R","L.R"))

  assert(x.top == "TTT")
  assert(x.bottom == "BBB")
  assert(y.left == "LLL")
  assert(y.right == "RRR")

  assert(x.rotate.left == x.top)
  assert(x.rotate.right == x.bottom)
  assert(y.flip.left == y.right)
  assert(y.flip.right == y.left)
  assert(x.invert.left == x.right.reverse)
  assert(x.invert.right == x.left.reverse)

  val input = """Tile 2311:
                |..##.#..#.
                |##..#.....
                |#...##..#.
                |####.#...#
                |##.##.###.
                |##...#.###
                |.#.#.#..##
                |..#....#..
                |###...#.#.
                |..###..###
                |
                |Tile 1951:
                |#.##...##.
                |#.####...#
                |.....#..##
                |#...######
                |.##.#....#
                |.###.#####
                |###.##.##.
                |.###....#.
                |..#.#..#.#
                |#...##.#..
                |
                |Tile 1171:
                |####...##.
                |#..##.#..#
                |##.#..#.#.
                |.###.####.
                |..###.####
                |.##....##.
                |.#...####.
                |#.##.####.
                |####..#...
                |.....##...
                |
                |Tile 1427:
                |###.##.#..
                |.#..#.##..
                |.#.##.#..#
                |#.#.#.##.#
                |....#...##
                |...##..##.
                |...#.#####
                |.#.####.#.
                |..#..###.#
                |..##.#..#.
                |
                |Tile 1489:
                |##.#.#....
                |..##...#..
                |.##..##...
                |..#...#...
                |#####...#.
                |#..#.#.#.#
                |...#.#.#..
                |##.#...##.
                |..##.##.##
                |###.##.#..
                |
                |Tile 2473:
                |#....####.
                |#..#.##...
                |#.##..#...
                |######.#.#
                |.#...#.#.#
                |.#########
                |.###.#..#.
                |########.#
                |##...##.#.
                |..###.#.#.
                |
                |Tile 2971:
                |..#.#....#
                |#...###...
                |#.#.###...
                |##.##..#..
                |.#####..##
                |.#..####.#
                |#..#.#..#.
                |..####.###
                |..#.#.###.
                |...#.#.#.#
                |
                |Tile 2729:
                |...#.#.#.#
                |####.#....
                |..#.#.....
                |....#..#.#
                |.##..##.#.
                |.#.####...
                |####.#.#..
                |##.####...
                |##..#.##..
                |#.##...##.
                |
                |Tile 3079:
                |#.#.#####.
                |.#..######
                |..#.......
                |######....
                |####.#..#.
                |.#...#.##.
                |#.#####.##
                |..#.###...
                |..#.......
                |..#.###...""".stripMargin

  val matches = parse(input) |> findMatches

  assert(part1(matches) == 20899048083289L)

  val Seq(corner1, corner2, corner3, corner4) = 
    matches.values.filter(_.count == 2).toSeq

  println(search(corner1, matches))
  println(search(corner2, matches))
  println(search(corner3, matches))
  println(search(corner4, matches))

  println("OK")
}