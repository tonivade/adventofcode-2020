package adventofcode

import scala.io.Source
import scala.collection.mutable
import scala.collection.SortedSet
import scala.collection.mutable.HashSet
import scala.annotation.tailrec

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
    def contains(value: Int): Boolean = matches.contains(value)
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

  def connected(a: Int, b: Int, matches: Map[Int, Result]): Seq[Int] =
    matches.values.filter(r => r.contains(a) && r.contains(b)).map(_.id).toSeq

  def searchPath(from: Int, to: Int, matches: Map[Int, Result]): List[Int] = {
    val borders = matches.filter(_._2.count == 3).toMap

    @tailrec
    def loop(current: Int, to: Int, path: List[Int]): List[Int] = {
      val tile = matches(current)

      if (tile.contains(to))
        path :+ current :+ to
      else {
        val next = tile.matches
          .filter(borders.contains(_))
          .filterNot(path.contains(_))

        if (next.size == 1)
          loop(next.head, to, path :+ current)
        else Nil
      }
    }
      
    val input = matches(from)
    val output = input.matches.filter(borders.contains(_))

    val result = output.map(loop(_, to, from :: Nil)).filterNot(_.isEmpty)

    result match {
      case path :: Nil => path
      case _ => Nil
    }
  }

  def searchBorders(matches: Map[Int, Result]): Seq[List[Int]] = {
    val corners = matches.values.filter(_.count == 2).toSeq
    
    corners.combinations(2).map {
      case a :: b :: Nil => searchPath(a.id, b.id, matches)
      case _ => throw new IllegalArgumentException("error")
    }.filterNot(_.isEmpty).toList
  }

  def build(matches: Map[Int, Result], left: List[Int], top: List[Int]): List[List[Int]] = {
    def column(top: Int, prev: List[Int]): List[Int] =
      prev.tail.foldLeft(top :: Nil) {
        case (state, current) =>
          state :+ connected(state.last, current, matches).find(!prev.contains(_)).get
      }

    (1 to 11).foldLeft(left :: Nil) {
      case (state, current) => state :+ column(top(current), state.last)
    }
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

  val List(top, left, _, _) = searchBorders(matches)
  assert(top.head == left.head)

  build(matches, left, top).foreach(println)
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

  /*
   * 1951    2311    3079
   * 2729    1427    2473
   * 2971    1489    1171
   */
  searchBorders(matches).foreach(println)

  assert(connected(2729, 2311, matches).contains(1427))
  assert(connected(2971, 1427, matches).contains(1489))

  println("OK")
}