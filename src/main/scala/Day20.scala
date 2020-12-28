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

  case class Tile(id: Int, image: Seq[String], fixed: Boolean = false) {
    def top: String = image.head
    def bottom: String = image.last
    def left: String = image.map(_.head).mkString
    def right: String = image.map(_.last).mkString

    def rotate: Tile = Tile(id, rotate(image))
    def flip: Tile = Tile(id, image.map(_.reverse))
    def invert: Tile = Tile(id, invert(image))

    def fix: Tile = Tile(id, image, true)

    def all: Seq[Tile] = 
      if (fixed)
        Seq(this)
      else
        Seq(
        this,
        rotate,
        rotate.flip,
        flip,
        flip.rotate,
        invert,
        invert.flip,
        invert.flip.rotate
      )

    def mkString: String = image.mkString("\n")

    def empty: Tile = Tile(id, image.map(_.map(_ => '?').mkString))

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
    val all = tiles.flatMap(_.all)

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

  def connectedTile(a: Int, b: Int, matches: Map[Int, Result]): Seq[Int] =
    matches.values.filter(r => r.contains(a) && r.contains(b)).map(_.id).toSeq

  def fixTile4(current: Tile, left: Tile, top: Tile): Tile = {
    val combinations = for {
      c <- current.all
      l <- left.all
      t <- top.all
    } yield(c, l, t)

    val result = combinations.filter {
      case (c, l, t) => c.left == l.right && c.top == t.bottom
    }

    result match {
      case (c, _, _) :: Nil => c.fix
      case _ => throw new IllegalStateException(s"${current.id}")
    }
  }

  def fixTile3(current: Tile, right: Tile, top: Tile): Tile = {
    val combinations = for {
      c <- current.all
      r <- right.all
      t <- top.all
    } yield(c, r, t)

    val result = combinations.filter {
      case (c, r, t) => c.right == r.left && c.top == t.bottom
    }

    result match {
      case (c, _, _) :: Nil => c.fix
      case _ => throw new IllegalStateException(s"${current.id}")
    }
  }

  def fixTile2(current: Tile, left: Tile, bottom: Tile): Tile = {
    val combinations = for {
      c <- current.all
      l <- left.all
      b <- bottom.all
    } yield(c, l, b)

    val result = combinations.filter {
      case (c, l, b) => c.left == l.right && c.bottom == b.top
    }

    result match {
      case (c, _, _) :: Nil => c.fix
      case _ => throw new IllegalStateException(s"${current.id}")
    }
  }

  def fixTile(current: Tile, right: Tile, bottom: Tile): Tile = {
    val combinations = for {
      c <- current.all
      r <- right.all
      b <- bottom.all
    } yield(c, r, b)

    val result = combinations.filter {
      case (c, r, b) => c.right == r.left && c.bottom == b.top
    }

    result match {
      case (c, _, _) :: Nil => c.fix
      case _ => throw new IllegalStateException(s"${current.id}")
    }
  }

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
          state :+ connectedTile(state.last, current, matches).find(!prev.contains(_)).get
      }

    (1 until top.size).foldLeft(left :: Nil) {
      case (state, current) => state :+ column(top(current), state.last)
    }
  }

  def fix(image: Seq[List[Int]], tiles: Map[Int, Tile]): Map[Int, Tile] = {
    val fixed = (0 until (image(0).size - 1)).flatMap { i =>
      val current = image.map(_(i))
      val next = image.map(_(i + 1))

      (0 until (current.size - 1)).map { j =>
        val tile = current(j)
        val right = next(j)
        val bottom = current(j + 1)

        (tile, fixTile(tiles(tile), tiles(right), tiles(bottom)))
      }
    }

    val right = image.map(_(image.size - 1))
    val rightPrev = image.map(_(image.size - 2))
    
    val rightFixed = (0 until (right.size - 1)).map { j =>
      val tile = right(j)
      val left = rightPrev(j)
      val bottom = right(j + 1)

      (tile, fixTile2(tiles(tile), tiles(left), tiles(bottom)))
    }

    val bottom = image(image.size - 1)
    val bottomPrev = image(image.size - 2)

    val bottomFixed = (0 until (bottom.size - 1)).map { j =>
      val tile = bottom(j)
      val right = bottom(j + 1)
      val top = bottomPrev(j)

      (tile, fixTile3(tiles(tile), tiles(right), tiles(top)))
    }

    val last = image.last.last
    val left = image.last.dropRight(1).last
    val top = image.map(_.last).dropRight(1).last

    val fixedLast = (last, fixTile4(tiles(last), tiles(left), tiles(top)))

    ((fixed ++ rightFixed ++ bottomFixed) :+ fixedLast).toMap
  }

  def mkImage(image: Seq[Seq[Tile]]): String =
    image.map { row =>
      row.foldLeft(Seq.fill(row(0).image.size)("")) {
        case (state, tile) => state.zip(tile.image).map { case (a, b) => a + b}
      }.mkString("\n")
    }.mkString("\n")

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

  val index = tiles.map(t => (t.id -> t)).toMap

  val List(left, top, _, _) = searchBorders(matches)
  assert(top.head == left.head)

  val image = build(matches, left, top)

  val fixedTiles = fix(image, index)

  val result = image
    .map(row => row.map(t => fixedTiles.getOrElse(t, index(t))))

  println(mkImage(result))
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

  val tiles = parse(input)
  val index = tiles.map(t => (t.id -> t)).toMap
  val matches = findMatches(tiles)

  assert(part1(matches) == 20899048083289L)

  /*
   * 1951    2311    3079
   * 2729    1427    2473
   * 2971    1489    1171
   */
  val List(left, top, _, _) = searchBorders(matches)

  assert(connectedTile(2729, 2311, matches).contains(1427))
  assert(connectedTile(2971, 1427, matches).contains(1489))

  println(fixTile(index(1951), index(2311), index(2729)).mkString)
  println(fixTile(index(2729), index(1427), index(2971)).mkString)

  val image = build(matches, left, top)

  val fixed = fix(image, index)

  val result = image
    .map(row => row.map(t => fixed.getOrElse(t, index(t))))

  println(mkImage(result))

  println("OK")
}