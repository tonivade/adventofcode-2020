package adventofcode

import scala.io.Source
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

    def noBorders: Tile = Tile(id, crop(image))

    def all: Seq[Tile] = 
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

    def count: Int = image.mkString.count(_ == '#')

    def mkString: String = image.mkString("\n")

    private def rotate(input: Seq[String]): Seq[String] =
      for {
        i <- 0 until input(0).size
      } yield (input.map(_(i)).mkString)

    private def invert(input: Seq[String]): Seq[String] =
      for {
        i <- (input.size - 1) to 0 by -1
      } yield (input(i))

    private def crop(input: Seq[String]): Seq[String] =
      input.drop(1).dropRight(1)
        .map(_.drop(1).dropRight(1))
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
  
  def fixTile(current: Tile, t1: Tile, t2: Tile)(predicate: (Tile, Tile, Tile) => Boolean): Tile = {
    val combinations = for {
      c <- current.all
      a <- t1.all
      b <- t2.all
    } yield(c, a, b)

    val result = combinations.filter {
      case (c, a, b) => predicate(c, a, b)
    }

    result match {
      case (tile, _, _) :: Nil => tile
      case _ => throw new IllegalStateException(s"${current.id}")
    }
  }
  
  def fixTileLeftTop(current: Tile, left: Tile, top: Tile): Tile =
    fixTile(current, left, top) {
      case (c, l, t) => c.left == l.right && c.top == t.bottom
    }

  def fixTileRightTop(current: Tile, right: Tile, top: Tile): Tile =
    fixTile(current, right, top) {
      case (c, r, t) => c.right == r.left && c.top == t.bottom
    }

  def fixTileLeftBottom(current: Tile, left: Tile, bottom: Tile): Tile =
    fixTile(current, left, bottom) {
      case (c, l, b) => c.left == l.right && c.bottom == b.top
    }

  def fixTileRightBottom(current: Tile, right: Tile, bottom: Tile): Tile =
    fixTile(current, right, bottom) {
      case (c, r, b) => c.right == r.left && c.bottom == b.top
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
          .filter(borders.contains)
          .filterNot(path.contains)

        if (next.size == 1)
          loop(next.head, to, path :+ current)
        else Nil
      }
    }
      
    val input = matches(from)
    val output = input.matches.filter(borders.contains)

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

  def buildMatches(matches: Map[Int, Result], left: List[Int], top: List[Int]): List[List[Int]] = {
    def column(top: Int, prev: List[Int]): List[Int] =
      prev.tail.foldLeft(top :: Nil) {
        case (state, current) =>
          state :+ connectedTile(state.last, current, matches).find(!prev.contains(_)).get
      }

    (1 until top.size).foldLeft(left :: Nil) {
      case (state, current) => state :+ column(top(current), state.last)
    }
  }

  def fixTiles(image: Seq[List[Int]], tiles: Map[Int, Tile]): Map[Int, Tile] = {
    // fixes all the tiles, except the last column and row
    def fixed: Seq[(Int, Tile)] = {
      (0 until image(0).size - 1).flatMap { i =>
        val current = image.map(_(i))
        val next = image.map(_(i + 1))

        (0 until current.size - 1).map { j =>
          val tile = current(j)
          val right = next(j)
          val bottom = current(j + 1)

          (tile, fixTileRightBottom(tiles(tile), tiles(right), tiles(bottom)))
        }
      }
    }

    // fixes the last column except the last tile
    def rightFixed: Seq[(Int, Tile)] = {
      val right = image.map(_(image.size - 1))
      val rightPrev = image.map(_(image.size - 2))
      
      (0 until right.size - 1).map { j =>
        val tile = right(j)
        val left = rightPrev(j)
        val bottom = right(j + 1)

        (tile, fixTileLeftBottom(tiles(tile), tiles(left), tiles(bottom)))
      }
    }

    // fixes the last row except the last tile
    def bottomFixed: Seq[(Int, Tile)] = {
      val bottom = image(image.size - 1)
      val bottomPrev = image(image.size - 2)

      (0 until bottom.size - 1).map { j =>
        val tile = bottom(j)
        val right = bottom(j + 1)
        val top = bottomPrev(j)

        (tile, fixTileRightTop(tiles(tile), tiles(right), tiles(top)))
      }
    }

    // fixes the last tile
    def fixedLast: (Int, Tile) = {
      val last = image.last.last
      val left = image.last.dropRight(1).last
      val top = image.map(_.last).dropRight(1).last

      (last, fixTileLeftTop(tiles(last), tiles(left), tiles(top)))
    }

    ((fixed ++ rightFixed ++ bottomFixed) :+ fixedLast).toMap
  }

  def mkImage(image: Seq[Seq[Tile]]): String =
    image.map { row =>
      row.foldLeft(Seq.fill(row(0).image.size)("")) {
        case (state, tile) => state.zip(tile.image).map { case (a, b) => a + b}
      }.mkString("\n")
    }.mkString("\n")

  def views(dimension: (Int, Int), image: String): Seq[String] = {
    val lines = image.linesIterator.toList

    val width = lines.map(_.size).head
    val height = lines.size

    val allSlices = (0 until width - dimension._2)
      .map(w => lines.slice(w, w + dimension._2))
      .flatMap(
        slice => (0 until height - dimension._1).map(
          h => slice.map(_.slice(h, h + dimension._1))))

    allSlices.map(_.mkString("\n"))
  }

  def comparePattern(pattern: String, slice: String): Boolean = {
    val flatPattern = pattern.linesIterator.mkString
    val flatSlice = slice.linesIterator.mkString

    val flatResult = flatPattern.zip(flatSlice).map {
      case (' ', _) => ' '
      case ('#', '#') => '#'
      case ('#', c) => c
      case (a, b) => throw new IllegalStateException(s"$a,$b")
    }.mkString

    flatPattern == flatResult
  }

  def searchPattern(pattern: String, tile: Tile): Int = {
    val width = pattern.linesIterator.map(_.size).toList.head
    val height = pattern.linesIterator.size

    val allViews = views((width, height), tile.mkString)

    allViews.filter(comparePattern(pattern, _)).size
  }

  def countPattern(pattern: String, image: Tile): Int = {
    // should be only one with monsters
    val monsters = image.all.map(searchPattern(pattern, _)).find(_ > 0).get

    pattern.count(_ == '#') * monsters
  }

  def buildImage(tiles: Seq[Tile], matches: Map[Int, Result]): Tile = {
    val index = tiles.map(t => (t.id -> t)).toMap

    val List(left, top, _, _) = searchBorders(matches)
    assert(top.head == left.head)

    val image = buildMatches(matches, left, top)

    val fixedTiles = fixTiles(image, index)

    val result = image.map(_.map(fixedTiles)).map(_.map(_.noBorders)) |> mkImage

    Tile(0, result.linesIterator.toSeq)
  }

  val input = Source.fromResource("day20.txt").mkString

  val pattern = """                  # 
                  |#    ##    ##    ###
                  | #  #  #  #  #  #   """.stripMargin
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
  val image = buildImage(tiles, matches)

  println(image.count - countPattern(pattern, image))
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

  println(fixTileLeftBottom(index(1951), index(2311), index(2729)).mkString)
  println(fixTileLeftBottom(index(2729), index(1427), index(2971)).mkString)

  val image = buildMatches(matches, left, top)

  val fixed = fixTiles(image, index)

  val result = image.map(_.map(fixed)).map(_.map(_.noBorders))
  
  val string = mkImage(result)

  assert(string.linesIterator.size == 24)
  assert(string.linesIterator.map(_.size).toList.head == 24)

  val images = Tile(0, string.linesIterator.toSeq)
  val monsters = images.all.map(searchPattern(pattern, _)).find(_ > 0).get

  assert(monsters == 2)
  
  val a = pattern.count(_ == '#') * monsters
  val b = string.count(_ == '#')

  assert(b - a == 273)

  println("OK")
}