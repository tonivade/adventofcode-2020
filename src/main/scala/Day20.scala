package adventofcode

import scala.io.Source

object Day20 {

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
    def positives: Int = List(top,botton,left,right).flatMap(_.toList).size
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

  def findCorners(input: String): Seq[Int] = {
    val tiles = parse(input)
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
      .map(x => (x._1, x._2.maxBy(_.positives)))
      .filter(_._2.positives == 2)
      .map(_._1)
      .toSeq
  }
}

object Day20Part1 extends App {
  import Day20._

  val input = Source.fromResource("tiles.txt").mkString

  val corners = findCorners(input)

  println(corners)
  
  println(corners.foldLeft(1L)(_ * _))
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

  val corners = findCorners(input)

  assert(corners.size == 4)

  assert(corners.foldLeft(1L)(_ * _) == 20899048083289L)

  println("OK")
}