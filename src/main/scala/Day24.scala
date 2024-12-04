package adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day24 {

  /*
   * NW  /\  NE
   *    /  \
   *   |    |
   * W |    | E
   *    \  /
   * SW  \/  SE
   */
  case class Position(x: Int, y: Int, z: Int) {
    def move(motion: Motion): Position = 
      motion match {
        case (dx, dy, dz) => Position(x + dx, y + dy, z + dz)
      }
  }

  object Position {
    val zero = Position(0, 0, 0)
  }

  type Motion = (Int, Int, Int)

  /* 
   * Cube Coordinates
   * 
   * x=0,y=0,z=0 center
   * 
   *     /\   /\
   *    /  \ /  \
   *   |0 -1|1 -1|
   *   |    |    |
   *  / \1  /\0 / \
   * /   \ /  \/   \
   * |-1 0|x  y|1 0|
   * |    |    |   |
   *  \1 / \z /\-1/
   *   \/   \/  \/
   *   |-1 1|0  1|
   *   |    |    |
   *    \0 / \-1/
   *     \/   \/
   */
  object Motion {
    val NW = (0, -1, 1)
    val NE = (1, -1, 0)
    val E = (1, 0, -1)
    val W = (-1, 0, 1)
    val SE = (0, 1, -1)
    val SW = (-1, 1, 0)

    val ALL = Seq(NW, NE, E, W, SE, SW)
  }

  sealed trait Tile
  case object Black extends Tile
  case object White extends Tile

  /*
   * Using cube coordinates, https://www.redblobgames.com/grids/hexagons/
   */ 
  case class HexaGrid(grid: Map[Position, Tile]) {

    def all: Seq[Position] = {

      val keys = grid.keySet

      val maxX = keys.map(_.x).max
      val minX = keys.map(_.x).min
      val maxY = keys.map(_.y).max
      val minY = keys.map(_.y).min
      val maxZ = keys.map(_.z).max
      val minZ = keys.map(_.z).min

      val all = for {
        z <- (minZ - 1) to (maxZ + 1)
        y <- (minY - 1) to (maxY + 1)
        x <- (minX - 1) to (maxX + 1)
      } yield Position(x, y, z)

      all
    }

    def get(position: Position): Tile = grid.getOrElse(position, White)

    def adjacent(position: Position): Seq[Position] =
      Motion.ALL.map(position.move)
        .filter(get(_) == Black)
    
    def update(position: Position): HexaGrid = {
      val updated = get(position) match {
        case Black => grid + (position -> White)
        case White => grid + (position -> Black)
      }
      HexaGrid(updated)
    }

    def black: Int = grid.count { case (_, t) => t == Black }
  }

  object HexaGrid {
    val empty = HexaGrid(Map.empty)
  }

  def flip(motions: Seq[Motion], grid: HexaGrid): HexaGrid = {
    val last = motions.foldLeft(Position.zero) {
      case (current, motion) => current.move(motion)
    }

    grid.update(last)
  }

  def parseLine(line: String): Seq[Motion] = {

    val (motions, _) = line.foldLeft((Seq.empty[Motion], ' ')) {
      case ((motions, 's'), 'e') => (motions :+ Motion.SE, ' ')
      case ((motions, 's'), 'w') => (motions :+ Motion.SW, ' ')
      case ((motions, 'n'), 'e') => (motions :+ Motion.NE, ' ')
      case ((motions, 'n'), 'w') => (motions :+ Motion.NW, ' ')
      case ((motions, ' '), 'e') => (motions :+ Motion.E, ' ')
      case ((motions, ' '), 'w') => (motions :+ Motion.W, ' ')
      case ((motions, ' '), c) => (motions, c)
      case ((motions, p), c) => throw new IllegalArgumentException(s"invalid p=$p,c=$c")
    }

    motions
  }

  def parseAll(input: String): Seq[Seq[Motion]] =
    input.linesIterator.map(parseLine).toSeq

  def perform(all: Seq[Seq[Motion]]): HexaGrid =
    all.foldLeft(HexaGrid.empty) {
      case (grid, motions) => flip(motions, grid)
    }

  def step(grid: HexaGrid): HexaGrid = {
    val all = grid.all

    val result = all.map { p => 
      val tile = grid.get(p)
      val adjacent = grid.adjacent(p)

      (tile, adjacent.map(grid.get).count(_ == Black)) match {
        case (Black, 0) => (p -> White)
        case (Black, x) if x > 2 => (p -> White)
        case (White, 2) => (p -> Black)
        case (t, _) => (p -> t)
      }
    }

    HexaGrid(result.toMap)
  }

  @tailrec
  def process(limit: Int)(initial: HexaGrid): HexaGrid = {
    println(s"step:$limit,black=${initial.black}")
    if (limit > 0)
      process(limit - 1)(step(initial))
    else
      initial
  }
}

object Day24Part1 extends App {
  import Day24._

  val input = Source.fromResource("day24.txt").mkString

  val result = perform(parseAll(input))

  println(result.black)
  println(process(100)(result).black)
}

object Day24Test extends App {
  import Day24._
  import Motion._

  val motions = parseLine("sesenwnenee")

  assert(motions == Seq(SE, SE, NW, NE, NE, E))

  val result = flip(motions, HexaGrid.empty)

  assert(result.black == 1)

  val input1 = """sesenwnenenewseeswwswswwnenewsewsw
                |neeenesenwnwwswnenewnwwsewnenwseswesw
                |seswneswswsenwwnwse
                |nwnwneseeswswnenewneswwnewseswneseene
                |swweswneswnenwsewnwneneseenw
                |eesenwseswswnenwswnwnwsewwnwsene
                |sewnenenenesenwsewnenwwwse
                |wenwwweseeeweswwwnwwe
                |wsweesenenewnwwnwsenewsenwwsesesenwne
                |neeswseenwwswnwswswnw
                |nenwswwsewswnenenewsenwsenwnesesenew
                |enewnwewneswsewnwswenweswnenwsenwsw
                |sweneswneswneneenwnewenewwneswswnese
                |swwesenesewenwneswnwwneseswwne
                |enesenwswwswneneswsenwnewswseenwsese
                |wnwnesenesenenwwnenwsewesewsesesew
                |nenewswnwewswnenesenwnesewesw
                |eneswnwswnwsenenwnwnwwseeswneewsenese
                |neswnwewnwnwseenwseesewsenwsweewe
                |wseweeenwnesenwwwswnew""".stripMargin

  val result1 = perform(parseAll(input1))

  assert(result1.black == 10)

  assert(process(1)(result1).black == 15)
  assert(process(2)(result1).black == 12)
//  assert(process(100)(result1).black == 2208)

  println("OK")
}