package adventofcode

import scala.io.Source

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
  }

  sealed trait Tile
  case object Black extends Tile
  case object White extends Tile

  /*
   * Using cube coordinates, https://www.redblobgames.com/grids/hexagons/
   */ 
  case class HexaGrid(grid: Map[Position, Tile]) {
    
    def update(position: Position): HexaGrid = {
      val updated = grid.getOrElse(position, White) match {
        case Black => grid.updated(position, White)
        case White => grid.updated(position, Black)
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
}

object Day24Part1 extends App {
  import Day24._

  val input = Source.fromResource("hexagon.txt").mkString

  val result = perform(parseAll(input))

  println(result.black)
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

  println("OK")
}