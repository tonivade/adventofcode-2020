package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  sealed trait Cube
  case object Active extends Cube
  case object Inactive extends Cube

  case class Layer(space: Map[(Int, Int), Cube]) {
    val minX: Int = if (space.isEmpty) 0 else space.keySet.map(_._1).min
    val maxX: Int = if (space.isEmpty) 0 else space.keySet.map(_._1).max

    val minY: Int = if (space.isEmpty) 0 else space.keySet.map(_._2).min
    val maxY: Int = if (space.isEmpty) 0 else space.keySet.map(_._2).max

    val all: Seq[(Int, Int)] =
      for {
        y <- minY to maxY
        x <- minX to maxX
      } yield (x, y)

    val width: Int =
      if (space.isEmpty)
        0 
      else
        maxX - minX + 1

    val height: Int =
      if (space.isEmpty)
        0 
      else
        maxY - minY + 1

    def get(x: Int, y: Int): Cube = space.getOrElse((x, y), Inactive)

    def pad: Layer = {
      val padded = all.map { case (x, y) => ((x + 1, y + 1), get(x, y)) }

      val a: ((Int, Int), Cube) = ((0, 0), Inactive)
      val b: ((Int, Int), Cube) = (((maxX + 2), 0), Inactive)
      val c: ((Int, Int), Cube) = ((0, (maxY + 2)), Inactive)
      val d: ((Int, Int), Cube) = (((maxX + 2), (maxY + 2)), Inactive)

      Layer((padded :+ a :+ b :+ c :+ d).toMap)
    }

    def mkString: String =
      if (space.isEmpty)
        "empty"
      else {
        all.map { case (x, y) => get(x, y) }.map {
          case Inactive => '.'
          case Active => '#'
        }.grouped(width).map(_.mkString).mkString("\n")
      }
  }

  object Layer {
    def empty(minX: Int, maxX: Int, minY: Int, maxY: Int): Layer = {
      val a: ((Int, Int), Cube) = ((0, 0), Inactive)
      val b: ((Int, Int), Cube) = ((maxX, 0), Inactive)
      val c: ((Int, Int), Cube) = ((0, maxY), Inactive)
      val d: ((Int, Int), Cube) = ((maxX, maxY), Inactive)

      Layer((a :: b :: c :: d :: Nil).toMap)
    }
  }

  case class Grid(grid: Map[Int, Layer]) {

    val minX = if (grid.isEmpty) 0 else grid.values.map(_.minX).min 
    val maxX = if (grid.isEmpty) 0 else grid.values.map(_.maxX).max
    val minY = if (grid.isEmpty) 0 else grid.values.map(_.minY).min
    val maxY = if (grid.isEmpty) 0 else grid.values.map(_.maxY).max
    val minZ = if (grid.isEmpty) 0 else grid.keySet.min
    val maxZ = if (grid.isEmpty) 0 else grid.keySet.max

    def getLayer(z: Int): Layer = grid.getOrElse(z, Layer.empty(minX, maxX, minY, maxY))

    def get(x: Int, y: Int, z: Int) = getLayer(z).get(x, y)
    
    def neighbors(x: Int, y: Int, z: Int): Int = {
      val xs = List(x - 1, x, x + 1)
      val ys = List(y - 1, y, y + 1)
      val zs = List(z - 1, z, z + 1)

      val all = for { 
        z1 <- zs 
        y1 <- ys 
        x1 <- xs 
        pos = (x1, y1, z1)
        if (pos != (x, y, z)) 
      } yield pos

      assert(all.size == 26, s"size=${all.size}")

      all.map { case (x1, y1, z1) => get(x1, y1, z1) }.count(_ == Active)
    }

    def applyRules: Grid = {
      val layers = for {
        z <- (minZ -1) to (maxZ + 1)
      } yield (z -> applyRules(z).pad)

      Grid(layers.toMap)
    }

    def applyRules(z: Int): Layer = {
      val layerZ = getLayer(z)

      val newLayerZ = layerZ.all.map {
        case (x, y) => {
          val cube = layerZ.get(x, y)
          val hoods = neighbors(x, y, z)
          val newState = (cube, hoods) match {
            case (Active, 2) => Active
            case (Active, 3) => Active
            case (Active, _) => Inactive
            case (Inactive, 3) => Active
            case (Inactive, _) => Inactive
          }

          ((x, y), newState)
        }
      }.toMap

      Layer(newLayerZ)
    }

    def active: Int = grid.flatMap {
      case (_, layer) => layer.space.map {
        case (_, cube) => cube
      }
    }.count(_ == Active)

    def mkString: String =
      (minZ to maxZ).map { z => (s"z=$z" :: getLayer(z).mkString :: Nil).mkString("\n") }.mkString("\n")
  }

  def parseCubes(input: String): Seq[Cube] =
    input.map {
      case '.' => Inactive
      case '#' => Active
    }.toList

  def parseLayer(input: String): Map[(Int, Int), Cube] =
    input.linesIterator.map(parseCubes).zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case (s, x) => ((x, y) -> s)
      }
    }.toMap

  @tailrec
  def step(current: Int)(grid: Grid): Grid = {
    if (current > 0)
      step(current - 1)(grid.applyRules)
    else 
      grid
  }
}

object Day17Part1 extends App {
  import Day17._

  val input = Source.fromResource("space.txt").mkString

  val grid = Grid(Map(0 -> Layer(parseLayer(input)).pad))

  println(grid.mkString)

  println(step(6)(grid).active)
}

object Day17Test extends App {
  import Day17._

  val input = """.#.
                |..#
                |###""".stripMargin

  val layer0 = Layer(parseLayer(input))

  assert(layer0.mkString == input)
  assert(layer0.get(0,0) == Inactive)
  assert(layer0.get(1,0) == Active)
  assert(layer0.get(2,0) == Inactive)
  
  println(layer0.pad.mkString)
  val grid = Grid(Map(0 -> layer0.pad))
  assert(grid.get(1,1,0) == Inactive)
  assert(grid.get(2,1,0) == Active)
  assert(grid.get(3,1,0) == Inactive)

  println("step=1")
  println(grid.applyRules.mkString)
  println("step=2")
  println(grid.applyRules.applyRules.mkString)
  println("step=3")
  println(grid.applyRules.applyRules.applyRules.mkString)
  
  assert(step(6)(grid).active == 112)
}