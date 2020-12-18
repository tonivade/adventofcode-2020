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

    def getCube(x: Int, y: Int): Cube = space.getOrElse((x, y), Inactive)

    def pad: Layer = {
      val padded = all.map { case (x, y) => ((x + 1, y + 1), getCube(x, y)) }

      val a: ((Int, Int), Cube) = ((minX, minY), Inactive)
      val b: ((Int, Int), Cube) = (((maxX + 2), minY), Inactive)
      val c: ((Int, Int), Cube) = ((minX, (maxY + 2)), Inactive)
      val d: ((Int, Int), Cube) = (((maxX + 2), (maxY + 2)), Inactive)

      Layer((padded :+ a :+ b :+ c :+ d).toMap)
    }

    def mkString: String =
      if (space.isEmpty)
        "empty"
      else {
        all.map { case (x, y) => getCube(x, y) }.map {
          case Inactive => '.'
          case Active => '#'
        }.grouped(width).map(_.mkString).mkString("\n")
      }
  }

  object Layer {
    def empty(minX: Int, maxX: Int, minY: Int, maxY: Int): Layer = {
      val a: ((Int, Int), Cube) = ((minX, minY), Inactive)
      val b: ((Int, Int), Cube) = ((maxX, minY), Inactive)
      val c: ((Int, Int), Cube) = ((minX, maxY), Inactive)
      val d: ((Int, Int), Cube) = ((maxX, maxY), Inactive)

      Layer((a :: b :: c :: d :: Nil).toMap)
    }
  }

  case class Grid(space: Map[Int, Layer]) {

    val minX = if (space.isEmpty) 0 else space.values.map(_.minX).min 
    val maxX = if (space.isEmpty) 0 else space.values.map(_.maxX).max
    val minY = if (space.isEmpty) 0 else space.values.map(_.minY).min
    val maxY = if (space.isEmpty) 0 else space.values.map(_.maxY).max
    val minZ = if (space.isEmpty) 0 else space.keySet.min
    val maxZ = if (space.isEmpty) 0 else space.keySet.max

    def getLayer(z: Int): Layer = space.getOrElse(z, Layer.empty(minX, maxX, minY, maxY))

    def getCube(x: Int, y: Int, z: Int): Cube = getLayer(z).getCube(x, y)
    
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

      all.map { case (x1, y1, z1) => getCube(x1, y1, z1) }.count(_ == Active)
    }

    def pad: Grid = {
      val paddedLayers = space.map { case (z, layer) => (z, layer.pad) }

      val front = Layer.empty(minX, maxX, minY, maxY).pad
      val back = Layer.empty(minX, maxX, minY, maxY).pad

      new Grid(paddedLayers + ((minZ - 1) -> back) + ((maxZ + 1) -> front))
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
          val cube = layerZ.getCube(x, y)
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

    def active: Int = space.flatMap {
      case (_, layer) => layer.space.map {
        case (_, cube) => cube
      }
    }.count(_ == Active)

    def mkString: String =
      (minZ to maxZ).map { z => (s"z=$z" :: getLayer(z).mkString :: Nil).mkString("\n") }.mkString("\n")
  }

  object Grid {
    def empty(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int): Grid = {
      val layer = Layer.empty(minY, maxX, minY, maxY)

      Grid(Map(minZ -> layer, maxZ -> layer))
    }
  }

  case class HyperGrid(space: Map[Int, Grid]) {

    val minX = if (space.isEmpty) 0 else space.values.map(_.minX).min 
    val maxX = if (space.isEmpty) 0 else space.values.map(_.maxX).max
    val minY = if (space.isEmpty) 0 else space.values.map(_.minY).min
    val maxY = if (space.isEmpty) 0 else space.values.map(_.maxY).max
    val minZ = if (space.isEmpty) 0 else space.values.map(_.minZ).max
    val maxZ = if (space.isEmpty) 0 else space.values.map(_.maxZ).max
    val minW = if (space.isEmpty) 0 else space.keySet.min
    val maxW = if (space.isEmpty) 0 else space.keySet.max

    def getGrid(w: Int): Grid = space.getOrElse(w, Grid.empty(minX, maxX, minY, maxY, minZ, maxZ))

    def getLayer(z: Int, w: Int): Layer = getGrid(w).getLayer(z)

    def getCube(x: Int, y: Int, z: Int, w: Int): Cube = getLayer(z, w).getCube(x, y)
    
    def neighbors(x: Int, y: Int, z: Int, w: Int): Int = {
      val xs = List(x - 1, x, x + 1)
      val ys = List(y - 1, y, y + 1)
      val zs = List(z - 1, z, z + 1)
      val ws = List(w - 1, w, w + 1)

      val all = for { 
        w1 <- ws 
        z1 <- zs 
        y1 <- ys 
        x1 <- xs 
        pos = (x1, y1, z1, w1)
        if (pos != (x, y, z, w)) 
      } yield pos

      assert(all.size == 80, s"size=${all.size}")

      all.map { case (x1, y1, z1, w1) => getCube(x1, y1, z1, w1) }.count(_ == Active)
    }

    def applyRules: HyperGrid = {
      val grids = for {
        w <- (minW -1) to (maxW + 1)
      } yield (w -> applyRules(w).pad)

      HyperGrid(grids.toMap)
    }

    def applyRules(w: Int): Grid = {
      val gridW = getGrid(w)

      val newGridW = ((minZ - 1) to (maxZ + 1)).map { z =>
        val layerZ = gridW.getLayer(z)
        val newLayerZ = gridW.getLayer(z).all.map {
          case (x, y) => {
            val cube = layerZ.getCube(x, y)
            val hoods = neighbors(x, y, z, w)
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


        (z -> Layer(newLayerZ))
      }.toMap

      Grid(newGridW)
    }

    def active: Int = space.flatMap {
      case (_, grid) => grid.space.flatMap {
        case (_, layer) => layer.space.map {
          case (_, cube) => cube
        }
      }
    }.count(_ == Active)

    def mkString: String = {
      val layers = for {
        w <- minW to maxW
        z <- minZ to maxZ
      } yield ((z, w), getLayer(z, w))


      layers.map {
        case ((z, w), layer) => s"w=$w,z=$z\n${layer.mkString}"
      }.mkString("\n")
    }
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

  @tailrec
  def step2(current: Int)(grid: HyperGrid): HyperGrid = {
    if (current > 0)
      step2(current - 1)(grid.applyRules)
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

object Day17Part2 extends App {
  import Day17._

  val input = Source.fromResource("space.txt").mkString

  val grid = Grid(Map(0 -> Layer(parseLayer(input)).pad))
  val hypergrid = HyperGrid(Map(0 -> grid))

  println(grid.mkString)

  println(step2(6)(hypergrid).active)
}

object Day17Test extends App {
  import Day17._

  val input = """.#.
                |..#
                |###""".stripMargin

  val layer0 = Layer(parseLayer(input))

  assert(layer0.mkString == input)
  assert(layer0.getCube(0,0) == Inactive)
  assert(layer0.getCube(1,0) == Active)
  assert(layer0.getCube(2,0) == Inactive)
  
  val grid = Grid(Map(0 -> layer0.pad))
  assert(grid.getCube(1,1,0) == Inactive)
  assert(grid.getCube(2,1,0) == Active)
  assert(grid.getCube(3,1,0) == Inactive)
  assert(step(6)(grid).active == 112)

  val hypergrid = HyperGrid(Map(0 -> grid))
  assert(step2(6)(hypergrid).active == 848)
}

object Day17Test1 extends App {
  import Day17._

  val input = """.#.
                |..#
                |###""".stripMargin

  val layer0 = Layer(parseLayer(input))
  
  val grid = Grid(Map(0 -> layer0.pad))

  println("step=1")
  println(grid.applyRules.mkString)
  println("step=2")
  println(grid.applyRules.applyRules.mkString)
  println("step=3")
  println(grid.applyRules.applyRules.applyRules.mkString)
}

object Day17Test2 extends App {
  import Day17._

  val input = """.#.
                |..#
                |###""".stripMargin

  val layer0 = Layer(parseLayer(input))
  val grid = Grid(Map(0 -> layer0.pad))
  val hypergrid = HyperGrid(Map(0 -> grid))

  println("step=1")
  println(hypergrid.applyRules.mkString)
  println("step=2")
  println(hypergrid.applyRules.applyRules.mkString)
  println("step=3")
  println(hypergrid.applyRules.applyRules.applyRules.mkString)
}