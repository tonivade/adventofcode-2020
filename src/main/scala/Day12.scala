package adventofcode

import scala.io.Source

object Day12 {

  sealed trait Direction {
    def left(degree: Int): Direction
    def right(degree: Int): Direction
  }

  case object North extends Direction {
    override def right(degree: Int): Direction = 
      degree match {
        case 90 => East
        case 180 => South
        case 270 => West
      }

    override def left(degree: Int): Direction = 
      degree match {
        case 90 => West
        case 180 => South
        case 270 => East
      }
  }

  case object South extends Direction {
    override def right(degree: Int): Direction = 
      degree match {
        case 90 => West
        case 180 => North
        case 270 => East
      }

    override def left(degree: Int): Direction = 
      degree match {
        case 90 => East
        case 180 => North
        case 270 => West
      }
  }
  case object East extends Direction {
    override def right(degree: Int): Direction = 
      degree match {
        case 90 => South
        case 180 => West
        case 270 => North
      }

    override def left(degree: Int): Direction = 
      degree match {
        case 90 => North
        case 180 => West
        case 270 => South
      }
  }
  case object West extends Direction {
    override def right(degree: Int): Direction = 
      degree match {
        case 90 => North
        case 180 => East
        case 270 => South
      }

    override def left(degree: Int): Direction = 
      degree match {
        case 90 => South
        case 180 => East
        case 270 => North
      }
  }

  sealed trait Move
  case object Forward extends Move
  case object Left extends Move
  case object Right extends Move

  sealed trait Nav
  case class MoveNav(op: Move, value: Int) extends Nav
  case class DirectionNav(op: Direction, value: Int) extends Nav

  case class Position(x: Int, y: Int) {
    lazy val distance = Math.abs(x) + Math.abs(y)

    def north(v: Int): Position = Position(x, y + v)
    def south(v: Int): Position = Position(x, y - v)
    def east(v: Int): Position = Position(x + v, y)
    def west(v: Int): Position = Position(x - v, y)
  }

  def parseLine(line: String): Nav =
    line.splitAt(1) match {
      case ("N", x) => DirectionNav(North, x.toInt)
      case ("S", x) => DirectionNav(South, x.toInt)
      case ("E", x) => DirectionNav(East, x.toInt)
      case ("W", x) => DirectionNav(West, x.toInt)
      case ("F", x) => MoveNav(Forward, x.toInt)
      case ("L", x) => MoveNav(Left, x.toInt)
      case ("R", x) => MoveNav(Right, x.toInt)
      case _ => throw new IllegalArgumentException(s"invalid format: $line")
    }
    
  val initial: (Direction, Position) = (East, Position(0, 0))

  def seal(input: List[Nav]): (Direction, Position) =
    input.foldLeft(initial) {
      case ((dir, current), DirectionNav(North, x)) => (dir, current.north(x))
      case ((dir, current), DirectionNav(South, x)) => (dir, current.south(x))
      case ((dir, current), DirectionNav(East, x)) => (dir, current.east(x))
      case ((dir, current), DirectionNav(West, x)) => (dir, current.west(x))

      case ((North, current), MoveNav(Forward, x)) => (North, current.north(x))
      case ((South, current), MoveNav(Forward, x)) => (South, current.south(x))
      case ((East, current), MoveNav(Forward, x)) => (East, current.east(x))
      case ((West, current), MoveNav(Forward, x)) => (West, current.west(x))

      case ((dir, current), MoveNav(Right, x)) => (dir.right(x), current)
      case ((dir, current), MoveNav(Left, x)) => (dir.left(x), current)
      
      case e => throw new IllegalArgumentException(s"invalid format: $e")
    }
}

object Day12Part1 extends App {
  import Day12._

  println("Day12 Part1")

  val input = Source.fromResource("navigation.txt").getLines().map(parseLine).toList

  println(seal(input)._2.distance)
}

object Day12Test extends App {

  import Day12._

  val input1 = """F10
                 |N3
                 |F7
                 |R90
                 |F11""".stripMargin

  val nav = input1.linesIterator.map(parseLine).toList

  assert(seal(nav)._2.distance == 25)

  println("OK")
}