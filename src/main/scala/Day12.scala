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

  sealed trait Action
  case class MoveAction(op: Move, value: Int) extends Action
  case class DirectionAction(op: Direction, value: Int) extends Action

  case class Position(x: Int, y: Int) {
    lazy val distance = Math.abs(x) + Math.abs(y)

    def north(v: Int): Position = Position(x, y + v)
    def south(v: Int): Position = Position(x, y - v)
    def east(v: Int): Position = Position(x + v, y)
    def west(v: Int): Position = Position(x - v, y)

    def move(waypoint: Position, value: Int) = 
      Position(x + (waypoint.x * value), y + (waypoint.y * value))

    def right(degree: Int): Position = 
      degree match {
        case 90 => Position(y, -x)
        case 180 => Position(-x, -y)
        case 270 => Position(-y, -x)
      }
      
    def left(degree: Int): Position = 
      degree match {
        case 90 => Position(-y, -x)
        case 180 => Position(-x, -y)
        case 270 => Position(y, -x)
      }
  }

  def parseLine(line: String): Action =
    line.splitAt(1) match {
      case ("N", x) => DirectionAction(North, x.toInt)
      case ("S", x) => DirectionAction(South, x.toInt)
      case ("E", x) => DirectionAction(East, x.toInt)
      case ("W", x) => DirectionAction(West, x.toInt)
      case ("F", x) => MoveAction(Forward, x.toInt)
      case ("L", x) => MoveAction(Left, x.toInt)
      case ("R", x) => MoveAction(Right, x.toInt)
      case _ => throw new IllegalArgumentException(s"invalid format: $line")
    }
    
  val initial: (Direction, Position) = (East, Position(0, 0))
  val initial2: (Direction, Position, Position) = (East, Position(0, 0), Position(10, 1))

  def seal(input: List[Action]): (Direction, Position) =
    input.foldLeft(initial)(step)
  
  def step(input: (Direction, Position), action: Action): (Direction, Position) =
    (input, action) match {
      case ((dir, current), DirectionAction(North, x)) => (dir, current.north(x))
      case ((dir, current), DirectionAction(South, x)) => (dir, current.south(x))
      case ((dir, current), DirectionAction(East, x)) => (dir, current.east(x))
      case ((dir, current), DirectionAction(West, x)) => (dir, current.west(x))

      case ((North, current), MoveAction(Forward, x)) => (North, current.north(x))
      case ((South, current), MoveAction(Forward, x)) => (South, current.south(x))
      case ((East, current), MoveAction(Forward, x)) => (East, current.east(x))
      case ((West, current), MoveAction(Forward, x)) => (West, current.west(x))

      case ((dir, current), MoveAction(Right, x)) => (dir.right(x), current)
      case ((dir, current), MoveAction(Left, x)) => (dir.left(x), current)
    }

  def seal2(input: List[Action]): (Direction, Position, Position) =
    input.foldLeft(initial2)(step2)
  
  def step2(input: (Direction, Position, Position), action: Action): (Direction, Position, Position) =
    (input, action) match {
      case ((dir, current, waypoint), DirectionAction(North, x)) => (dir, current, waypoint.north(x))
      case ((dir, current, waypoint), DirectionAction(South, x)) => (dir, current, waypoint.south(x))
      case ((dir, current, waypoint), DirectionAction(East, x)) => (dir, current, waypoint.east(x))
      case ((dir, current, waypoint), DirectionAction(West, x)) => (dir, current, waypoint.west(x))

      case ((dir, current, waypoint), MoveAction(Forward, x)) => (dir, current.move(waypoint, x), waypoint)

      case ((dir, current, waypoint), MoveAction(Right, x)) => (dir, current, waypoint.right(x))
      case ((dir, current, waypoint), MoveAction(Left, x)) => (dir, current, waypoint.left(x))
    }

  val input = Source.fromResource("navigation.txt").getLines().map(parseLine).toList
}

object Day12Part1 extends App {
  import Day12._

  println("Day12 Part1")

  println(seal(input)._2.distance)
}

object Day12Part2 extends App {
  import Day12._

  println("Day12 Part2")

  println(seal2(input)._2.distance)
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
  assert(seal2(nav)._2.distance == 286)

  assert(Position(10, 4).right(90) == Position(4, -10))
  assert(Position(10, 4).right(180) == Position(-10, -4))
  assert(Position(10, 4).right(270) == Position(-4, -10))

  assert(Position(10, 4).left(270) == Position(4, -10))
  assert(Position(10, 4).left(180) == Position(-10, -4))
  assert(Position(10, 4).left(90) == Position(-4, -10))

  println("OK")
}