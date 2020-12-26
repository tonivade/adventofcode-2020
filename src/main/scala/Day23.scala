package adventofcode

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object Day23 {

  def circular[A](a: Seq[A]): Stream[A] = {
    val repeat = a.toStream
    def b: Stream[A] = repeat #::: b
    b
  }

  /*
   * Vector will contain on each position the value of the next element
   * 
   * For example: 3, 8, 9, 1, 2, 5, 4, 6, 7
   *  
   * |0|1|2|3|4|5|6|7|8|9|
   * ---------------------
   * |3|2|5|8|6|4|7|3|9|1|
   * 
   * Next step will be: 2, 8, 9, 1, 5, 4, 6, 7, 3
   *  
   * |0|1|2|3|4|5|6|7|8|9|
   * ---------------------
   * |2|5|8|2|6|4|7|3|9|1|
   * 
   */
  case class CircularList(list: Vector[Int]) {

    def toVector: Vector[Int] = take(0, list.size - 1)

    def move: CircularList = {
      val current = list(0)
      val taken = take(current, 3)
      var target = search(current - 1, taken).orElse(search(list.size - 1, taken)).getOrElse(0)

      assert(target > 0, s"invalid target: $target")

      val head = takeWhile(taken.last, target)
      val tail = takeWhile(target, current)
      val result = (head :+ target) ++ taken ++ (tail :+ current)

      CircularList(result:_*)
    }
      
    def takeWhile(from: Int, target: Int): Vector[Int] = {
      val next = list(from)
      if (next != target) {
        next +: takeWhile(next, target) 
      }
      else 
        Vector.empty
    }

    def take(from: Int, n: Int): Vector[Int] = 
      if (n > 0) {
        val current = list(from)
        current +: take(current, n - 1) 
      }
      else 
        Vector.empty

    def search(target: Int, taken: Vector[Int]): Option[Int] =
      if (target < 1)
        None
      else if (taken.contains(target))
          search(target - 1, taken)
        else
          Some(target)

    def label: String = take(1, list.size - 2).mkString
  }

  object CircularList {

    def apply(items: Int*): CircularList = {
      val (res, last) = items.foldLeft((Array.fill(items.size + 1)(0), 0)) {
        case ((array, previous), current) =>
          array(previous) = current
          (array, current)
      }

      // last should point to first to complete the circle
      res(last) = items.head

      CircularList(res.toVector)
    }
  }

  def play(limit: Int)(input: CircularList): CircularList = {
    if (limit % 100 == 0) println(limit)
    if (limit > 0)
      play(limit - 1)(input.move)
    else
      input
  }
}

object Day23Part1 extends App {
  import Day23._

  val result = play(100)(CircularList(9, 6, 2, 7, 1, 3, 8, 5, 4))

  println(result.label)
}

object Day23Test extends App {
  import Day23._

  // 3,2,5,8,6,4,7,3,9,1
  // 3,2,5,8,6,4,7,0,9,1
  val list1 = CircularList(3, 8, 9, 1, 2, 5, 4, 6, 7)

  assert(list1.list == Vector(3, 2, 5, 8, 6, 4, 7, 3, 9, 1))
  assert(list1.toVector == Vector(3, 8, 9, 1, 2, 5, 4, 6, 7))

  assert(list1.move == CircularList(2, 8, 9, 1, 5, 4, 6, 7, 3))
  assert(list1.move.move == CircularList(5, 4, 6, 7, 8, 9, 1, 3, 2))
  assert(list1.move.move.move == CircularList(8, 9, 1, 3, 4, 6, 7, 2, 5))

  val result10 = play(10)(list1)
  assert(result10 == CircularList(8, 3, 7, 4, 1, 9, 2, 6, 5))
  assert(result10.label == "92658374")
  
  val result100 = play(100)(list1)
  assert(result100.label == "67384529")

  //val result10M = play(10000000)(CircularList(1000000)(3, 8, 9, 1, 2, 5, 4, 6, 7))
  //println(result10M)

  println("OK")
}