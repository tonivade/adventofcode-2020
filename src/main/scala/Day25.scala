package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day25 {

  val magic = 20201227L

  def step(x: Long, subject: Long): Long = (x * subject) % magic

  @tailrec
  def loop(n: Long, x: Long = 1)(subject: Long): Long = 
    if (n > 0) 
      loop(n - 1, step(x, subject))(subject)
    else x

  def bruteforce(key: Long): Option[Long] = {
    Stream.iterate(1L)(step(_, 7))
      .zipWithIndex
      .find(_._1 == key)
      .map(_._2)
  }
}

object Day25Part1 extends App {
  import Day25._

  val Seq(card, door) = Source.fromResource("keys.txt").getLines().map(_.toLong).toSeq

  val loopSizeCard = bruteforce(card).get

  // 16311885
  println(loop(loopSizeCard)(door))
}

object Day25Test extends App {
  import Day25._
  
  val subject = 7

  val doorPubKey = 17807724L
  val cardPubKey = 5764801L

  val secretKey = 14897079

  val doorLoopSize = bruteforce(doorPubKey).get
  val cardLoopSize = bruteforce(cardPubKey).get

  assert(doorLoopSize == 11)
  assert(cardLoopSize == 8)

  assert(loop(cardLoopSize)(subject) == cardPubKey)
  assert(loop(doorLoopSize)(subject) == doorPubKey)

  assert(loop(cardLoopSize)(doorPubKey) == secretKey)
  assert(loop(doorLoopSize)(cardPubKey) == secretKey)

  println("OK")
}