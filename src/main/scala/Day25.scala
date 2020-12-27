package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day25 {

  val subject = 7
  val magic = 20201227

  def step(x: Long, subject: Long): Long = (x * subject) % magic

  @tailrec
  def loop(n: Long, x: Long = 1)(subject: Long): Long = 
    if (n > 0) 
      loop(n - 1, step(x, subject))(subject)
    else x

  def bruteforce(key: Long)(subject: Long): Option[Long] = {
    Stream.iterate(1L)(step(_, subject))
      .zipWithIndex
      .find(_._1 == key)
      .map(_._2)
  }
}

object Day25Part1 extends App {
  import Day25._

  val Seq(card, door) = Source.fromResource("keys.txt").getLines().map(_.toLong).toSeq

  val loopSizeCard = bruteforce(card)(subject).get

  // 16311885
  println(loop(loopSizeCard)(door))
}

object Day25Test extends App {
  import Day25._

  val doorPubKey = 17807724
  val cardPubKey = 5764801

  val secretKey = 14897079

  val doorLoopSize = bruteforce(doorPubKey)(subject).get
  val cardLoopSize = bruteforce(cardPubKey)(subject).get

  assert(doorLoopSize == 11)
  assert(cardLoopSize == 8)

  assert(loop(cardLoopSize)(subject) == cardPubKey)
  assert(loop(doorLoopSize)(subject) == doorPubKey)

  assert(loop(cardLoopSize)(doorPubKey) == secretKey)
  assert(loop(doorLoopSize)(cardPubKey) == secretKey)

  println("OK")
}