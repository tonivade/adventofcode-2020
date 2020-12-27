package adventofcode

import scala.annotation.tailrec

object Day25 {

  val subject = 7
  val magic = 20201227

  def transform(x: Int) = ???

  def step(x: Int): Int = (x * subject) % magic

  @tailrec
  def loop(n: Int, x: Int = 1): Int = 
    if (n > 0) 
      loop(n - 1, step(x)) 
    else x
}

object Day25Test extends App {
  import Day25._

  val doorPubKey = 17807724
  val cardPubKey = 5764801

  assert(loop(8) == cardPubKey)
  assert(loop(11) == doorPubKey)

  println(loop(8, doorPubKey))
  println(loop(11, cardPubKey))

  assert(transform(cardPubKey) == transform(doorPubKey))

  println("OK")
}