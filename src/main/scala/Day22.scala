package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day22 {

  type Card = Int

  case class Player(deck: List[Card]) {
    def add(card1: Card, card2: Card) = Player(deck :+ card1 :+ card2)
    def left: Int = deck.size
    def take: (Player, Card) = (Player(deck.tail), deck.head)
    def points: Int = deck.reverse.zipWithIndex.map { case (card, pos) => card * (pos + 1) }.sum
  }

  case class Game(player1: Player, player2: Player) {

    def next: (Game, Card, Card) = {
      val (p1, c1) = player1.take
      val (p2, c2) = player2.take

      (Game(p1, p2), c1, c2)
    }

    def gameover: Boolean = player1.left == 0 || player2.left == 0

    def update(f: (Player, Player) => (Player, Player)): Game = {
      val (p1, p2) = f(player1, player2)
      Game(p1, p2)
    }

    def winner: Option[Player] = 
      if (player1.left == 0)
        Some(player2)
      else if (player2.left == 0) 
        Some(player1)
      else 
        None
  }

  @tailrec
  def play(game: Game): Game = {
    if (game.gameover)
      game
    else {
      val next = game.next match {
        case (next, card1, card2) if card1 > card2 => next.update { case (p1, p2) => (p1.add(card1, card2), p2)}
        case (next, card1, card2) if card1 < card2 => next.update { case (p1, p2) => (p1, p2.add(card2, card1))}
        case (next, card1, card2) if card1 == card2 => throw new IllegalStateException(s"$card1,$card2")
      }
      play(next)
    }
  }
}

object Day22Part1 extends App {
  import Day22._

  val Array(p1, p2) = Source.fromResource("game.txt").mkString.split("\n\n")

  val player1 = Player(p1.split("\n").tail.map(_.toInt).toList)
  val player2 = Player(p2.split("\n").tail.map(_.toInt).toList)

  val result = play(Game(player1, player2))
  println(result.winner.map(_.points))
}

object Day22Test extends App {
  import Day22._

  val p1 = Player(List(9,2,6,3,1))
  val p2 = Player(List(5,8,4,7,10))
  val game = Game(p1, p2)

  val result = play(game)

  assert(result.player2.deck == List(3, 2, 10, 6, 8, 5, 9, 4, 7, 1))

  val winner = result.winner.orNull

  assert(winner.points == 306)

  println("OK")
}