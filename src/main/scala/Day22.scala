package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day22 {

  type Card = Int

  case class Player(deck: List[Card]) {
    def add(card1: Card, card2: Card) = Player(deck :+ card1 :+ card2)
    def left: Int = deck.size
    def looser: Boolean = left == 0
    def take: (Player, Card) = (Player(deck.tail), deck.head)
    def points: Int = deck.reverse.zipWithIndex.map { case (card, pos) => card * (pos + 1) }.sum
    def rescursive: Boolean = deck.tail.size >= deck.head
  }

  case class Game(player1: Player, player2: Player) {

    def next: (Game, Card, Card) = {
      val (p1, c1) = player1.take
      val (p2, c2) = player2.take

      (Game(p1, p2), c1, c2)
    }

    def nextRecursive: Game = {
      val (p1, c1) = player1.take
      val (p2, c2) = player2.take

      val deck1 = p1.deck.take(c1)
      val deck2 = p2.deck.take(c2)

      Game(Player(deck1), Player(deck2))
    }

    def gameover: Boolean = winner1 || winner2

    def recursive: Boolean = 
      player1.rescursive && player2.rescursive

    def winner1: Boolean = player2.looser

    def winner2: Boolean = player1.looser

    def updateP1(card1: Card, card2: Card): Game = 
      update((p1, p2) => (p1.add(card1, card2), p2))

    def updateP2(card1: Card, card2: Card): Game = 
      update((p1, p2) => (p1, p2.add(card1, card2)))

    def update(f: (Player, Player) => (Player, Player)): Game = {
      val (p1, p2) = f(player1, player2)
      Game(p1, p2)
    }

    def break: Game = Game(player1, Player(List.empty))

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
        case (next, card1, card2) if card1 > card2 => next.updateP1(card1, card2)
        case (next, card1, card2) if card1 < card2 => next.updateP2(card2, card1)
        case (next, card1, card2) if card1 == card2 => throw new IllegalStateException(s"$card1,$card2")
      }
      play(next)
    }
  }

  type State = (Set[Game], Game)

  def play2(state: State): Game = 
    state match {
      case (played, game) =>
        if (game.gameover)
          game
        else if (played.contains(game))
          game.break
        else if (game.recursive) {
          val result = play2(played, game.nextRecursive)
          
          val next = game.next match {
            case (next, card1, card2) if result.winner1 => next.updateP1(card1, card2)
            case (next, card1, card2) if result.winner2 => next.updateP2(card2, card1)
            case (next, card1, card2) => throw new IllegalStateException(s"$card1,$card2")
          }

          play2(played + game, next)
        } else {
          val next = game.next match {
            case (next, card1, card2) if card1 > card2 => next.updateP1(card1, card2)
            case (next, card1, card2) if card1 < card2 => next.updateP2(card2, card1)
            case (next, card1, card2) => throw new IllegalStateException(s"$card1,$card2")
          }
          play2(played + game, next)
        }
    }
}

object Day22Part1 extends App {
  import Day22._

  val Array(p1, p2) = Source.fromResource("day22.txt").mkString.split("\n\n")

  val player1 = Player(p1.split("\n").tail.map(_.toInt).toList)
  val player2 = Player(p2.split("\n").tail.map(_.toInt).toList)

  val result = play(Game(player1, player2))
  println(result.winner.map(_.points))
  val result2 = play2(Set.empty, Game(player1, player2))
  println(result2.winner.map(_.points))
}

object Day22Test extends App {
  import Day22._

  val p1 = Player(List(9, 2, 6, 3, 1))
  val p2 = Player(List(5, 8, 4, 7, 10))
  val game = Game(p1, p2)

  val result = play(game)

  assert(result.player2.deck == List(3, 2, 10, 6, 8, 5, 9, 4, 7, 1))

  val winner = result.winner.orNull

  assert(winner.points == 306)

  val winner2 = play2(Set.empty, game).winner.orNull
  assert(winner2.points == 291)

  println("OK")
}