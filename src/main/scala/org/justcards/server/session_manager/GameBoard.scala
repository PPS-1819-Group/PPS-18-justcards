package org.justcards.server.session_manager

import org.justcards.commons.Card
import org.justcards.server.Commons.UserInfo
import GameBoard.PlayerCards
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge

import scala.util.Random

/**
 * Trait for classes that want to contain all the information about a match.
 */
trait GameBoard {

  /**
   * Getter.
   * @return cards on field
   */
  def fieldCards: List[(Card, String)]

  /**
   * Getter.
   * @return cards of all player
   */
  def playerCards: Map[String, PlayerCards]

  /**
   * Getter.
   * @return deck of match
   */
  def deck: List[Card]

  /**
   * Getter.
   * @return turn order of all player
   */
  def turn: List[String]

  /**
   * Getter.
   * @return last card of deck
   */
  def optionLastCardDeck: Option[Card]

  /**
   * Getter.
   * @return User that must play
   */
  def optionTurnPlayer: Option[String]

  /**
   * Getter.
   * @return player after given player
   */
  def playerAfter(player: String): Option[String]

  /**
   * Getter.
   * @param player player
   * @return cards held by the user
   */
  def handCardsOf(player: String): Set[Card]

  /**
   * Getter.
   * @return cards held by the player that has to play
   */
  def handCardsOfTurnPlayer: Option[Set[Card]] = this optionTurnPlayer match {
    case Some(player) => Some(this handCardsOf player)
    case None => None
  }

  /**
   * Getter.
   * @param player player
   * @return cards that player has taken from the field
   */
  def tookCardsOf(player: String): Set[Card]

  /**
   * Draw new cards.
   * @return a new GameBoard with the new cards drawn.
   */
  def draw: Option[GameBoard]

  /**
   * Play a new card.
   * @param card card that turn player plays
   * @return the new GameBoard after the move
   */
  def playerPlays(card: Card): GameBoard

  /**
   * Handle all the moves at the and of a hand.
   * @param player turn winner
   * @return a new default GameBoard
   */
  def handWinner(player: String): GameBoard

}

object GameBoard {

  /**
   * Create a new GameBoard.
   * @param gameKnowledge the knowledge of the game
   * @param team1 the first team members
   * @param team2 the second team members
   * @param firstPlayer the first player the match
   * @return a new GameBoard
   */
  def apply(gameKnowledge: GameKnowledge, team1: List[String], team2: List[String], firstPlayer: Option[UserInfo]): GameBoard = {
    import ListWithShift._
    val initialConfiguration = gameKnowledge.initialConfiguration//hand, draw, field
    val deck: List[Card] = Random.shuffle (gameKnowledge.deckCards toList)
    val turn: List[String] = team1.zipAll(team2, null, null)
      .flatMap {
        case (a, null) => Seq(a)
        case (null, b) => Seq(b)
        case (a, b) => Seq(a, b)
      }
    val (newDeck, playerCards) = drawCards(initialConfiguration._1, deck, turn.map((_,PlayerCards(Set(),Set()))).toMap, turn)
    val handTurn: List[String] = if (firstPlayer isEmpty) {
      val first = gameKnowledge.sessionStarterPlayer(playerCards.map(x => (UserInfo(x._1, null), x._2.hand)).toSet)
      if (first isDefined)
        (turn shiftTo first.get.username) get
      else
        turn
    } else (turn shiftTo firstPlayer.get.username) get

    val (drawCards2, deckRemaining) = newDeck splitAt initialConfiguration._3
    GameBoardImpl(
      drawCards2 map(a=>(a,handTurn.head)),
      playerCards,
      deckRemaining,
      handTurn,
      turn,
      initialConfiguration._2)
  }

  case class PlayerCards(hand: Set[Card], took: Set[Card])

  private[this] case class GameBoardImpl(fieldCards: List[(Card, String)],
                                         playerCards: Map[String, PlayerCards],
                                         deck: List[Card],
                                         handTurn: List[String],
                                         turn: List[String],
                                         nCardsDraw: Int) extends GameBoard {
    import ListWithShift._

    private def apply(field: List[(Card,String)],
              playerCards: Map[String, PlayerCards],
              deck: List[Card],
              handTurn: List[String]): GameBoard = {
      GameBoardImpl(field, playerCards, deck, handTurn, turn, nCardsDraw)
    }

    override def optionLastCardDeck: Option[Card] = deck lastOption

    override def optionTurnPlayer: Option[String] = handTurn headOption

    override def playerAfter(player: String): Option[String] = turn indexOf player match {
      case -1 => None
      case x if x == turn.size - 1 => Some(turn.head)
      case x => Some(turn(x + 1))
    }

    override def handCardsOf(player: String): Set[Card] = playerCards(player).hand

    override def tookCardsOf(player: String): Set[Card] = playerCards(player).took

    override def draw: Option[GameBoard] = {
      if (deck.isEmpty || nCardsDraw == 0) None
      else {
        val (newDeck, newPlayerCards) = drawCards(nCardsDraw, deck, playerCards, turn)
        Some(this (
          fieldCards,
          newPlayerCards,
          newDeck,
          handTurn
        ))
      }
    }


    override def playerPlays(card: Card): GameBoard =
      this(
        fieldCards :+ (card, handTurn.head),
        playerCards.map{
          case (player, cards) if player == handTurn.head => handTurn.head -> PlayerCards(cards.hand - card, cards.took)
          case x => x
        },
        deck,
        handTurn tail
      )

    override def handWinner(winner: String): GameBoard =
      this(
        List(),
        playerCards.map{
          case (`winner`, cards) => winner -> PlayerCards(cards.hand, cards.took ++ fieldCards.map(_._1))
          case x => x
        },
        deck,
        turn.shiftTo(winner).get
      )

  }

  private def drawCards(nCard: Int, deck: List[Card], playerCards: Map[String, PlayerCards], turn: List[String]): (List[Card], Map[String, PlayerCards]) = {
    val (drawCards, newDeck) = deck splitAt (nCard * 4)
    val splitDraw: List[List[Card]] = drawCards.sliding(nCard, nCard).toList
    val newPlayerCards: Map[String, PlayerCards] = (for(
      index <- turn.indices;
      username = turn(index);
      user = playerCards(username)
    ) yield username -> PlayerCards(user.hand ++ splitDraw(index), user.took)).toMap
    (newDeck, newPlayerCards)
  }

}

