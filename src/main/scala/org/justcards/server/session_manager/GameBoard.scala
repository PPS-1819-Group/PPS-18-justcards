package org.justcards.server.session_manager

import org.justcards.commons.Card
import org.justcards.server.Commons.{PlayerCards, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge

import scala.util.Random

sealed trait GameBoard {

  /**
   * Getter
   * @return cards on field
   */
  def fieldCards: List[(Card, UserInfo)]

  /**
   * Getter
   * @return cards of all player
   */
  def playerCards: Map[UserInfo, PlayerCards]

  /**
   * Getter
   * @return User that must play
   */
  def getTurnPlayer: UserInfo

  /**
   * Getter
   * @param player player
   * @return cards in the player hand
   */
  def getPlayerHandCards(player: UserInfo): Set[Card]

  /**
   * Getter
   * @param player player
   * @return cards that player won
   */
  def getPlayerTookCards(player: UserInfo): Set[Card]

  /**
   * Players draw
   * @return GameBoard after the players draw
   */
  def draw: GameBoard

  /**
   * Player play a card
   * @param card card that player play
   * @return GameBoard after player play the card
   */
  def playerPlays(card: Card): GameBoard

  /**
   * Give fieldCards to turn winner and reset turn
   * @param player turn winner
   * @return GameBoard with empty field and reset turn
   */
  def handWinner(player: UserInfo): GameBoard

}

object GameBoard {

  def apply(gameKnowledge: GameKnowledge, team1: List[UserInfo], team2: List[UserInfo], firstPlayer: UserInfo): GameBoard = {
    import ListWithShift._
    val initialConfiguration = gameKnowledge.initialConfiguration//hand, draw, field
    var deck: List[Card] = Random.shuffle (gameKnowledge.deckCards toList)
    val turn: List[UserInfo] = team1.zipAll(team2, null, null)
      .flatMap {
        case (a, null) => Seq(a)
        case (null, b) => Seq(b)
        case (a, b) => Seq(a, b)
      }
    val players = turn.map( a => {
      val player = (a , PlayerCards(deck take initialConfiguration._1 toSet, Set()))
      deck = deck drop initialConfiguration._1
      player
    }).toMap
    val handTurn = turn.shiftTo(firstPlayer).get

    GameBoardImpl(
      deck take initialConfiguration._3 map(a=>(a,firstPlayer)),
      players,
      deck drop initialConfiguration._3,
      handTurn,
      turn,
      initialConfiguration._2)

  }



  private[this] case class GameBoardImpl(fieldCards: List[(Card, UserInfo)],
                                         playerCards: Map[UserInfo, PlayerCards],
                                         deck: List[Card],
                                         handTurn: List[UserInfo],
                                         turn: List[UserInfo],
                                         nCardsDraw: Int) extends GameBoard {
    import ListWithShift._

    private def apply(field: List[(Card,UserInfo)],
              playerCards: Map[UserInfo, PlayerCards],
              deck: List[Card],
              handTurn: List[UserInfo]): GameBoard = {
      GameBoardImpl(field, playerCards, deck, handTurn, turn, nCardsDraw)
    }

    override def getTurnPlayer: UserInfo = handTurn.head

    override def getPlayerHandCards(player: UserInfo): Set[Card] = playerCards(player).hand

    override def getPlayerTookCards(player: UserInfo): Set[Card] = playerCards(player).took

    override def draw: GameBoard =
      this(
        fieldCards,
        playerCards.mapValues( cards => PlayerCards(cards.hand ++ deck.take(nCardsDraw), cards.took)),
        deck drop nCardsDraw,
        handTurn
      )

    override def playerPlays(card: Card): GameBoard =
      this(
        fieldCards,
        playerCards.map{
          case (player, cards) if player == handTurn.head => handTurn.head -> PlayerCards(cards.hand - card, cards.took)
          case x => x
        },
        deck,
        handTurn tail
      )

    override def handWinner(winner: UserInfo): GameBoard =
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

}

