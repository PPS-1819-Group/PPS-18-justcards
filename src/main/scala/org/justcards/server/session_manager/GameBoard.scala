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

}

object GameBoard {

  def apply(gameKnowledge: GameKnowledge, team1: List[UserInfo], team2: List[UserInfo], firstPlayer: UserInfo ): GameBoard = {
    import ListWithShift._
    val initialConfiguration = gameKnowledge.initialConfiguration//hand, draw, field
    val deck: List[Card] = Random.shuffle (gameKnowledge.deckCards toList)
    val turn: List[UserInfo] = team1.zipAll(team2, null, null)
      .flatMap {
        case (a, b) => Seq(a, b)
        case (a, null) => Seq(a)
        case (null, b) => Seq(b)
      }
    val players = turn.map( a => (a , PlayerCards(Set(), Set()))).toMap
    val handTurn = turn.shiftTo(firstPlayer).get
    GameBoardImpl(List(), players, deck, handTurn, turn)
  }



  private[this] case class GameBoardImpl(fieldCards: List[(Card, UserInfo)],
                                         playerCards: Map[UserInfo, PlayerCards],
                                         deck: List[Card],
                                         handTurn: List[UserInfo],
                                         turn: List[UserInfo]) extends GameBoard {

    /**
     * Creates a GameBoard
     * @param field cards on field and their player
     * @param playerCards cards of players
     * @param deck deck of cards
     * @param handTurn list of user have to do their turn yet
     * @param turn list of user in order
     * @return a new GameBoard
     */
    def apply(field: List[(Card,UserInfo)],
              playerCards: Map[UserInfo, PlayerCards],
              deck: List[Card],
              handTurn: List[UserInfo],
              turn: List[UserInfo]): GameBoard = {
      GameBoardImpl(field, playerCards, deck, handTurn, turn)
    }
  }

}

