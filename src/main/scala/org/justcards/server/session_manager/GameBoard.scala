package org.justcards.server.session_manager

import org.justcards.commons.Card
import org.justcards.server.Commons.UserInfo

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

  /**
   * Creates a new lobby
   * @param field cards on field and their player
   * @param playerCards cards of players
   * @param deck deck of cards
   * @param turn list of user have to do their turn
   * @return a new lobby
   */
  def apply(field: List[(Card,UserInfo)],
            playerCards: Map[UserInfo, PlayerCards],
            deck: List[Card],
            turn: List[UserInfo]): GameBoard = {
    GameBoardImpl(field, playerCards, deck, turn)
  }

  private[this] case class GameBoardImpl(fieldCards: List[(Card, UserInfo)],
                                         playerCards: Map[UserInfo, PlayerCards],
                                         deck: List[Card],
                                         turn: List[UserInfo]) extends GameBoard {
  }

}

case class PlayerCards(hand: Set[Card], took: Set[Card])