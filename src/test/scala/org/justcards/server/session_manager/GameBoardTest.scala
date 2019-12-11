package org.justcards.server.session_manager

import org.justcards.server.session_manager.SessionManagerTest._
import org.scalactic.source.Position
import org.scalatest.{BeforeAndAfter, Matchers, WordSpecLike}

class GameBoardTest extends WordSpecLike with Matchers with BeforeAndAfter {

  val USERNAME = "username"
  val HAND_CARDS = 1
  val DRAW_CARD = 1
  var gameBoard: GameBoard = _
  val gameKnowledge = TestGameKnowledge((HAND_CARDS, DRAW_CARD, 0))


  "The GameBoard" when {

    "initialize" should {

      "give the cards to all users" in {
        gameBoard = GameBoard(gameKnowledge, List(USERNAME, USERNAME + 1), List(USERNAME + 2, USERNAME + 3), None)
        (gameBoard.handCardsOf(USERNAME).size,
          gameBoard.handCardsOf(USERNAME + 1).size,
          gameBoard.handCardsOf(USERNAME + 2).size,
          gameBoard.handCardsOf(USERNAME + 3).size) shouldBe (HAND_CARDS, HAND_CARDS, HAND_CARDS, HAND_CARDS)
      }

      "deck have all cards except cards in the hand of players" in {
        gameBoard = GameBoard(gameKnowledge, List(USERNAME, USERNAME + 1), List(USERNAME + 2, USERNAME + 3), None)
        gameBoard.deck.size + HAND_CARDS*4 shouldBe gameKnowledge.deckCards.size
      }

    }

    "all user draw" should {

      "give the cards to all users" in {
        gameBoard = GameBoard(gameKnowledge, List(USERNAME, USERNAME + 1), List(USERNAME + 2, USERNAME + 3), None).draw.get
        (gameBoard.handCardsOf(USERNAME).size,
          gameBoard.handCardsOf(USERNAME + 1).size,
          gameBoard.handCardsOf(USERNAME + 2).size,
          gameBoard.handCardsOf(USERNAME + 3).size) shouldBe (HAND_CARDS+DRAW_CARD, HAND_CARDS+DRAW_CARD, HAND_CARDS+DRAW_CARD, HAND_CARDS+DRAW_CARD)
      }


      "deck have all cards except cards in the hand of players" in {
        gameBoard = GameBoard(gameKnowledge, List(USERNAME, USERNAME + 1), List(USERNAME + 2, USERNAME + 3), None).draw.get
        gameBoard.deck.size + (HAND_CARDS+DRAW_CARD)*4 shouldBe gameKnowledge.deckCards.size
      }
    }
  }

}
