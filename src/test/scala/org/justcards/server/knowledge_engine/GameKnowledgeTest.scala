package org.justcards.server.knowledge_engine

import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons.BriscolaSetting
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class GameKnowledgeTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private val gameName = "beccaccino"
  private val bastoniBriscola = "bastoni"
  private val coppeBriscola = "coppe"
  private val spadeBriscola = "spade"
  private val denaraBriscola = "denara"
  private val nonValidBriscola = "nonValid"
  private val seeds = Set(bastoniBriscola,coppeBriscola,spadeBriscola,denaraBriscola)
  private val gameKnowledge = GameKnowledge()(GameId(gameName))

  "The game knowledge" should {

    "return the initial configuration of a game" in {
      gameKnowledge.initialConfiguration shouldBe (10,0,0)
    }

    "return the cards in the game deck" in {
      val cards = for (seed <- seeds; number <- 1 to 10) yield Card(number,seed)
      gameKnowledge.deckCards shouldBe cards
    }

    "return how the Briscola has to be chosen" in {
      gameKnowledge.hasToChooseBriscola shouldBe BriscolaSetting.USER
    }

    "allow to choose a valid Briscola" in {
      gameKnowledge.setBriscola(bastoniBriscola) shouldBe true
    }

    "deny to choose a non valid Briscola" in {
      gameKnowledge.setBriscola(nonValidBriscola) shouldBe false
    }

    "allow to play a card in the hand of a player" when {

      "there are no cards on the field" in {
        val cardToPlay = Card(1,spadeBriscola)
        gameKnowledge.play(cardToPlay,List(),Set(cardToPlay)) shouldBe Some(List(cardToPlay))
      }

      "has the same seed of the first card played in the current turn" in {
        val cardToPlay = Card(3,spadeBriscola)
        val cardsOnField = List(Card(1,spadeBriscola))
        gameKnowledge.play(cardToPlay,cardsOnField,Set(cardToPlay)) shouldBe Some(cardsOnField:::List(cardToPlay))
      }

      "has a different seed of the first card played in the current turn and the player does not have card with that seed in his hand" in {
        val cardToPlay = Card(3,spadeBriscola)
        val cardsOnField = List(Card(1,bastoniBriscola))
        gameKnowledge.play(cardToPlay,cardsOnField,Set(cardToPlay)) shouldBe Some(cardsOnField:::List(cardToPlay))
      }

    }

    "deny to play a card" when {

      "the player does not have the card in his hand" in {
        val cardToPlay = Card(1,spadeBriscola)
        gameKnowledge.play(cardToPlay,List(),Set(Card(2,spadeBriscola))) shouldBe None
      }

    }

  }
}
