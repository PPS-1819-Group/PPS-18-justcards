package org.justcards.server.knowledge_engine

import org.justcards.commons.{Card, GameId}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class GameKnowledgeTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private val gameName = "beccaccino"
  private val bastoniBriscola = "bastoni"
  private val coppeBriscola = "coppe"
  private val spadeBriscola = "spade"
  private val denaraBriscola = "denara"
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

  }
}