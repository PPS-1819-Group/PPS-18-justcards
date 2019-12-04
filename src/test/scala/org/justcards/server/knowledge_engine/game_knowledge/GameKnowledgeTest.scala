package org.justcards.server.knowledge_engine.game_knowledge

import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons.{BriscolaSetting, Team, UserInfo}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class GameKnowledgeTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private val gameName = "beccaccino"
  private val bastoni = "bastoni"
  private val coppe = "coppe"
  private val spade = "spade"
  private val denara = "denara"
  private val nonValidSeed = "nonValid"
  private val seeds = Set(bastoni,coppe,spade,denara)
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
      gameKnowledge.setBriscola(bastoni) shouldBe true
    }

    "deny to choose a non valid Briscola" in {
      gameKnowledge.setBriscola(nonValidSeed) shouldBe false
    }

    "allow to play a card in the hand of a player" when {

      "there are no cards on the field" in {
        val cardToPlay = Card(1,spade)
        gameKnowledge.play(cardToPlay,List(),Set(cardToPlay)) shouldBe Some(List(cardToPlay))
      }

      "has the same seed of the first card played in the current turn" in {
        val cardToPlay = Card(3,spade)
        val cardsOnField = List(Card(1,spade))
        gameKnowledge.play(cardToPlay,cardsOnField,Set(cardToPlay)) shouldBe Some(cardsOnField:::List(cardToPlay))
      }

      "has a different seed of the first card played in the current turn and the player does not have card with that seed in his hand" in {
        val cardToPlay = Card(3,spade)
        val cardsOnField = List(Card(1,bastoni))
        gameKnowledge.play(cardToPlay,cardsOnField,Set(cardToPlay)) shouldBe Some(cardsOnField:::List(cardToPlay))
      }

    }

    "deny to play a card" when {

      "the player does not have the card in his hand" in {
        val cardToPlay = Card(1,spade)
        gameKnowledge.play(cardToPlay,List(),Set(Card(2,spade))) shouldBe None
      }

    }

    "determine who wins the current hand" in {
      val winner = "player3"
      val cardsOnField = for (number <- 1 to 4) yield (Card(number, spade),UserInfo("player" + number, null))
      gameKnowledge.handWinner(cardsOnField.toList).username shouldBe winner
    }

    "determine who wins the current match" in {
      val firstTeamCards = for (number <- 1 to 4) yield Card(number, spade)
      val secondTeamCards = for (number <- 2 to 5) yield Card(number, coppe)
      gameKnowledge.matchWinner(firstTeamCards toSet, secondTeamCards toSet, Team.TEAM_1) shouldBe (Team.TEAM_1,2,0)
    }

    "determine the points obtained in a match" in {
      val firstTeamCards = for (number <- 1 to 4) yield Card(number, spade)
      val secondTeamCards = for (number <- 2 to 5) yield Card(number, coppe)
      gameKnowledge.matchPoints(firstTeamCards toSet, secondTeamCards toSet, Team.TEAM_1) shouldBe (2,0)
    }

    "determine a winner team for the session" when {

      "a team points are higher or equal the required points to win and are higher than the other team's points" in {
        gameKnowledge.sessionWinner(41,37) shouldBe Some(Team.TEAM_1)
      }

    }

    "not determine a winner team for the session" when {

      "the team has equals points" in {
        gameKnowledge.sessionWinner(33,33) shouldBe None
      }

      "the team has equal points both higher or equal than the required for win the session" in {
        gameKnowledge.sessionWinner(44,44) shouldBe None
      }

      "neither team has reached enough points to win" in {
        gameKnowledge.sessionWinner(37,39) shouldBe None
      }

    }

  }
}
