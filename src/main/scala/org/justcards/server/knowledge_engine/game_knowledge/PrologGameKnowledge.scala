package org.justcards.server.knowledge_engine.game_knowledge

import java.io.FileInputStream

import alice.tuprolog.{Prolog, Theory}
import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge._

class PrologGameKnowledge(private val game: GameId) extends GameKnowledge {

  import PrologGameKnowledge._
  private val knowledge: Prolog = createKnowledge(game)

  override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = ???

  override def deckCards: Set[Card] = ???

  override def hasToChooseBriscola: BriscolaSetting = ???

  override def isBriscolaValid(seed: Seed): Boolean = ???

  override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = ???

  override def handWinner(fieldCards: List[(Card, Commons.UserInfo)]): Commons.UserInfo = ???

  override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = ???

  override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Team = ???

  override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = ???
}

object PrologGameKnowledge {

  def apply(): GameKnowledgeFactory = (game: GameId) => new PrologGameKnowledge(game)

  private[this] val COMMON_RULES_FILE: FileInputStream = GameKnowledge.COMMON_RULES_PATH
  private[this] val COMMON_RULES = new Theory(COMMON_RULES_FILE)

  private[this] implicit def fromStringToInputStream(path: String): FileInputStream = new FileInputStream(path)

  private def createKnowledge(game: GameId): Prolog = {
    val engine = new Prolog()
    val gameTheoryFile: FileInputStream = GAMES_PATH concat game.name.toLowerCase concat ".pl"
    val gameTheory = new Theory(gameTheoryFile)
    engine setTheory COMMON_RULES
    engine setTheory gameTheory
    engine
  }
}
