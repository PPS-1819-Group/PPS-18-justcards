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

  override def initialConfiguration: (Int, Int, Int) = ???

  override def getDeckCards: Set[Card] = ???

  override def hasToChooseBriscola: BriscolaSetting = ???

  override def play(card: Card, field: Set[Card], hand: Set[Card]): Option[Set[Card]] = ???

  override def handWinner(fieldCards: Set[(Card, Commons.UserInfo)]): Commons.UserInfo = ???

  override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Int, Int) = ???

  override def sessionWinner(firstTeamPoints: Int, secondTeamPoints: Int): Team = ???

  override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Int, Int) = ???
}

object PrologGameKnowledge {

  def apply(): GameKnowledgeFactory = (game: GameId) => new PrologGameKnowledge(game)

  private[this] val COMMON_RULES = new Theory(GameKnowledge.COMMON_RULES_PATH)

  private[this] implicit def fromStringToInputStream(path: String): FileInputStream =
    new FileInputStream(path)

  private def createKnowledge(game: GameId): Prolog = {
    val engine = new Prolog()
    val gameTheory = new Theory(GAMES_PATH concat game.name.toLowerCase concat ".pl")
    engine.setTheory(COMMON_RULES)
    engine.setTheory(gameTheory)
    engine
  }
}
