package org.justcards.server.knowledge_engine.game_knowledge

import org.justcards.commons.{Card, GameId}
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team.Team
import org.justcards.server.Commons.UserInfo

trait GameKnowledgeFactory extends (GameId => GameKnowledge)

trait GameKnowledge {
  def initialConfiguration: (Int,Int,Int)
  def getDeckCards: Set[Card]
  def hasToChooseBriscola: BriscolaSetting
  def play(card: Card, field: Set[Card], hand: Set[Card]): Option[Set[Card]]
  def handWinner(fieldCards: Set[(Card,UserInfo)]): UserInfo
  def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team,Int,Int)
  def sessionWinner(firstTeamPoints: Int, secondTeamPoints: Int): Team
  def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Int,Int)
}

object GameKnowledge {

  def apply(): GameKnowledgeFactory = PrologGameKnowledge()

  private[this] val COMMON_PATH: String = "src/main/resources/org/justcards/rules/"
  val COMMON_RULES_PATH: String = COMMON_PATH concat "commonRules.pl"
  val GAMES_PATH: String = COMMON_PATH + "games/"
}
