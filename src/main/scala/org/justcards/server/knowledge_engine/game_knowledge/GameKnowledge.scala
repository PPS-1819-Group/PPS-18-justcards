package org.justcards.server.knowledge_engine.game_knowledge

import org.justcards.commons.GameId

trait GameKnowledgeFactory extends (GameId => GameKnowledge)

trait GameKnowledge

object GameKnowledge {

  def apply(): GameKnowledgeFactory = PrologGameKnowledge()

  private[this] val COMMON_PATH: String = "src/main/resources/org/justcards/rules/"
  val COMMON_RULES_PATH: String = COMMON_PATH concat "commonRules.pl"
  val GAMES_PATH: String = COMMON_PATH + "games/"
}
