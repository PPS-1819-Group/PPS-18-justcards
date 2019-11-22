package org.justcards.server.knowledge_engine.game_knowledge

import java.io.FileInputStream

import alice.tuprolog.{Prolog, Theory}
import org.justcards.commons.GameId
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge._

class PrologGameKnowledge(private val game: GameId) extends GameKnowledge {

  import PrologGameKnowledge._
  private val knowledge: Prolog = createKnowledge(game)

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
