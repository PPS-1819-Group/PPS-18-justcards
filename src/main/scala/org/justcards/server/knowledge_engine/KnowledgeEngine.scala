package org.justcards.server.knowledge_engine

import akka.actor.{Actor, Props}
import org.justcards.commons.GameId

class KnowledgeEngine extends Actor {

  import KnowledgeEngine._

  override def receive: Receive = {
    case GameExistenceRequest(_) => sender() ! GameExistenceResponse(true)
  }

}

object KnowledgeEngine {
  def apply(): Props = Props(classOf[KnowledgeEngine])

  case class GameExistenceRequest(gameId: GameId)
  case class GameExistenceResponse(existence: Boolean)

}
