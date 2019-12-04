package org.justcards.server.session_manager

import akka.actor.{Actor, Props}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.session_manager.SessionCreator.CreateSession
import org.justcards.server.user_manager.Lobby

class SessionCreator extends Actor {

  override def receive: Receive = {
    case CreateSession(lobby, gameKnowledge) =>
      context.actorOf(SessionManager(lobby, gameKnowledge))
  }
}

object SessionCreator {

  def apply(): Props = Props(classOf[SessionCreator])

  case class CreateSession(lobby: Lobby, gameKnowledge: GameKnowledge)
}