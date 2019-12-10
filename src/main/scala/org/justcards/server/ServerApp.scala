package org.justcards.server

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions
import org.justcards.commons._
import org.justcards.server.connection_manager.ServerConnectionManager
import org.justcards.server.knowledge_engine.{GamesManager, KnowledgeEngine}
import org.justcards.server.session_manager.SessionCreator
import org.justcards.server.user_manager.UserManager

/**
  * Server App entry point.
  */
object ServerApp extends App {

  val port = 6789

  val system = ActorSystem(SERVER_SYSTEM_NAME, ConfigFactory.load("server"))
  val sessionCreator = system.actorOf(SessionCreator())
  val knowledgeEngine = system.actorOf(KnowledgeEngine(GamesManager()))
  val userManager = system.actorOf(UserManager(sessionCreator, knowledgeEngine))
  system.actorOf(
    ServerConnectionManager(port, userManager, ActorWithConnectionOptions.REMOTES),
    SERVER_NAME
  )

  println("JustCards server ready")
}
