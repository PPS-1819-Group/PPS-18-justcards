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

  val serverAddress = if(args.length > 0) args(0) else "127.0.0.1"
  val port = 6789

  val defaultConfig = ConfigFactory.load("server")
  val config = ConfigFactory.parseString(s"akka.remote.artery.canonical.hostname = $serverAddress").withFallback(defaultConfig)

  val system = ActorSystem(SERVER_SYSTEM_NAME, config)
  val sessionCreator = system.actorOf(SessionCreator())
  val knowledgeEngine = system.actorOf(KnowledgeEngine(GamesManager()))
  val userManager = system.actorOf(UserManager(sessionCreator, knowledgeEngine))
  system.actorOf(
    ServerConnectionManager(port, userManager, ActorWithConnectionOptions.REMOTES),
    SERVER_NAME
  )

  println("JustCards server ready")
}
