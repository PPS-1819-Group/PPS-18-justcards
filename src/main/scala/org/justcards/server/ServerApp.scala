package org.justcards.server

import akka.actor.ActorSystem
import org.justcards.server.connection_manager.ServerConnectionManager
import org.justcards.server.user_manager.UserManager

object ServerApp extends App {

  val port = 6789

  val system = ActorSystem("justCards-server")
  val userManager = system.actorOf(UserManager())
  system.actorOf(ServerConnectionManager(port, userManager))

}
