package org.justcards.client

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.controller.AppController
import org.justcards.client.view.View
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions

/**
 * Entry point to run the client application.
 */
object ClientApp extends App {
  val host = if(args.length > 0) args(0) else "127.0.0.1"
  val port = 6789

  val system = ActorSystem("justcards-client", ConfigFactory.load("client"))
  val connectionManager = ConnectionManager(host, port, ActorWithConnectionOptions.REMOTES)
  val view = View()
  val appController = system.actorOf(AppController(connectionManager,view))
}
