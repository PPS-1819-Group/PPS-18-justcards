package org.justcards.client

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.controller.AppController
import org.justcards.client.view.{ConsoleView, View}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions

object ClientApp extends App {

  val host = "127.0.0.1"
  val port = 6789

  val system = ActorSystem("justcards-client", ConfigFactory.load("client"))
  val connectionManager = ConnectionManager(host, port, ActorWithConnectionOptions.REMOTES)
  val view: View = ConsoleView()
  val appController = system.actorOf(AppController(connectionManager,view))
}
