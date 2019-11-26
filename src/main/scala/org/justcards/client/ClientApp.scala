package org.justcards.client

import akka.actor.ActorSystem
import org.justcards.client.connection_manager.TcpConnectionManager
import org.justcards.client.controller.AppController
import org.justcards.client.view.{ConsoleView, View}

object ClientApp extends App {
  val system = ActorSystem("justcards-client")
  val host = "localhost"
  val port = 6789
  val connectionManager = TcpConnectionManager(host,port)
  val view: View = ConsoleView()
  val appController = system.actorOf(AppController(connectionManager,view))
}
