package org.justcards.client.connection_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.commons.AppMessage
import org.justcards.commons.actor_connection.{ActorWithConnection, Outer}

trait ConnectionManager extends (ActorRef => Props)

object ConnectionManager {
  case object InitializeConnection
}

abstract class AbstractConnectionManager(appController: ActorRef) extends ActorWithConnection with Actor {

  override def receive: Receive = parse orElse waitForInit

  protected def init: Receive

  protected def initializeConnection(): Unit

  protected def connected(server: ActorRef): Unit =
    this become (work(server) orElse connectionErrorHandling(server))

  protected def connectionErrorHandling(server: ActorRef): Receive

  private def waitForInit: Receive = {
    case InitializeConnection =>
      initializeConnection()
      this become init
  }

  private def work(server: ActorRef): Receive = {
    case m: AppMessage => server ==> m
    case Outer(m: AppMessage) => appController ! m
  }
}
