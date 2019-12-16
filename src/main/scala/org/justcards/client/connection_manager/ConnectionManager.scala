package org.justcards.client.connection_manager

import java.util.Objects

import akka.actor.{Actor, ActorRef, Props, Stash}
import org.justcards.client.connection_manager.ConnectionManager._
import org.justcards.commons.AppError.{CANNOT_CONNECT, CONNECTION_LOST, MESSAGE_SENDING_FAILED}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions._
import org.justcards.commons.{AppError, AppMessage, ErrorOccurred}
import org.justcards.commons.actor_connection.{ActorWithConnection, Outer}

trait ConnectionManager extends (ActorRef => Props)

object ConnectionManager {
  case object InitializeConnection
  case object TerminateConnection
  case object Connected
  case class DetailedErrorOccurred(error: AppError.Value, message: AppMessage)

  def apply(host: String, port: Int): ConnectionManager = ConnectionManager(host, port, REMOTES)

  def apply(host: String, port: Int, mode: ActorWithConnectionOptions): ConnectionManager =
    mode match {
      case TCP => TcpConnectionManager(host, port)
      case REMOTES => RemoteConnectionManager(host, port)
    }

}

abstract class AbstractConnectionManager(appController: ActorRef) extends ActorWithConnection with Actor with Stash {

  override def init: Receive = parse orElse waitForInit

  protected def initConnection: Receive

  protected def connectionErrorHandling(server: ActorRef): Receive

  private def waitForInit: Receive = {
    case InitializeConnection =>
      initializeConnection()
      this become (initConnection orElse stashUnhandled)
  }

  private def work(server: ActorRef): Receive = {
    case m: AppMessage => server ==> m
    case Outer(m: AppMessage) => appController ! m
    case TerminateConnection => terminateConnection(server)
  }

  private def stashUnhandled: Receive = {
    case _ => stash()
  }

  protected def initializeConnection(): Unit

  protected def terminateConnection(server: ActorRef): Unit

  protected def connected(server: ActorRef): Unit = {
    unstashAll()
    this become (work(server) orElse connectionErrorHandling(server) orElse stashUnhandled)
    appController ! Connected
  }

  protected def error(message: AppError.Value, data: AppMessage*): Unit = message match {
    case CONNECTION_LOST =>
      unstashAll()
      initializeConnection()
      this become (init orElse stashUnhandled)
      appController ! ErrorOccurred(CONNECTION_LOST)
    case CANNOT_CONNECT =>
      this become waitForInit
      appController ! ErrorOccurred(CANNOT_CONNECT)
    case MESSAGE_SENDING_FAILED => data filter Objects.nonNull foreach { message =>
      appController ! DetailedErrorOccurred(MESSAGE_SENDING_FAILED,message)
    }
  }
}