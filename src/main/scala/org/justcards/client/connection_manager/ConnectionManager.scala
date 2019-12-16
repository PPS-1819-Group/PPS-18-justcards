package org.justcards.client.connection_manager

import java.util.Objects

import akka.actor.{Actor, ActorRef, Props, Stash}
import org.justcards.client.connection_manager.ConnectionManager._
import org.justcards.commons.AppError.{CANNOT_CONNECT, CONNECTION_LOST, MESSAGE_SENDING_FAILED}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions._
import org.justcards.commons.{AppError, AppMessage, ErrorOccurred}
import org.justcards.commons.actor_connection.{ActorWithConnection, Outer}

object ConnectionManager {
  /**
   * Factory to create a ConnectionManager given reference of an AppController.
   */
  type ConnectionManagerFactory = ActorRef => Props
  case object InitializeConnection
  case object TerminateConnection
  case object Connected
  case class DetailedErrorOccurred(error: AppError.Value, message: AppMessage)

  /**
   * Create a ConnectionManagerFactory.
   * @param host the server hostname
   * @param port the server port
   * @return a ConnectionManagerFactory
   */
  def apply(host: String, port: Int): ConnectionManagerFactory = ConnectionManager(host, port, REMOTES)

  /**
   * Create a ConnectionManagerFactory.
   * @param host the server hostname
   * @param port the server port
   * @param mode the connection technology to be used
   * @return a ConnectionManagerFactory
   */
  def apply(host: String, port: Int, mode: ActorWithConnectionOptions): ConnectionManagerFactory =
    mode match {
      case TCP => TcpConnectionManager(host, port)
      case REMOTES => RemoteConnectionManager(host, port)
    }

}

/**
 * Partial implementation of a ConnectionManager that encapsulate all the logic.
 * @param appController the AppController actor reference
 */
abstract class AbstractConnectionManager(appController: ActorRef) extends ActorWithConnection with Actor with Stash {

  override final def init: Receive = waitForInit

  /**
   * The behaviour to initialize a connection and get connected.
   * @return the behaviour to initialize a connection
   */
  protected def initConnection: Receive

  /**
   * The behaviour to handle connection errors due to the specific implementation.
   * @param server the server whose the connection is established
   * @return the behaviour to handle connection errors
   */
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

  /**
   * Procedure to start the connection process.
   */
  protected def initializeConnection(): Unit

  /**
   * Procedure to terminate the connection with the server.
   * @param server the server whose the connection is established
   */
  protected def terminateConnection(server: ActorRef): Unit

  /**
   * Procedure to call once the connection is established.
   * @param server the server whose the connection is established
   */
  protected final def connected(server: ActorRef): Unit = {
    unstashAll()
    this become (work(server) orElse connectionErrorHandling(server) orElse stashUnhandled)
    appController ! Connected
  }

  /**
   * Notify an error occurred.
   * @param message the error
   * @param data messages that caused that error
   */
  protected final def error(message: AppError.Value, data: AppMessage*): Unit = message match {
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