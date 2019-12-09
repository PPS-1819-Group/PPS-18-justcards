package org.justcards.client.connection_manager

import akka.actor.{ActorLogging, ActorRef, Props, Terminated}
import org.justcards.commons.AppError.CONNECTION_LOST
import org.justcards.commons.actor_connection.ActorWithRemotes
import org.justcards.commons.actor_connection.ActorWithRemotes._
import org.justcards.commons._

import scala.util.Success

class RemoteConnectionManager(address: (String, Int), appController: ActorRef)
                              extends AbstractConnectionManager(appController) with ActorWithRemotes with ActorLogging {

  override protected def init: Receive = {
    case Connected(server) =>
      context watch server
      connected(server)
  }

  override protected def connectionErrorHandling(server: ActorRef): Receive = {
    case Terminated(`server`) => error(CONNECTION_LOST)
  }

  override protected def initializeConnection(): Unit = {

    import context.dispatcher
    import scala.concurrent.duration._
    import akka.util.Timeout
    implicit val timeout = Timeout(3 seconds)

    val path = "akka://" + SERVER_SYSTEM_NAME + "@" + address._1 + ":" + address._2 + "/user/" + SERVER_NAME
    context actorSelection path resolveOne() onComplete {
      case Success(actor) => actor ! Connect()
      case _ => log.warning(path + " not found! Cannot connect to server")
    }
  }

  override protected def terminateConnection(server: ActorRef): Unit = {}
}

object RemoteConnectionManager {

  def apply(address: (String, Int)): ConnectionManager =
    Props(classOf[RemoteConnectionManager], address, _)

  def apply(serverName: String, host: String, port: Int): ConnectionManager =
    RemoteConnectionManager((host, port))
}
