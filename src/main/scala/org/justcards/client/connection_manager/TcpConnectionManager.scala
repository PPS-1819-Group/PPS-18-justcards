package org.justcards.client.connection_manager

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.io.Tcp.{Close, Closed, CommandFailed, Connect, Connected, ConnectionClosed, Register, Write}
import org.justcards.commons.AppError._
import org.justcards.commons.actor_connection.ActorWithTcp
import org.justcards.commons.actor_connection.ActorWithTcp._

object TcpConnectionManager {

  def apply(address: InetSocketAddress): ConnectionManager =
    (appController: ActorRef) => Props(classOf[TcpConnectionManager], address, appController)

  def apply(host: String, port: Int): ConnectionManager = TcpConnectionManager(new InetSocketAddress(host, port))

  private[this] class TcpConnectionManager(address: InetSocketAddress, appController: ActorRef)
    extends AbstractConnectionManager(appController) with ActorWithTcp {

    import context.system

    override protected def initializeConnection(): Unit = IO(Tcp) ! Connect(address)

    override protected def init: Receive = {
      case CommandFailed(_: Connect) => error(CANNOT_CONNECT)
      case _ @ Connected(_, _) =>
        val server = sender()
        server ! Register(self)
        connected(server)
    }

    override protected def connectionErrorHandling(server: ActorRef): Receive = {
      case CommandFailed(message: Write) => error(MESSAGE_SENDING_FAILED,extractMessage(message.data).get)
      case _: ConnectionClosed | Closed => error(CONNECTION_LOST)
    }

    override protected def terminateConnection(server: ActorRef): Unit = {
      server ! Close
    }
  }
}