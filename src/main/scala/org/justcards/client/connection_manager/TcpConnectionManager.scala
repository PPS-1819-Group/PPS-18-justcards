package org.justcards.client.connection_manager

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.io.{IO, Tcp}
import akka.io.Tcp.{CommandFailed, Connect, Connected, ConnectionClosed, Register, Write}
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.commons.{AppError, AppMessage, ErrorOccurred}
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}

object TcpConnectionManager {

  def apply(address: InetSocketAddress): ConnectionManager =
    (appController: ActorRef) => Props(classOf[TcpConnectionManager], address, appController)

  def apply(host: String, port: Int): ConnectionManager = TcpConnectionManager(new InetSocketAddress(host, port))

  private[this] class TcpConnectionManager(address: InetSocketAddress, appController: ActorRef)
    extends AbstractConnectionManager(appController) with ActorWithTcp with Stash {

    import context.system

    override protected def initializeConnection(): Unit = IO(Tcp) ! Connect(address)

    override protected def init: Receive = {
      case CommandFailed(_: Connect) => appController ! ErrorOccurred(AppError.CANNOT_CONNECT.toString)
      case _ @ Connected(_, _) =>
        val server = sender()
        server ! Register(self)
        unstashAll()
        connected(server)
      case _ => stash()
    }

    override protected def connectionErrorHandling(server: ActorRef): Receive = {
      case CommandFailed(_: Write) => appController ! ErrorOccurred(AppError.MESSAGE_SENDING_FAILED.toString)
      case _: ConnectionClosed =>
        unstashAll()
        this become init
        appController ! ErrorOccurred(AppError.CONNECTION_LOST.toString)
      case _ => stash()
    }
  }
}

