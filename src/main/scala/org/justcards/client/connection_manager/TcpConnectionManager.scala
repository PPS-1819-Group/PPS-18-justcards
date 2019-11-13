package org.justcards.client.connection_manager

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.io.{IO, Tcp}
import akka.io.Tcp.{CommandFailed, Connect, Connected, ConnectionClosed, Register, Write}
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.commons.{AppMessage, ErrorOccurred}
import org.justcards.commons.AppError._
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}

object TcpConnectionManager {

  def apply(address: InetSocketAddress): ConnectionManager =
    (appController: ActorRef) => Props(classOf[TcpConnectionManagerWithTcp], address, appController)

  def apply(host: String, port: Int): ConnectionManager = TcpConnectionManager(new InetSocketAddress(host, port))

  abstract class TcpConnectionManager(address: InetSocketAddress, appController: ActorRef) extends ActorWithConnection with Actor with Stash {

    import context.system

    override def receive: Receive = parse orElse init

    private def init: Receive = {
      case InitializeConnection => IO(Tcp) ! Connect(address)
      case CommandFailed(_: Connect) => appController ! ErrorOccurred(CANNOT_CONNECT)
      case _ @ Connected(_, _) =>
        val server = sender()
        server ! Register(self)
        unstashAll()
        this become (ready(server) orElse connectionErrorHandling(server))
      case _ => stash()
    }

    private def ready(server: ActorRef): Receive = {
      case m: AppMessage => server ==> m
      case Outer(m: AppMessage) => appController ! m
    }

    private def connectionErrorHandling(server: ActorRef): Receive = {
      case CommandFailed(_: Write) => appController ! ErrorOccurred(MESSAGE_SENDING_FAILED)
      case _: ConnectionClosed =>
        unstashAll()
        this become init
        appController ! ErrorOccurred(CONNECTION_LOST)
      case _ => stash()
    }
  }

  private[this] class TcpConnectionManagerWithTcp(address: InetSocketAddress, appController: ActorRef)
    extends TcpConnectionManager(address, appController) with ActorWithTcp
}

