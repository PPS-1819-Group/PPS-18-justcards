package org.justcards.client.connection_manager

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.io.{IO, Tcp}
import akka.io.Tcp.{Connect, Connected, Received, Register}
import org.justcards.commons.{AppMessage, LogIn, RetrieveAvailableGames}
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}

object TcpConnectionManager {

  def apply(address: InetSocketAddress): ConnectionManager =
    (appController: ActorRef) => Props(classOf[TcpConnectionManagerWithTcp], address, appController)

  def apply(host: String, port: Int): ConnectionManager = TcpConnectionManager(new InetSocketAddress(host, port))

  abstract class TcpConnectionManager(address: InetSocketAddress, appController: ActorRef) extends ActorWithConnection with Actor with Stash {

    import context.system
    IO(Tcp) ! Connect(address)

    override def receive: Receive = parse orElse init

    private def init: Receive = {
      case _ @ Connected(_, _) =>
        val server = sender()
        server ! Register(self)
        unstashAll()
        this become ready(server)
      case _ => stash()
    }

    private def ready(server: ActorRef): Receive = {
      case m: AppMessage => server ==> m
      case Outer(m: AppMessage) => appController ! m
      case _ =>
    }
  }

  private[this] class TcpConnectionManagerWithTcp(address: InetSocketAddress, appController: ActorRef)
    extends TcpConnectionManager(address, appController) with ActorWithTcp
}

