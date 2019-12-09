package org.justcards.server.connection_manager

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions
import org.justcards.server.user_manager.User

/**
  * TCP connection manager that works as a server
  * @param port the port to connect to
  * @param userManager the userManager that the users will talk to
  */
private[connection_manager] class ServerConnectionManagerWithTcp(private val port: Int,
                                                                 private val userManager: ActorRef) extends Actor {

  import ServerConnectionManagerWithTcp._
  import context.system
  IO(Tcp) ! Bind(self, new InetSocketAddress(SERVER_URL, port))

  override def receive: Receive = {
    case CommandFailed(_: Bind) => context.stop(self)
    case _ @ Connected(_,_) =>
      val connection = sender()
      val user = context.actorOf(User(connection, userManager, ActorWithConnectionOptions.TCP))
      connection ! Register(user)
  }

}

object ServerConnectionManagerWithTcp {
  def apply(port: Int, userManager: ActorRef): Props = Props(classOf[ServerConnectionManagerWithTcp], port, userManager)
  private val SERVER_URL = "localhost"
}
