package org.justcards.server.connection_manager

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import org.justcards.server.user_manager.User

class ServerConnectionManager(private val port: Int, private val userManager: ActorRef) extends Actor {

  import ServerConnectionManager._
  import context.system
  IO(Tcp) ! Bind(self, new InetSocketAddress(serverUrl, port))

  override def receive: Receive = {
    case CommandFailed(_: Bind) => context.stop(self)
    case _ @ Connected(_,_) =>
      val connection = sender()
      val user = context.actorOf(User(connection, userManager))
      connection ! Register(user)
  }

}

object ServerConnectionManager {
  def apply(port: Int, userManager: ActorRef): Props = Props(classOf[ServerConnectionManager], port, userManager)
  private val serverUrl = "localhost"
}
