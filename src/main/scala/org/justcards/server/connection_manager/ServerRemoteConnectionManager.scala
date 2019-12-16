package org.justcards.server.connection_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions
import org.justcards.server.user_manager.User

/**
 * Connection manager that works with remotes
 * @param userManager the userManager that the users will talk to
 */
private[connection_manager] class ServerRemoteConnectionManager(private val userManager: ActorRef) extends Actor {

  import org.justcards.commons.actor_connection.ActorWithRemotes._

  override def receive: Receive = {
    case Connect(_) =>
      val connection = sender()
      val user = context.actorOf(User(connection, userManager, ActorWithConnectionOptions.REMOTES))
      sender() ! Connected(user)
  }

}

object ServerRemoteConnectionManager {
  def apply(userManager: ActorRef): Props = Props(classOf[ServerRemoteConnectionManager], userManager)
}
