package org.justcards.server.connection_manager

import akka.actor.{ActorRef, Props}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions._

object ServerConnectionManager {

  def apply(port: Int, userManager: ActorRef): Props = ServerConnectionManager(port, userManager, REMOTES)

  def apply(port: Int, userManager: ActorRef, mode: ActorWithConnectionOptions): Props = mode match {
    case TCP => ServerConnectionManagerWithTcp(port, userManager)
    case REMOTES => ServerRemoteConnectionManager(userManager)
  }

}
