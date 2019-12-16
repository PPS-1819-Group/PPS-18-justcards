package org.justcards.server.connection_manager

import akka.actor.{ActorRef, Props}
import org.justcards.commons.actor_connection.ActorWithConnection.ActorWithConnectionOptions._

object ServerConnectionManager {
  /**
   * Create a new ServerConnectionManager of a default type.
   * @param port the port to connect to
   * @param userManager the userManager that the users will talk to
   * @return a new ServerConnectionManager
   */
  def apply(port: Int, userManager: ActorRef): Props = ServerConnectionManager(port, userManager, REMOTES)

  /**
   * Create a new ServerConnectionManager.
   * @param port the port to connect to
   * @param userManager the userManager that the users will talk to
   * @param mode the type of connection you want to use
   * @return a new ServerConnectionManager
   */
  def apply(port: Int, userManager: ActorRef, mode: ActorWithConnectionOptions): Props = mode match {
    case TCP => ServerConnectionManagerWithTcp(port, userManager)
    case REMOTES => ServerRemoteConnectionManager(userManager)
  }

}
