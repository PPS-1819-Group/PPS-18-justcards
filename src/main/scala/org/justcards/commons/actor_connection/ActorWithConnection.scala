package org.justcards.commons.actor_connection

import akka.actor.{Actor, ActorRef}
import org.justcards.commons.AppMessage

abstract class ActorWithConnection extends Actor {

  /**
   * Behaviour to parse external received messages.
   * @return the behaviour
   */
  protected def parse: Receive

  /**
   * Initial behaviour.
   * @return the behaviour
   */
  protected def init: Receive

  final override def receive: Receive = parse orElse init

  /**
   * Works in the same way of the actor context become, but integrating the parse behaviour.
   * @param behaviour the next behaviour
   * @param discardOld if true changes behaviour without saving the old one
   */
  final def become(behaviour: Receive, discardOld: Boolean = true): Unit =
    context become (parse orElse behaviour, discardOld)

  private[actor_connection] def tellWithConnection(actor: ActorRef, message: AppMessage): Unit
  implicit class MyPersonalSender(actor: ActorRef) {
    def ==>(message: AppMessage): Unit = tellWithConnection(actor, message)
  }
}

object ActorWithConnection {

  /**
   * Available connection technologies.
   */
  object ActorWithConnectionOptions extends Enumeration {
    type ActorWithConnectionOptions = Value
    val TCP, REMOTES = Value
  }

}
