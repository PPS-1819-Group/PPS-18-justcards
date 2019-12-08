package org.justcards.commons.actor_connection

import akka.actor.{Actor, ActorRef}
import org.justcards.commons.AppMessage

abstract class ActorWithConnection extends Actor {
  def parse: Receive
  final def become(behaviour: Receive): Unit = context become (parse orElse behaviour)
  private[actor_connection] def tellWithConnection(actor: ActorRef, message: AppMessage): Unit
  implicit class MyPersonalSender(actor: ActorRef) {
    def ==>(message: AppMessage): Unit = tellWithConnection(actor, message)
  }
}

object ActorWithConnection {

  object ActorWithConnectionOptions extends Enumeration {
    type ActorWithConnectionOptions = Value
    val TCP, REMOTES = Value
  }

}
