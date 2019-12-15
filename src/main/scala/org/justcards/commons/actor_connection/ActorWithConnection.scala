package org.justcards.commons.actor_connection

import akka.actor.{Actor, ActorRef}
import org.justcards.commons.AppMessage

abstract class ActorWithConnection extends Actor {

  protected def parse: Receive
  protected def init: Receive

  final override def receive: Receive = parse orElse init
  final def become(behaviour: Receive, discardOld: Boolean = true): Unit =
    context become (parse orElse behaviour, discardOld)

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
