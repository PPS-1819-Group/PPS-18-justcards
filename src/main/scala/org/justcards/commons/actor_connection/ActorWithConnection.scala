package org.justcards.commons.actor_connection

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.justcards.commons.AppMessage

abstract class ActorWithConnection extends Actor with ActorLogging {
  def parse: Receive
  final def become(behaviour: Receive): Unit = context become (parse orElse behaviour)
  private[actor_connection] def tellWithConnection(actor: ActorRef, message: Any): Unit = {
    log.debug("sending to " + actor + " - message " + message)
    actor ! message
  }
  implicit class MyPersonalSender(actor: ActorRef) {
    def ==>(message: AppMessage): Unit = tellWithConnection(actor, message)
  }
}
