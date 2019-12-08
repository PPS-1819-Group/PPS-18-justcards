package org.justcards.commons.actor_connection

import akka.actor.{ActorLogging, ActorRef}
import org.justcards.commons.{AppMessage, JsonSerializable}

trait ActorWithRemotes extends ActorWithConnection with ActorLogging {

  import ActorWithRemotes._

  override def parse: Receive = {
    case Remote(msg: AppMessage) =>
      log.debug("Received from outside message " + msg)
      self ! Outer(msg)
  }

  override private[actor_connection] def tellWithConnection(actor: ActorRef, message: AppMessage): Unit =
    actor ! Remote(message)

}

object ActorWithRemotes {

  case class Remote(message: AppMessage) extends JsonSerializable
  case class Connect(options: String = "") extends JsonSerializable
  case class Connected(actorRef: ActorRef) extends JsonSerializable

}
