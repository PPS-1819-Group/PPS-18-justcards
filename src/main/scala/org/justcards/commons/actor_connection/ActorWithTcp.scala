package org.justcards.commons.actor_connection

import akka.actor.Actor
import akka.io.Tcp.Received

trait ActorWithTcp extends Actor {

  import org.justcards.commons.AppMessage._

  abstract override def receive: Receive = {
    case Received(data) =>
      val msgReceived = extractMessage(data)
      super.receive(Outer(msgReceived))
    case message => super.receive(message)
  }

}
