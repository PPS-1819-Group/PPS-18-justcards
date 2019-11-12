package org.justcards.commons.actor_connection

import akka.io.Tcp.Received

trait ActorWithTcp extends ActorWithConnection {

  import org.justcards.commons.AppMessage._

  override def parse: Receive = {
    case Received(data) => self ! Outer(extractMessage(data))
  }
}
