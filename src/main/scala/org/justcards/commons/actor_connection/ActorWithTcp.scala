package org.justcards.commons.actor_connection

import akka.io.Tcp.Received

trait ActorWithTcp extends ActorWithConnection {

  import org.justcards.commons.AppMessage._

  override def parse: Receive = {
    case Received(data) =>
      //println("actor with tcp: received Received(" + data + ")")
      val msgReceived = extractMessage(data)
      //println("actor with tcp: msg extracted " + msgReceived)
      self ! Outer(msgReceived)
  }
}
