package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import org.justcards._
import commons.AppMessage._
import org.justcards.commons.AppMessage

class User(private val userRef: ActorRef, private val userManager: ActorRef) extends Actor {

  override def receive: Receive = init orElse errorBehaviour

  def init: Receive = {
    case Received(data) =>
      val msgReceived = extractMessage(data)
      server.log(msgReceived.toString)
      userManager ! msgReceived
    case message: AppMessage => userRef ==> message
  }

  def errorBehaviour: Receive = {
    case CommandFailed(w: Write) =>
      // O/S buffer was full
      server.log("Write failed, trying to send => " + extractMessage(w.data))
    case _: ErrorClosed | PeerClosed =>
      server.log("Connection with user has been closed")
      context stop self
  }

}

object User {
  def apply(userRef: ActorRef, userManager: ActorRef): Props = Props(classOf[User], userRef, userManager)
}