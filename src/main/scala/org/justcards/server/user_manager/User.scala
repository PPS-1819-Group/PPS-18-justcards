package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import org.justcards._
import commons.AppMessage._
import org.justcards.commons.{AppMessage, ErrorOccurred, Logged}
import org.justcards.server.user_manager.UserManagerMessage.UserLogout

class User(private val userRef: ActorRef, private val userManager: ActorRef) extends Actor {

  override def receive: Receive = behaviour(notLogged)

  private def receiveMessage: Receive = {
    case Received(data) =>
      val msgReceived = extractMessage(data)
      userManager ! msgReceived
  }

  private def notLogged: Receive = {
    case Logged(username) => context become behaviour(logged(username), username)
    case msg: ErrorOccurred => userRef ==> msg
  }

  private def logged(username: String): Receive = {
    case message: AppMessage => userRef ==> message
  }

  private def errorBehaviour(username: String): Receive = {
    case CommandFailed(w: Write) =>
      // O/S buffer was full
      server.log("Write failed, trying to send => " + extractMessage(w.data))
    case _: ErrorClosed | PeerClosed =>
      server.log("Connection with user has been closed")
      if (!username.isEmpty) userManager ! UserLogout(username, self)
      context stop self
  }

  private def behaviour(currentBehaviour: Receive, username: String = ""): Receive =
    receiveMessage orElse currentBehaviour orElse errorBehaviour(username)

}

object User {
  def apply(userRef: ActorRef, userManager: ActorRef): Props = Props(classOf[User], userRef, userManager)
}