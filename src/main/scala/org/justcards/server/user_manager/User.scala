package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import org.justcards.commons.{AppMessage, ErrorOccurred, LogOut, Logged}
import org.justcards.commons.AppMessage._
import org.justcards.commons.actor_connection.ActorWithTcp
import org.justcards.commons.actor_connection.Outer
import org.justcards.server
import org.justcards.server.user_manager.UserManagerMessage.UserLogout

abstract class BasicUserActor(userRef: ActorRef, userManager: ActorRef) extends Actor {

  override def receive: Receive = behaviour(notLogged)

  private def receiveMessage: Receive = {
    case Outer(msg) => userManager ! msg
  }

  private def notLogged: Receive = {
    case Outer(_: LogOut) =>
    case Logged(username) => context become behaviour(logged(username), username)
    case msg: ErrorOccurred => userRef ==> msg
  }

  private def logged(username: String): Receive = {
    case Outer(msg: LogOut) =>
      userManager ! msg
      context stop self
    case message: AppMessage => userRef ==> message
  }

  protected def errorBehaviour(username: String): Receive

  private def behaviour(currentBehaviour: Receive, username: String = ""): Receive =
    currentBehaviour orElse receiveMessage orElse errorBehaviour(username)

}

object User {

  def apply(userRef: ActorRef, userManager: ActorRef): Props = Props(classOf[UserActorWithTcp], userRef, userManager)

  private[this] class UserActorWithTcp(private val userRef: ActorRef, private val userManager: ActorRef)
    extends BasicUserActor(userRef, userManager) with ActorWithTcp {

    override def errorBehaviour(username: String): Receive = {
      case CommandFailed(w: Write) =>
        // O/S buffer was full
        server.log("Write failed, trying to send => " + extractMessage(w.data))
      case _: ErrorClosed | PeerClosed =>
        server.log("Connection with user has been closed")
        if (!username.isEmpty) userManager ! UserLogout(username, self)
        context stop self
    }

  }

}