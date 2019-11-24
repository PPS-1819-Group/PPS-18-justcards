package org.justcards.server.user_manager

import akka.actor.{ActorRef, Props}
import akka.io.Tcp._
import org.justcards.commons._
import org.justcards.commons.actor_connection.{ActorWithConnection, ActorWithTcp, Outer}
import org.justcards.server
import org.justcards.server.user_manager.UserManagerMessage.LogOutAndExitFromLobby

abstract class BasicUserActor(userRef: ActorRef, userManager: ActorRef) extends ActorWithConnection {

  import org.justcards.commons.AppError._

  override def receive: Receive = parse orElse completeBehaviour(notLogged)

  private def notLogged: Receive = {
    case Outer(msg: LogIn) => userManager ! msg
    case Outer(_: LogOut) =>
    case Logged(username) =>
      userRef ==> Logged(username)
      this become completeBehaviour(logged(username), username)
  }

  private def logged(username: String): Receive = {
    case Outer(LogOut(`username`)) =>
      userManager ! LogOut(username)
      context stop self
    case message: LobbyJoined => changeToLobbyState(message, username)
    case message: LobbyCreated => changeToLobbyState(message, username)
  }

  private def inLobby(username: String, lobby: LobbyId): Receive = {
    case Outer(LogOut(`username`)) =>
      userManager ! LogOutAndExitFromLobby(username, lobby)
      context stop self
  }

  private def commonBehaviour: Receive = {
    case Outer(_: LogIn) => userRef ==> ErrorOccurred(USER_ALREADY_LOGGED)
    case Outer(_: LogOut) => userRef ==> ErrorOccurred(USER_WRONG_USERNAME)
    case Outer(msg) => userManager ! msg
    case msg: ErrorOccurred => userRef ==> msg
    case message: AppMessage => userRef ==> message
  }

  protected def errorBehaviour(username: String): Receive = {
    case msg => server.log("User -> " + username + " | unhandled message | " + msg)
  }

  private def completeBehaviour(behaviour: Receive, username: String = ""): Receive =
    behaviour orElse commonBehaviour orElse errorBehaviour(username)

  private def changeToLobbyState[A <: {val lobby: LobbyId}](msg: A, username: String): Unit = {
    userRef ==> msg.asInstanceOf[AppMessage]
    this become completeBehaviour(
      inLobby(username, msg.lobby),
      username
    )
  }

}

object User {

  def apply(userRef: ActorRef, userManager: ActorRef): Props = Props(classOf[UserActorWithTcp], userRef, userManager)

  private[this] class UserActorWithTcp(private val userRef: ActorRef, private val userManager: ActorRef)
    extends BasicUserActor(userRef, userManager) with ActorWithTcp {

    import org.justcards.commons.actor_connection.ActorWithTcp._

    private[this] val CONNECTION_CLOSED = "Connection with user has been closed"
    private[this] val COMMAND_FAILED = "Write failed, trying to send => "

    override def errorBehaviour(username: String): Receive = {
      case CommandFailed(w: Write) =>
        // O/S buffer was full
        server.log(COMMAND_FAILED + extractMessage(w.data))
      case _: ConnectionClosed =>
        server.log(CONNECTION_CLOSED)
        if (!username.isEmpty) self ! Outer(LogOut(username))
        else context stop self
    }

  }

}